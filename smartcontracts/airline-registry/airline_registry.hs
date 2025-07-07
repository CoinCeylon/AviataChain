{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module AirlineRegistry where

import           Plutus.Contract
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts
import           PlutusTx
import           PlutusTx.Prelude
import           Data.Aeson                (FromJSON, ToJSON)
import           GHC.Generics              (Generic)
import qualified Data.Map                  as Map
import           Schema

-- | Airline Registration Data
data AirlineInfo = AirlineInfo
    { aiCode         :: !BuiltinByteString  -- IATA airline code (e.g., "AA", "UA")
    , aiName         :: !BuiltinByteString  -- Full airline name
    , aiCountry      :: !BuiltinByteString  -- Country of operation
    , aiOwner        :: !PubKeyHash         -- Airline owner's public key
    , aiRegistrar    :: !PubKeyHash         -- Registry authority
    , aiLicenseHash  :: !BuiltinByteString  -- Hash of airline license
    , aiStatus       :: !AirlineStatus      -- Registration status
    , aiRegisteredAt :: !POSIXTime          -- Registration timestamp
    , aiExpiresAt    :: !POSIXTime          -- License expiry
    , aiMinDeposit   :: !Integer            -- Minimum deposit required
    } deriving (Show, Generic)

instance ToJSON AirlineInfo
instance FromJSON AirlineInfo

PlutusTx.makeIsDataIndexed ''AirlineInfo [('AirlineInfo, 0)]
PlutusTx.makeLift ''AirlineInfo

-- | Airline Registration Status
data AirlineStatus
    = Pending      -- Application pending approval
    | Active       -- Actively registered
    | Suspended    -- Temporarily suspended
    | Revoked      -- License revoked
    | Expired      -- License expired
    deriving (Show, Generic)

instance ToJSON AirlineStatus
instance FromJSON AirlineStatus

PlutusTx.makeIsDataIndexed ''AirlineStatus 
    [('Pending, 0), ('Active, 1), ('Suspended, 2), ('Revoked, 3), ('Expired, 4)]
PlutusTx.makeLift ''AirlineStatus

-- | Registry Actions
data RegistryAction
    = RegisterAirline AirlineInfo    -- Register new airline
    | UpdateAirline AirlineInfo      -- Update airline information
    | SuspendAirline                 -- Suspend airline operations
    | ReactivateAirline              -- Reactivate suspended airline
    | RevokeAirline                  -- Revoke airline license
    | RenewLicense POSIXTime         -- Renew airline license
    | UpdateDeposit Integer          -- Update required deposit
    deriving (Show, Generic)

instance ToJSON RegistryAction
instance FromJSON RegistryAction

PlutusTx.makeIsDataIndexed ''RegistryAction 
    [('RegisterAirline, 0), ('UpdateAirline, 1), ('SuspendAirline, 2), 
     ('ReactivateAirline, 3), ('RevokeAirline, 4), ('RenewLicense, 5), ('UpdateDeposit, 6)]
PlutusTx.makeLift ''RegistryAction

-- | Registry Configuration
data RegistryConfig = RegistryConfig
    { rcRegistryAuthority :: !PubKeyHash    -- Registry authority public key
    , rcMinDeposit        :: !Integer       -- Minimum deposit in lovelace
    , rcLicensePeriod     :: !POSIXTime     -- License validity period
    , rcRegistrationFee   :: !Integer       -- Registration fee in lovelace
    } deriving (Show, Generic)

instance ToJSON RegistryConfig
instance FromJSON RegistryConfig

PlutusTx.makeIsDataIndexed ''RegistryConfig [('RegistryConfig, 0)]
PlutusTx.makeLift ''RegistryConfig

-- | Airline Registry Validator
airlineRegistryValidator :: AirlineInfo -> RegistryAction -> ScriptContext -> Bool
airlineRegistryValidator datum action ctx =
    case action of
        RegisterAirline newAirline ->
            traceIfFalse "Unauthorized registration" (txSignedBy info (aiRegistrar newAirline)) &&
            traceIfFalse "Invalid airline code" (isValidAirlineCode (aiCode newAirline)) &&
            traceIfFalse "Insufficient deposit" (hasMinimumDeposit newAirline) &&
            traceIfFalse "Airline already registered" (aiStatus datum == Pending) &&
            traceIfFalse "Invalid license period" (aiExpiresAt newAirline > aiRegisteredAt newAirline)
        
        UpdateAirline updatedAirline ->
            traceIfFalse "Unauthorized update" (txSignedBy info (aiOwner datum) || txSignedBy info (aiRegistrar datum)) &&
            traceIfFalse "Cannot change airline code" (aiCode updatedAirline == aiCode datum) &&
            traceIfFalse "Cannot change owner without authority" (aiOwner updatedAirline == aiOwner datum || txSignedBy info (aiRegistrar datum)) &&
            traceIfFalse "Invalid status transition" (validStatusTransition (aiStatus datum) (aiStatus updatedAirline))
        
        SuspendAirline ->
            traceIfFalse "Unauthorized suspension" (txSignedBy info (aiRegistrar datum)) &&
            traceIfFalse "Cannot suspend from current status" (aiStatus datum == Active) &&
            traceIfFalse "Airline already suspended" (aiStatus datum /= Suspended)
        
        ReactivateAirline ->
            traceIfFalse "Unauthorized reactivation" (txSignedBy info (aiRegistrar datum)) &&
            traceIfFalse "Cannot reactivate from current status" (aiStatus datum == Suspended) &&
            traceIfFalse "License expired" (to (aiExpiresAt datum) `contains` txInfoValidRange info)
        
        RevokeAirline ->
            traceIfFalse "Unauthorized revocation" (txSignedBy info (aiRegistrar datum)) &&
            traceIfFalse "Cannot revoke from current status" (aiStatus datum `elem` [Active, Suspended])
        
        RenewLicense newExpiry ->
            traceIfFalse "Unauthorized renewal" (txSignedBy info (aiOwner datum) || txSignedBy info (aiRegistrar datum)) &&
            traceIfFalse "Invalid renewal period" (newExpiry > aiExpiresAt datum) &&
            traceIfFalse "Renewal fee not paid" (hasRenewalFee info)
        
        UpdateDeposit newDeposit ->
            traceIfFalse "Unauthorized deposit update" (txSignedBy info (aiRegistrar datum)) &&
            traceIfFalse "Invalid deposit amount" (newDeposit >= 0)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    isValidAirlineCode :: BuiltinByteString -> Bool
    isValidAirlineCode code = 
        lengthOfByteString code == 2 || lengthOfByteString code == 3

    hasMinimumDeposit :: AirlineInfo -> Bool
    hasMinimumDeposit airline =
        let depositValue = getDepositValue info
        in depositValue >= aiMinDeposit airline

    validStatusTransition :: AirlineStatus -> AirlineStatus -> Bool
    validStatusTransition from to =
        case (from, to) of
            (Pending, Active) -> True
            (Active, Suspended) -> True
            (Suspended, Active) -> True
            (Active, Revoked) -> True
            (Suspended, Revoked) -> True
            (_, Expired) -> True
            _ -> False

    getDepositValue :: TxInfo -> Integer
    getDepositValue txInfo =
        let outputs = txInfoOutputs txInfo
            scriptOutputs = filter (isScriptOutput . txOutAddress) outputs
        in case scriptOutputs of
            [] -> 0
            (out:_) -> getLovelaceValue (txOutValue out)

    isScriptOutput :: Address -> Bool
    isScriptOutput (Address (ScriptCredential _) _) = True
    isScriptOutput _ = False

    getLovelaceValue :: Value -> Integer
    getLovelaceValue val = getLovelace $ fromValue val

    hasRenewalFee :: TxInfo -> Bool
    hasRenewalFee txInfo =
        let fee = getRenewalFee txInfo
        in fee >= 10000000 -- 10 ADA minimum renewal fee

    getRenewalFee :: TxInfo -> Integer
    getRenewalFee txInfo =
        let outputs = txInfoOutputs txInfo
            registryOutputs = filter (isRegistryOutput . txOutAddress) outputs
        in case registryOutputs of
            [] -> 0
            (out:_) -> getLovelaceValue (txOutValue out)

    isRegistryOutput :: Address -> Bool
    isRegistryOutput addr = addr == scriptAddress airlineRegistryValidatorScript

-- | Compiled Validator
airlineRegistryValidatorScript :: Validator
airlineRegistryValidatorScript = Validator $ fromCompiledCode $(PlutusTx.compile [|| airlineRegistryValidator ||])

-- | Validator Hash
airlineRegistryValidatorHash :: ValidatorHash
airlineRegistryValidatorHash = validatorHash airlineRegistryValidatorScript

-- | Script Address
airlineRegistryAddress :: Ledger.Address
airlineRegistryAddress = scriptAddress airlineRegistryValidatorScript

-- | Contract Schema
type RegistrySchema = 
    Endpoint "register-airline" RegisterParams
        .\/ Endpoint "update-airline" UpdateParams
        .\/ Endpoint "suspend-airline" SuspendParams
        .\/ Endpoint "reactivate-airline" ReactivateParams
        .\/ Endpoint "revoke-airline" RevokeParams
        .\/ Endpoint "renew-license" RenewParams
        .\/ Endpoint "get-airline-info" GetAirlineParams
        .\/ Endpoint "list-airlines" ListAirlinesParams

-- | Parameter Types
data RegisterParams = RegisterParams
    { rpAirlineCode   :: BuiltinByteString
    , rpAirlineName   :: BuiltinByteString
    , rpCountry       :: BuiltinByteString
    , rpLicenseHash   :: BuiltinByteString
    , rpDepositAmount :: Integer
    } deriving (Show, Generic, ToJSON, FromJSON)

data UpdateParams = UpdateParams
    { upAirlineCode :: BuiltinByteString
    , upUpdatedInfo :: AirlineInfo
    } deriving (Show, Generic, ToJSON, FromJSON)

data SuspendParams = SuspendParams
    { spAirlineCode :: BuiltinByteString
    , spReason      :: BuiltinByteString
    } deriving (Show, Generic, ToJSON, FromJSON)

data ReactivateParams = ReactivateParams
    { rapAirlineCode :: BuiltinByteString
    } deriving (Show, Generic, ToJSON, FromJSON)

data RevokeParams = RevokeParams
    { rvpAirlineCode :: BuiltinByteString
    , rvpReason      :: BuiltinByteString
    } deriving (Show, Generic, ToJSON, FromJSON)

data RenewParams = RenewParams
    { rnpAirlineCode :: BuiltinByteString
    , rnpNewExpiry   :: POSIXTime
    } deriving (Show, Generic, ToJSON, FromJSON)

data GetAirlineParams = GetAirlineParams
    { gapAirlineCode :: BuiltinByteString
    } deriving (Show, Generic, ToJSON, FromJSON)

data ListAirlinesParams = ListAirlinesParams
    { lapStatus :: Maybe AirlineStatus
    , lapLimit  :: Maybe Integer
    } deriving (Show, Generic, ToJSON, FromJSON)

-- | Contract Implementation
airlineRegistryContract :: Contract () RegistrySchema Text ()
airlineRegistryContract = do
    logInfo @String "Starting Airline Registry Contract"
    selectList [registerAirline, updateAirline, suspendAirline, reactivateAirline, 
                revokeAirline, renewLicense, getAirlineInfo, listAirlines] >> airlineRegistryContract
  where
    registerAirline = endpoint @"register-airline" $ \params -> do
        pkh <- ownPubKeyHash
        now <- currentTime
        let expiry = now + (365 * 24 * 60 * 60 * 1000) -- 1 year validity
        let airlineInfo = AirlineInfo
                { aiCode = rpAirlineCode params
                , aiName = rpAirlineName params
                , aiCountry = rpCountry params
                , aiOwner = pkh
                , aiRegistrar = pkh -- Should be from config
                , aiLicenseHash = rpLicenseHash params
                , aiStatus = Pending
                , aiRegisteredAt = now
                , aiExpiresAt = expiry
                , aiMinDeposit = rpDepositAmount params
                }
        let depositValue = Ada.lovelaceValueOf (rpDepositAmount params)
        let tx = Constraints.mustPayToTheScript airlineInfo depositValue
        ledgerTx <- submitTxConstraints airlineRegistryValidatorScript tx
        void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
        logInfo @String $ "Registered airline: " ++ show (rpAirlineCode params)
    
    updateAirline = endpoint @"update-airline" $ \params -> do
        utxos <- utxosAt airlineRegistryAddress
        let relevantUtxos = Map.filter (matchesAirlineCode (upAirlineCode params)) utxos
        case Map.toList relevantUtxos of
            [] -> logError @String "Airline not found"
            ((oref, o):_) -> do
                let tx = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ UpdateAirline (upUpdatedInfo params))
                        <> Constraints.mustPayToTheScript (upUpdatedInfo params) (txOutValue $ txOutTxOut o)
                ledgerTx <- submitTxConstraints airlineRegistryValidatorScript tx
                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                logInfo @String $ "Updated airline: " ++ show (upAirlineCode params)
    
    suspendAirline = endpoint @"suspend-airline" $ \params -> do
        utxos <- utxosAt airlineRegistryAddress
        let relevantUtxos = Map.filter (matchesAirlineCode (spAirlineCode params)) utxos
        case Map.toList relevantUtxos of
            [] -> logError @String "Airline not found"
            ((oref, o):_) -> do
                let tx = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData SuspendAirline)
                ledgerTx <- submitTxConstraints airlineRegistryValidatorScript tx
                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                logInfo @String $ "Suspended airline: " ++ show (spAirlineCode params)
    
    reactivateAirline = endpoint @"reactivate-airline" $ \params -> do
        utxos <- utxosAt airlineRegistryAddress
        let relevantUtxos = Map.filter (matchesAirlineCode (rapAirlineCode params)) utxos
        case Map.toList relevantUtxos of
            [] -> logError @String "Airline not found"
            ((oref, o):_) -> do
                let tx = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ReactivateAirline)
                ledgerTx <- submitTxConstraints airlineRegistryValidatorScript tx
                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                logInfo @String $ "Reactivated airline: " ++ show (rapAirlineCode params)
    
    revokeAirline = endpoint @"revoke-airline" $ \params -> do
        utxos <- utxosAt airlineRegistryAddress
        let relevantUtxos = Map.filter (matchesAirlineCode (rvpAirlineCode params)) utxos
        case Map.toList relevantUtxos of
            [] -> logError @String "Airline not found"
            ((oref, o):_) -> do
                let tx = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData RevokeAirline)
                ledgerTx <- submitTxConstraints airlineRegistryValidatorScript tx
                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                logInfo @String $ "Revoked airline: " ++ show (rvpAirlineCode params)
    
    renewLicense = endpoint @"renew-license" $ \params -> do
        utxos <- utxosAt airlineRegistryAddress
        let relevantUtxos = Map.filter (matchesAirlineCode (rnpAirlineCode params)) utxos
        case Map.toList relevantUtxos of
            [] -> logError @String "Airline not found"
            ((oref, o):_) -> do
                let renewalFee = Ada.lovelaceValueOf 10000000 -- 10 ADA
                let tx = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ RenewLicense (rnpNewExpiry params))
                        <> Constraints.mustPayToTheScript (extractDatum o) (txOutValue (txOutTxOut o) <> renewalFee)
                ledgerTx <- submitTxConstraints airlineRegistryValidatorScript tx
                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                logInfo @String $ "Renewed license for airline: " ++ show (rnpAirlineCode params)
    
    getAirlineInfo = endpoint @"get-airline-info" $ \params -> do
        utxos <- utxosAt airlineRegistryAddress
        let relevantUtxos = Map.filter (matchesAirlineCode (gapAirlineCode params)) utxos
        case Map.toList relevantUtxos of
            [] -> logError @String "Airline not found"
            ((_, o):_) -> do
                let airlineInfo = extractDatum o
                logInfo @String $ "Airline info: " ++ show airlineInfo
    
    listAirlines = endpoint @"list-airlines" $ \params -> do
        utxos <- utxosAt airlineRegistryAddress
        let airlines = map (extractDatum . snd) (Map.toList utxos)
        let filteredAirlines = case lapStatus params of
                Nothing -> airlines
                Just status -> filter (\ai -> aiStatus ai == status) airlines
        let limitedAirlines = case lapLimit params of
                Nothing -> filteredAirlines
                Just limit -> take (fromInteger limit) filteredAirlines
        logInfo @String $ "Found " ++ show (length limitedAirlines) ++ " airlines"

-- | Utility Functions
matchesAirlineCode :: BuiltinByteString -> ChainIndexTxOut -> Bool
matchesAirlineCode code txOut =
    case txOutDatum txOut of
        Nothing -> False
        Just datum -> case PlutusTx.fromBuiltinData datum of
            Just (airlineInfo :: AirlineInfo) -> aiCode airlineInfo == code
            Nothing -> False

extractDatum :: ChainIndexTxOut -> AirlineInfo
extractDatum txOut =
    case txOutDatum txOut of
        Nothing -> error "No datum found"
        Just datum -> case PlutusTx.fromBuiltinData datum of
            Just airlineInfo -> airlineInfo
            Nothing -> error "Invalid datum format"

-- | Initialize Registry with Default Configuration
initializeRegistry :: RegistryConfig -> Contract () RegistrySchema Text ()
initializeRegistry config = do
    logInfo @String "Initializing Airline Registry"
    let initialValue = Ada.lovelaceValueOf (rcMinDeposit config)
    let tx = Constraints.mustPayToTheScript config initialValue
    ledgerTx <- submitTxConstraints airlineRegistryValidatorScript tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String "Registry initialized successfully"

-- | Helper function to check if airline is active
isAirlineActive :: BuiltinByteString -> Contract () RegistrySchema Text Bool
isAirlineActive airlineCode = do
    utxos <- utxosAt airlineRegistryAddress
    let relevantUtxos = Map.filter (matchesAirlineCode airlineCode) utxos
    case Map.toList relevantUtxos of
        [] -> return False
        ((_, o):_) -> do
            let airlineInfo = extractDatum o
            now <- currentTime
            return $ aiStatus airlineInfo == Active && aiExpiresAt airlineInfo > now

-- | Get airline by code
getAirlineByCode :: BuiltinByteString -> Contract () RegistrySchema Text (Maybe AirlineInfo)
getAirlineByCode airlineCode = do
    utxos <- utxosAt airlineRegistryAddress
    let relevantUtxos = Map.filter (matchesAirlineCode airlineCode) utxos
    case Map.toList relevantUtxos of
        [] -> return Nothing
        ((_, o):_) -> return $ Just $ extractDatum o