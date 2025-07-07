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

module LoyaltyToken where

import           Plutus.Contract
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts
import           PlutusTx
import           PlutusTx.Prelude
import           Data.Aeson                (FromJSON, ToJSON)
import           GHC.Generics              (Generic)
import           Schema

-- | Loyalty Token Data Types
data LoyaltyToken = LoyaltyToken
    { ltAirline     :: !BuiltinByteString  -- Airline identifier
    , ltCustomer    :: !PubKeyHash         -- Customer public key hash
    , ltPoints      :: !Integer            -- Point balance
    , ltTier        :: !Integer            -- Loyalty tier (1-5)
    , ltExpirySlot  :: !POSIXTime          -- Expiry timestamp
    } deriving (Show, Generic)

instance ToJSON LoyaltyToken
instance FromJSON LoyaltyToken

PlutusTx.makeIsDataIndexed ''LoyaltyToken [('LoyaltyToken, 0)]
PlutusTx.makeLift ''LoyaltyToken

-- | Loyalty Token Actions
data LoyaltyAction
    = MintTokens Integer              -- Mint new loyalty tokens
    | BurnTokens Integer              -- Burn loyalty tokens
    | TransferPoints PubKeyHash Integer -- Transfer points to another customer
    | UpdateTier Integer              -- Update loyalty tier
    | ExtendExpiry POSIXTime          -- Extend token expiry
    deriving (Show, Generic)

instance ToJSON LoyaltyAction
instance FromJSON LoyaltyAction

PlutusTx.makeIsDataIndexed ''LoyaltyAction 
    [('MintTokens, 0), ('BurnTokens, 1), ('TransferPoints, 2), ('UpdateTier, 3), ('ExtendExpiry, 4)]
PlutusTx.makeLift ''LoyaltyAction

-- | Loyalty Token Validator
loyaltyTokenValidator :: LoyaltyToken -> LoyaltyAction -> ScriptContext -> Bool
loyaltyTokenValidator datum action ctx =
    case action of
        MintTokens amount ->
            traceIfFalse "Invalid mint amount" (amount > 0) &&
            traceIfFalse "Unauthorized minting" (txSignedBy info (ltCustomer datum)) &&
            traceIfFalse "Token not expired" (to (ltExpirySlot datum) `contains` txInfoValidRange info)
        
        BurnTokens amount ->
            traceIfFalse "Invalid burn amount" (amount > 0) &&
            traceIfFalse "Insufficient balance" (ltPoints datum >= amount) &&
            traceIfFalse "Unauthorized burning" (txSignedBy info (ltCustomer datum))
        
        TransferPoints recipient amount ->
            traceIfFalse "Invalid transfer amount" (amount > 0) &&
            traceIfFalse "Insufficient balance" (ltPoints datum >= amount) &&
            traceIfFalse "Unauthorized transfer" (txSignedBy info (ltCustomer datum)) &&
            traceIfFalse "Invalid recipient" (recipient /= ltCustomer datum)
        
        UpdateTier newTier ->
            traceIfFalse "Invalid tier" (newTier >= 1 && newTier <= 5) &&
            traceIfFalse "Unauthorized tier update" (txSignedBy info (ltCustomer datum))
        
        ExtendExpiry newExpiry ->
            traceIfFalse "Invalid expiry extension" (newExpiry > ltExpirySlot datum) &&
            traceIfFalse "Unauthorized expiry extension" (txSignedBy info (ltCustomer datum))
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

-- | Compiled Validator
loyaltyTokenValidatorScript :: Validator
loyaltyTokenValidatorScript = Validator $ fromCompiledCode $$(PlutusTx.compile [|| loyaltyTokenValidator ||])

-- | Validator Hash
loyaltyTokenValidatorHash :: ValidatorHash
loyaltyTokenValidatorHash = validatorHash loyaltyTokenValidatorScript

-- | Script Address
loyaltyTokenAddress :: Ledger.Address
loyaltyTokenAddress = scriptAddress loyaltyTokenValidatorScript

-- | Contract Endpoints
type LoyaltySchema = 
    Endpoint "mint-loyalty-tokens" MintParams
        .\/ Endpoint "burn-loyalty-tokens" BurnParams
        .\/ Endpoint "transfer-points" TransferParams
        .\/ Endpoint "update-tier" UpdateTierParams
        .\/ Endpoint "extend-expiry" ExtendExpiryParams

data MintParams = MintParams
    { mpAirline :: BuiltinByteString
    , mpAmount  :: Integer
    , mpTier    :: Integer
    } deriving (Show, Generic, ToJSON, FromJSON)

data BurnParams = BurnParams
    { bpAmount :: Integer
    } deriving (Show, Generic, ToJSON, FromJSON)

data TransferParams = TransferParams
    { tpRecipient :: PubKeyHash
    , tpAmount    :: Integer
    } deriving (Show, Generic, ToJSON, FromJSON)

data UpdateTierParams = UpdateTierParams
    { utpNewTier :: Integer
    } deriving (Show, Generic, ToJSON, FromJSON)

data ExtendExpiryParams = ExtendExpiryParams
    { eepNewExpiry :: POSIXTime
    } deriving (Show, Generic, ToJSON, FromJSON)

-- | Contract Implementation
loyaltyContract :: Contract () LoyaltySchema Text ()
loyaltyContract = do
    logInfo @String "Starting Loyalty Token Contract"
    selectList [mintTokens, burnTokens, transferPoints, updateTier, extendExpiry] >> loyaltyContract
  where
    mintTokens = endpoint @"mint-loyalty-tokens" $ \params -> do
        pkh <- ownPubKeyHash
        now <- currentTime
        let expiryTime = now + 31536000000 -- 1 year expiry
        let token = LoyaltyToken
                { ltAirline = mpAirline params
                , ltCustomer = pkh
                , ltPoints = mpAmount params
                , ltTier = mpTier params
                , ltExpirySlot = expiryTime
                }
        let tx = Constraints.mustPayToTheScript token (Ada.lovelaceValueOf (mpAmount params * 1000000))
        ledgerTx <- submitTxConstraints loyaltyTokenValidatorScript tx
        void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
        logInfo @String $ "Minted " ++ show (mpAmount params) ++ " loyalty tokens"
    
    burnTokens = endpoint @"burn-loyalty-tokens" $ \params -> do
        utxos <- utxosAt loyaltyTokenAddress
        case Map.toList utxos of
            [] -> logError @String "No UTxOs found"
            ((oref, o):_) -> do
                let tx = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ BurnTokens (bpAmount params))
                ledgerTx <- submitTxConstraints loyaltyTokenValidatorScript tx
                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                logInfo @String $ "Burned " ++ show (bpAmount params) ++ " loyalty tokens"
    
    transferPoints = endpoint @"transfer-points" $ \params -> do
        utxos <- utxosAt loyaltyTokenAddress
        case Map.toList utxos of
            [] -> logError @String "No UTxOs found"
            ((oref, o):_) -> do
                let tx = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ TransferPoints (tpRecipient params) (tpAmount params))
                ledgerTx <- submitTxConstraints loyaltyTokenValidatorScript tx
                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                logInfo @String $ "Transferred " ++ show (tpAmount params) ++ " points"
    
    updateTier = endpoint @"update-tier" $ \params -> do
        utxos <- utxosAt loyaltyTokenAddress
        case Map.toList utxos of
            [] -> logError @String "No UTxOs found"
            ((oref, o):_) -> do
                let tx = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ UpdateTier (utpNewTier params))
                ledgerTx <- submitTxConstraints loyaltyTokenValidatorScript tx
                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                logInfo @String $ "Updated tier to " ++ show (utpNewTier params)
    
    extendExpiry = endpoint @"extend-expiry" $ \params -> do
        utxos <- utxosAt loyaltyTokenAddress
        case Map.toList utxos of
            [] -> logError @String "No UTxOs found"
            ((oref, o):_) -> do
                let tx = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ ExtendExpiry (eepNewExpiry params))
                ledgerTx <- submitTxConstraints loyaltyTokenValidatorScript tx
                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                logInfo @String "Extended token expiry"