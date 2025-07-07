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

module TokenPolicy where

import           Plutus.Contract
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts
import           PlutusTx
import           PlutusTx.Prelude
import           Data.Aeson                (FromJSON, ToJSON)
import           GHC.Generics              (Generic)
import qualified Data.Map                  as Map

-- | Token Policy Parameters
data TokenPolicyParams = TokenPolicyParams
    { tppAirlineRegistry :: !ValidatorHash  -- Reference to airline registry
    , tppMaxSupply       :: !Integer        -- Maximum token supply per airline
    , tppMinAmount       :: !Integer        -- Minimum minting amount
    } deriving (Show, Generic)

instance ToJSON TokenPolicyParams
instance FromJSON TokenPolicyParams

PlutusTx.makeIsDataIndexed ''TokenPolicyParams [('TokenPolicyParams, 0)]
PlutusTx.makeLift ''TokenPolicyParams

-- | Token Policy Redeemer
data TokenPolicyRedeemer
    = MintLoyaltyTokens   -- Mint new loyalty tokens
    | BurnLoyaltyTokens   -- Burn existing loyalty tokens
    deriving (Show, Generic)

instance ToJSON TokenPolicyRedeemer
instance FromJSON TokenPolicyRedeemer

PlutusTx.makeIsDataIndexed ''TokenPolicyRedeemer 
    [('MintLoyaltyTokens, 0), ('BurnLoyaltyTokens, 1)]
PlutusTx.makeLift ''TokenPolicyRedeemer

-- | Token metadata structure
data TokenMetadata = TokenMetadata
    { tmAirline     :: !BuiltinByteString
    , tmTokenName   :: !BuiltinByteString
    , tmDescription :: !BuiltinByteString
    , tmImage       :: !BuiltinByteString
    , tmAttributes  :: ![TokenAttribute]
    } deriving (Show, Generic)

instance ToJSON TokenMetadata
instance FromJSON TokenMetadata

PlutusTx.makeIsDataIndexed ''TokenMetadata [('TokenMetadata, 0)]
PlutusTx.makeLift ''TokenMetadata

data TokenAttribute = TokenAttribute
    { taKey   :: !BuiltinByteString
    , taValue :: !BuiltinByteString
    } deriving (Show, Generic)

instance ToJSON TokenAttribute
instance FromJSON TokenAttribute

PlutusTx.makeIsDataIndexed ''TokenAttribute [('TokenAttribute, 0)]
PlutusTx.makeLift ''TokenAttribute

-- | Loyalty Token Minting Policy
loyaltyTokenPolicy :: TokenPolicyParams -> TokenPolicyRedeemer -> ScriptContext -> Bool
loyaltyTokenPolicy params redeemer ctx =
    case redeemer of
        MintLoyaltyTokens -> validateMinting
        BurnLoyaltyTokens -> validateBurning
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    validateMinting :: Bool
    validateMinting =
        traceIfFalse "Invalid minting amount" validMintAmount &&
        traceIfFalse "Airline not registered" airlineIsRegistered &&
        traceIfFalse "Supply limit exceeded" withinSupplyLimit &&
        traceIfFalse "Invalid token name" validTokenName &&
        traceIfFalse "Missing metadata" hasValidMetadata

    validateBurning :: Bool
    validateBurning =
        traceIfFalse "Invalid burning amount" validBurnAmount &&
        traceIfFalse "Unauthorized burning" authorizedBurning

    validMintAmount :: Bool
    validMintAmount = 
        let mintedAmount = getMintedAmount info
        in mintedAmount >= tppMinAmount params && mintedAmount > 0

    validBurnAmount :: Bool
    validBurnAmount = 
        let burnedAmount = getBurnedAmount info
        in burnedAmount > 0

    airlineIsRegistered :: Bool
    airlineIsRegistered = 
        let registryInputs = filter (isFromRegistry . txInInfoResolved) (txInfoInputs info)
        in not (null registryInputs)

    withinSupplyLimit :: Bool
    withinSupplyLimit =
        let currentSupply = getCurrentSupply info
            mintedAmount = getMintedAmount info
        in currentSupply + mintedAmount <= tppMaxSupply params

    validTokenName :: Bool
    validTokenName =
        let tokenNames = getTokenNames info
        in all isValidTokenName tokenNames

    hasValidMetadata :: Bool
    hasValidMetadata =
        let metadataEntries = getMetadataEntries info
        in not (null metadataEntries)

    authorizedBurning :: Bool
    authorizedBurning =
        let signatories = txInfoSignatories info
        in any (`elem` signatories) (getAuthorizedSigners info)

    isFromRegistry :: TxOut -> Bool
    isFromRegistry txOut =
        case txOutAddress txOut of
            Address (ScriptCredential vh) _ -> vh == tppAirlineRegistry params
            _ -> False

    getMintedAmount :: TxInfo -> Integer
    getMintedAmount txInfo =
        let minted = txInfoMint txInfo
        in case flattenValue minted of
            [] -> 0
            ((_, _, amt):_) -> amt

    getBurnedAmount :: TxInfo -> Integer
    getBurnedAmount txInfo =
        let minted = txInfoMint txInfo
        in case flattenValue minted of
            [] -> 0
            ((_, _, amt):_) -> negate amt

    getCurrentSupply :: TxInfo -> Integer
    getCurrentSupply _ = 0 -- Simplified - in real implementation, calculate from UTxOs

    getTokenNames :: TxInfo -> [TokenName]
    getTokenNames txInfo =
        let minted = txInfoMint txInfo
        in case flattenValue minted of
            [] -> []
            vals -> map (\(_, tn, _) -> tn) vals

    isValidTokenName :: TokenName -> Bool
    isValidTokenName (TokenName tn) = 
        lengthOfByteString tn >= 1 && lengthOfByteString tn <= 32

    getMetadataEntries :: TxInfo -> [TxMetadataValue]
    getMetadataEntries txInfo =
        case Map.lookup 721 (txInfoMetadata txInfo) of
            Just metadata -> [metadata]
            Nothing -> []

    getAuthorizedSigners :: TxInfo -> [PubKeyHash]
    getAuthorizedSigners _ = [] -- Simplified - get from airline registry

-- | Parameterized Minting Policy
mkLoyaltyTokenPolicy :: TokenPolicyParams -> MintingPolicy
mkLoyaltyTokenPolicy params = 
    MintingPolicy $ fromCompiledCode $$(PlutusTx.compile [|| \p -> loyaltyTokenPolicy p ||])
    `PlutusTx.applyCode` PlutusTx.liftCode params

-- | Policy ID calculation
loyaltyTokenPolicyId :: TokenPolicyParams -> MintingPolicyHash
loyaltyTokenPolicyId = mintingPolicyHash . mkLoyaltyTokenPolicy

-- | Create token name from airline code
mkTokenName :: BuiltinByteString -> TokenName
mkTokenName airlineCode = TokenName $ airlineCode <> "_LOYALTY"

-- | Create token metadata
mkTokenMetadata :: BuiltinByteString -> BuiltinByteString -> TokenMetadata
mkTokenMetadata airlineCode airlineName = TokenMetadata
    { tmAirline = airlineCode
    , tmTokenName = airlineCode <> "_LOYALTY"
    , tmDescription = "Loyalty points for " <> airlineName
    , tmImage = "https://loyalty.airline.com/images/" <> airlineCode <> ".png"
    , tmAttributes = 
        [ TokenAttribute "type" "loyalty_points"
        , TokenAttribute "airline" airlineCode
        , TokenAttribute "transferable" "true"
        , TokenAttribute "expirable" "true"
        ]
    }

-- | Utility functions for token operations
class TokenOperations a where
    mintTokens :: a -> Integer -> Contract w s Text ()
    burnTokens :: a -> Integer -> Contract w s Text ()
    transferTokens :: a -> PubKeyHash -> Integer -> Contract w s Text ()

-- | Default token operations implementation
instance TokenOperations TokenPolicyParams where
    mintTokens params amount = do
        pkh <- ownPubKeyHash
        let tokenName = mkTokenName "AIRLINE" -- Should be dynamic
        let policy = mkLoyaltyTokenPolicy params
        let val = Value.singleton (mintingPolicyHash policy) tokenName amount
        let tx = Constraints.mustMintValue val
        ledgerTx <- submitTxConstraints policy tx
        void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
        logInfo @String $ "Minted " ++ show amount ++ " loyalty tokens"

    burnTokens params amount = do
        let tokenName = mkTokenName "AIRLINE" -- Should be dynamic
        let policy = mkLoyaltyTokenPolicy params
        let val = Value.singleton (mintingPolicyHash policy) tokenName (negate amount)
        let tx = Constraints.mustMintValue val
        ledgerTx <- submitTxConstraints policy tx
        void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
        logInfo @String $ "Burned " ++ show amount ++ " loyalty tokens"

    transferTokens params recipient amount = do
        let tokenName = mkTokenName "AIRLINE" -- Should be dynamic
        let policy = mkLoyaltyTokenPolicy params
        let val = Value.singleton (mintingPolicyHash policy) tokenName amount
        let tx = Constraints.mustPayToPubKey recipient val
        ledgerTx <- submitTxConstraints policy tx
        void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
        logInfo @String $ "Transferred " ++ show amount ++ " loyalty tokens"

-- | Helper function to create standard token policy parameters
standardTokenPolicyParams :: ValidatorHash -> TokenPolicyParams
standardTokenPolicyParams registryHash = TokenPolicyParams
    { tppAirlineRegistry = registryHash
    , tppMaxSupply = 1000000000 -- 1 billion tokens max
    , tppMinAmount = 1000       -- Minimum 1000 tokens per mint
    }