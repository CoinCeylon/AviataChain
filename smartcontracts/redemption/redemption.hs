-- smart-contracts/loyalty-token/LoyaltyToken.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module LoyaltyToken where

import qualified Data.Map as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

-- | Airline identifier
type AirlineId = Text

-- | User identifier  
type UserId = Text

-- | Loyalty token amount
type TokenAmount = Integer

-- | Loyalty token metadata
data LoyaltyTokenMetadata = LoyaltyTokenMetadata
    { ltmAirlineId :: AirlineId
    , ltmUserId :: UserId
    , ltmTokenType :: TokenType
    , ltmExpiryDate :: Integer -- Unix timestamp
    , ltmTransferable :: Bool
    } deriving (Show, Eq, Generic)

-- | Types of loyalty tokens
data TokenType 
    = Miles 
    | Points 
    | Credits
    deriving (Show, Eq, Generic)

-- | Token balance for a user
data TokenBalance = TokenBalance
    { tbUserId :: UserId
    , tbAirlineId :: AirlineId
    , tbAmount :: TokenAmount
    , tbMetadata :: LoyaltyTokenMetadata
    } deriving (Show, Eq, Generic)

-- | Token registry storing all token balances
type TokenRegistry = Map.Map (UserId, AirlineId) TokenBalance

-- | Initialize a new token balance
initTokenBalance :: UserId -> AirlineId -> TokenAmount -> TokenType -> Integer -> Bool -> TokenBalance
initTokenBalance userId airlineId amount tokenType expiry transferable =
    TokenBalance
        { tbUserId = userId
        , tbAirlineId = airlineId
        , tbAmount = amount
        , tbMetadata = LoyaltyTokenMetadata
            { ltmAirlineId = airlineId
            , ltmUserId = userId
            , ltmTokenType = tokenType
            , ltmExpiryDate = expiry
            , ltmTransferable = transferable
            }
        }

-- | Get token balance for a user from specific airline
getTokenBalance :: UserId -> AirlineId -> TokenRegistry -> Maybe TokenBalance
getTokenBalance userId airlineId registry = 
    Map.lookup (userId, airlineId) registry

-- | Update token balance
updateTokenBalance :: UserId -> AirlineId -> TokenAmount -> TokenRegistry -> TokenRegistry
updateTokenBalance userId airlineId newAmount registry =
    Map.adjust (\tb -> tb { tbAmount = newAmount }) (userId, airlineId) registry

-- | Add tokens to user balance
addTokens :: UserId -> AirlineId -> TokenAmount -> TokenRegistry -> TokenRegistry
addTokens userId airlineId amount registry =
    case Map.lookup (userId, airlineId) registry of
        Nothing -> registry -- User not found
        Just balance -> Map.insert (userId, airlineId) 
                                 (balance { tbAmount = tbAmount balance + amount }) 
                                 registry

-- | Subtract tokens from user balance
subtractTokens :: UserId -> AirlineId -> TokenAmount -> TokenRegistry -> Either String TokenRegistry
subtractTokens userId airlineId amount registry =
    case Map.lookup (userId, airlineId) registry of
        Nothing -> Left "User not found"
        Just balance -> 
            if tbAmount balance >= amount
            then Right $ Map.insert (userId, airlineId) 
                                   (balance { tbAmount = tbAmount balance - amount }) 
                                   registry
            else Left "Insufficient balance"

-- smart-contracts/loyalty-token/TokenPolicy.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module TokenPolicy where

import LoyaltyToken
import Data.Text (Text)

-- | Token policy configuration
data TokenPolicy = TokenPolicy
    { tpMinBalance :: TokenAmount
    , tpMaxBalance :: TokenAmount
    , tpExpiryPeriod :: Integer -- Days
    , tpTransferFee :: TokenAmount
    , tpBurnRate :: Double -- Percentage
    } deriving (Show, Eq)

-- | Default token policy
defaultTokenPolicy :: TokenPolicy
defaultTokenPolicy = TokenPolicy
    { tpMinBalance = 0
    , tpMaxBalance = 1000000
    , tpExpiryPeriod = 365
    , tpTransferFee = 10
    , tpBurnRate = 0.01
    }

-- | Validate token operation against policy
validateTokenOperation :: TokenPolicy -> TokenAmount -> TokenAmount -> Bool
validateTokenOperation policy currentBalance operationAmount =
    let newBalance = currentBalance + operationAmount
    in newBalance >= tpMinBalance policy && newBalance <= tpMaxBalance policy

-- | Calculate transfer fee
calculateTransferFee :: TokenPolicy -> TokenAmount -> TokenAmount
calculateTransferFee policy amount = 
    max (tpTransferFee policy) (round $ fromIntegral amount * tpBurnRate policy)

-- | Check if tokens are expired
isTokenExpired :: Integer -> LoyaltyTokenMetadata -> Bool
isTokenExpired currentTime metadata = 
    currentTime > ltmExpiryDate metadata

-- smart-contracts/loyalty-token/Validators.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Validators where

import LoyaltyToken
import TokenPolicy
import Data.Text (Text)

-- | Validation result
data ValidationResult = ValidationResult
    { vrValid :: Bool
    , vrError :: Maybe Text
    } deriving (Show, Eq)

-- | Successful validation
validResult :: ValidationResult
validResult = ValidationResult True Nothing

-- | Failed validation with error message
invalidResult :: Text -> ValidationResult
invalidResult err = ValidationResult False (Just err)

-- | Validate token mint operation
validateMint :: TokenPolicy -> UserId -> AirlineId -> TokenAmount -> ValidationResult
validateMint policy userId airlineId amount
    | amount <= 0 = invalidResult "Amount must be positive"
    | amount > tpMaxBalance policy = invalidResult "Amount exceeds maximum balance"
    | otherwise = validResult

-- | Validate token burn operation
validateBurn :: TokenPolicy -> TokenBalance -> TokenAmount -> ValidationResult
validateBurn policy balance amount
    | amount <= 0 = invalidResult "Amount must be positive"
    | amount > tbAmount balance = invalidResult "Insufficient balance"
    | otherwise = validResult

-- | Validate token transfer operation
validateTransfer :: TokenPolicy -> TokenBalance -> TokenAmount -> UserId -> ValidationResult
validateTransfer policy balance amount recipient
    | amount <= 0 = invalidResult "Amount must be positive"
    | amount > tbAmount balance = invalidResult "Insufficient balance"
    | not (ltmTransferable $ tbMetadata balance) = invalidResult "Tokens not transferable"
    | tbUserId balance == recipient = invalidResult "Cannot transfer to self"
    | otherwise = validResult

-- | Validate token redemption operation
validateRedemption :: TokenPolicy -> TokenBalance -> TokenAmount -> Integer -> ValidationResult
validateRedemption policy balance amount currentTime
    | amount <= 0 = invalidResult "Amount must be positive"
    | amount > tbAmount balance = invalidResult "Insufficient balance"
    | isTokenExpired currentTime (tbMetadata balance) = invalidResult "Tokens expired"
    | otherwise = validResult

-- smart-contracts/airline-registry/AirlineRegistry.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AirlineRegistry where

import qualified Data.Map as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import LoyaltyToken (AirlineId, TokenType, TokenAmount)

-- | Airline information
data AirlineInfo = AirlineInfo
    { aiAirlineId :: AirlineId
    , aiName :: Text
    , aiCountry :: Text
    , aiActive :: Bool
    , aiSupportedTokens :: [TokenType]
    , aiExchangeRates :: Map.Map TokenType Double
    } deriving (Show, Eq, Generic)

-- | Airline registry
type AirlineRegistry = Map.Map AirlineId AirlineInfo

-- | Register a new airline
registerAirline :: AirlineInfo -> AirlineRegistry -> AirlineRegistry
registerAirline airline registry = 
    Map.insert (aiAirlineId airline) airline registry

-- | Get airline information
getAirline :: AirlineId -> AirlineRegistry -> Maybe AirlineInfo
getAirline airlineId registry = 
    Map.lookup airlineId registry

-- | Update airline information
updateAirline :: AirlineId -> AirlineInfo -> AirlineRegistry -> AirlineRegistry
updateAirline airlineId airline registry = 
    Map.insert airlineId airline registry

-- | Deactivate an airline
deactivateAirline :: AirlineId -> AirlineRegistry -> AirlineRegistry
deactivateAirline airlineId registry = 
    Map.adjust (\airline -> airline { aiActive = False }) airlineId registry

-- | Get all active airlines
getActiveAirlines :: AirlineRegistry -> [AirlineInfo]
getActiveAirlines registry = 
    filter aiActive $ Map.elems registry

-- | Check if airline supports token type
supportsTokenType :: AirlineId -> TokenType -> AirlineRegistry -> Bool
supportsTokenType airlineId tokenType registry = 
    case Map.lookup airlineId registry of
        Nothing -> False
        Just airline -> tokenType `elem` aiSupportedTokens airline

-- | Get exchange rate for token type
getExchangeRate :: AirlineId -> TokenType -> AirlineRegistry -> Maybe Double
getExchangeRate airlineId tokenType registry = 
    case Map.lookup airlineId registry of
        Nothing -> Nothing
        Just airline -> Map.lookup tokenType (aiExchangeRates airline)

-- smart-contracts/airline-registry/RegistryValidators.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module RegistryValidators where

import AirlineRegistry
import Data.Text (Text)
import qualified Data.Text as T

-- | Validation result for registry operations
data RegistryValidationResult = RegistryValidationResult
    { rvrValid :: Bool
    , rvrError :: Maybe Text
    } deriving (Show, Eq)

-- | Successful validation
validRegistryResult :: RegistryValidationResult
validRegistryResult = RegistryValidationResult True Nothing

-- | Failed validation with error message
invalidRegistryResult :: Text -> RegistryValidationResult
invalidRegistryResult err = RegistryValidationResult False (Just err)

-- | Validate airline registration
validateAirlineRegistration :: AirlineInfo -> AirlineRegistry -> RegistryValidationResult
validateAirlineRegistration airline registry
    | T.null (aiAirlineId airline) = invalidRegistryResult "Airline ID cannot be empty"
    | T.null (aiName airline) = invalidRegistryResult "Airline name cannot be empty"
    | T.null (aiCountry airline) = invalidRegistryResult "Country cannot be empty"
    | null (aiSupportedTokens airline) = invalidRegistryResult "Must support at least one token type"
    | Map.member (aiAirlineId airline) registry = invalidRegistryResult "Airline already registered"
    | otherwise = validRegistryResult

-- | Validate airline update
validateAirlineUpdate :: AirlineId -> AirlineInfo -> AirlineRegistry -> RegistryValidationResult
validateAirlineUpdate airlineId airline registry
    | not (Map.member airlineId registry) = invalidRegistryResult "Airline not found"
    | aiAirlineId airline /= airlineId = invalidRegistryResult "Airline ID mismatch"
    | T.null (aiName airline) = invalidRegistryResult "Airline name cannot be empty"
    | null (aiSupportedTokens airline) = invalidRegistryResult "Must support at least one token type"
    | otherwise = validRegistryResult

-- | Validate exchange rate
validateExchangeRate :: Double -> RegistryValidationResult
validateExchangeRate rate
    | rate <= 0 = invalidRegistryResult "Exchange rate must be positive"
    | rate > 1000 = invalidRegistryResult "Exchange rate too high"
    | otherwise = validRegistryResult

-- smart-contracts/point-exchange/PointExchange.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module PointExchange where

import qualified Data.Map as Map
import GHC.Generics (Generic)
import LoyaltyToken
import AirlineRegistry

-- | Exchange transaction
data ExchangeTransaction = ExchangeTransaction
    { etId :: Text
    , etUserId :: UserId
    , etFromAirline :: AirlineId
    , etToAirline :: AirlineId
    , etFromAmount :: TokenAmount
    , etToAmount :: TokenAmount
    , etExchangeRate :: Double
    , etTimestamp :: Integer
    , etStatus :: ExchangeStatus
    } deriving (Show, Eq, Generic)

-- | Exchange status
data ExchangeStatus 
    = Pending
    | Completed
    | Failed
    | Cancelled
    deriving (Show, Eq, Generic)

-- | Exchange registry
type ExchangeRegistry = Map.Map Text ExchangeTransaction

-- | Calculate exchange amount
calculateExchange :: TokenAmount -> Double -> TokenAmount
calculateExchange amount rate = round $ fromIntegral amount * rate

-- | Create exchange transaction
createExchange :: Text -> UserId -> AirlineId -> AirlineId -> TokenAmount -> Double -> Integer -> ExchangeTransaction
createExchange txId userId fromAirline toAirline amount rate timestamp =
    ExchangeTransaction
        { etId = txId
        , etUserId = userId
        , etFromAirline = fromAirline
        , etToAirline = toAirline
        , etFromAmount = amount
        , etToAmount = calculateExchange amount rate
        , etExchangeRate = rate
        , etTimestamp = timestamp
        , etStatus = Pending
        }

-- | Process exchange transaction
processExchange :: Text -> TokenRegistry -> AirlineRegistry -> ExchangeRegistry -> Either String (TokenRegistry, ExchangeRegistry)
processExchange txId tokenRegistry airlineRegistry exchangeRegistry =
    case Map.lookup txId exchangeRegistry of
        Nothing -> Left "Transaction not found"
        Just tx -> 
            case etStatus tx of
                Pending -> executeExchange tx tokenRegistry airlineRegistry exchangeRegistry
                _ -> Left "Transaction already processed"

-- | Execute exchange transaction
executeExchange :: ExchangeTransaction -> TokenRegistry -> AirlineRegistry -> ExchangeRegistry -> Either String (TokenRegistry, ExchangeRegistry)
executeExchange tx tokenRegistry airlineRegistry exchangeRegistry = do
    -- Check if both airlines are active
    case (getAirline (etFromAirline tx) airlineRegistry, getAirline (etToAirline tx) airlineRegistry) of
        (Just fromAirline, Just toAirline) -> 
            if aiActive fromAirline && aiActive toAirline
            then do
                -- Subtract tokens from source
                newTokenRegistry <- subtractTokens (etUserId tx) (etFromAirline tx) (etFromAmount tx) tokenRegistry
                -- Add tokens to destination
                let finalTokenRegistry = addTokens (etUserId tx) (etToAirline tx) (etToAmount tx) newTokenRegistry
                let completedTx = tx { etStatus = Completed }
                let newExchangeRegistry = Map.insert (etId tx) completedTx exchangeRegistry
                Right (finalTokenRegistry, newExchangeRegistry)
            else Left "One or both airlines inactive"
        _ -> Left "Invalid airline(s)"

-- smart-contracts/point-exchange/ExchangeValidators.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module ExchangeValidators where

import PointExchange
import LoyaltyToken
import AirlineRegistry
import Data.Text (Text)
import qualified Data.Text as T

-- | Exchange validation result
data ExchangeValidationResult = ExchangeValidationResult
    { evrValid :: Bool
    , evrError :: Maybe Text
    } deriving (Show, Eq)

-- | Successful validation
validExchangeResult :: ExchangeValidationResult
validExchangeResult = ExchangeValidationResult True Nothing

-- | Failed validation with error message
invalidExchangeResult :: Text -> ExchangeValidationResult
invalidExchangeResult err = ExchangeValidationResult False (Just err)

-- | Validate exchange transaction
validateExchange :: UserId -> AirlineId -> AirlineId -> TokenAmount -> TokenRegistry -> AirlineRegistry -> ExchangeValidationResult
validateExchange userId fromAirline toAirline amount tokenRegistry airlineRegistry
    | T.null userId = invalidExchangeResult "User ID cannot be empty"
    | T.null fromAirline = invalidExchangeResult "From airline cannot be empty"
    | T.null toAirline = invalidExchangeResult "To airline cannot be empty"
    | fromAirline == toAirline = invalidExchangeResult "Cannot exchange with same airline"
    | amount <= 0 = invalidExchangeResult "Amount must be positive"
    | otherwise = validateExchangeBusinessRules userId fromAirline toAirline amount tokenRegistry airlineRegistry

-- | Validate exchange business rules
validateExchangeBusinessRules :: UserId -> AirlineId -> AirlineId -> TokenAmount -> TokenRegistry -> AirlineRegistry -> ExchangeValidationResult
validateExchangeBusinessRules userId fromAirline toAirline amount tokenRegistry airlineRegistry =
    case (getAirline fromAirline airlineRegistry, getAirline toAirline airlineRegistry) of
        (Nothing, _) -> invalidExchangeResult "From airline not found"
        (_, Nothing) -> invalidExchangeResult "To airline not found"
        (Just fromAirlineInfo, Just toAirlineInfo) ->
            if not (aiActive fromAirlineInfo) then invalidExchangeResult "From airline inactive"
            else if not (aiActive toAirlineInfo) then invalidExchangeResult "To airline inactive"
            else validateUserBalance userId fromAirline amount tokenRegistry

-- | Validate user has sufficient balance
validateUserBalance :: UserId -> AirlineId -> TokenAmount -> TokenRegistry -> ExchangeValidationResult
validateUserBalance userId airlineId amount tokenRegistry =
    case getTokenBalance userId airlineId tokenRegistry of
        Nothing -> invalidExchangeResult "User has no tokens with this airline"
        Just balance -> 
            if tbAmount balance >= amount
            then validExchangeResult
            else invalidExchangeResult "Insufficient balance"

-- | Validate exchange rate
validateExchangeRate :: Double -> ExchangeValidationResult
validateExchangeRate rate
    | rate <= 0 = invalidExchangeResult "Exchange rate must be positive"
    | rate > 100 = invalidExchangeResult "Exchange rate too high"
    | otherwise = validExchangeResult

-- smart-contracts/redemption/Redemption.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Redemption where

import qualified Data.Map as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import LoyaltyToken
import AirlineRegistry

-- | Redemption item type
data RedemptionItemType
    = FlightTicket
    | UpgradeTicket
    | Merchandise
    | VoucherCredit
    | ExtraServices
    deriving (Show, Eq, Generic)

-- | Redemption item
data RedemptionItem = RedemptionItem
    { riId :: Text
    , riName :: Text
    , riDescription :: Text
    , riType :: RedemptionItemType
    , riCost :: TokenAmount
    , riAirlineId :: AirlineId
    , riAvailable :: Bool
    , riCategory :: Text
    } deriving (Show, Eq, Generic)

-- | Redemption transaction
data RedemptionTransaction = RedemptionTransaction
    { rtId :: Text
    , rtUserId :: UserId
    , rtAirlineId :: AirlineId
    , rtItemId :: Text
    , rtTokenAmount :: TokenAmount
    , rtTimestamp :: Integer
    , rtStatus :: RedemptionStatus
    , rtConfirmationCode :: Maybe Text
    } deriving (Show, Eq, Generic)

-- | Redemption status
data RedemptionStatus
    = RedemptionPending
    | RedemptionApproved
    | RedemptionCompleted
    | RedemptionFailed
    | RedemptionCancelled
    deriving (Show, Eq, Generic)

-- | Redemption catalog
type RedemptionCatalog = Map.Map Text RedemptionItem

-- | Redemption transaction registry
type RedemptionRegistry = Map.Map Text RedemptionTransaction

-- | Add item to redemption catalog
addRedemptionItem :: RedemptionItem -> RedemptionCatalog -> RedemptionCatalog
addRedemptionItem item catalog = 
    Map.insert (riId item) item catalog

-- | Get redemption item
getRedemptionItem :: Text -> RedemptionCatalog -> Maybe RedemptionItem
getRedemptionItem itemId catalog = 
    Map.lookup itemId catalog

-- | Get items by airline
getItemsByAirline :: AirlineId -> RedemptionCatalog -> [RedemptionItem]
getItemsByAirline airlineId catalog = 
    filter (\item -> riAirlineId item == airlineId && riAvailable item) $ Map.elems catalog

-- | Get items by category
getItemsByCategory :: Text -> RedemptionCatalog -> [RedemptionItem]
getItemsByCategory category catalog = 
    filter (\item -> riCategory item == category && riAvailable item) $ Map.elems catalog

-- | Create redemption transaction
createRedemption :: Text -> UserId -> AirlineId -> Text -> TokenAmount -> Integer -> RedemptionTransaction
createRedemption txId userId airlineId itemId amount timestamp =
    RedemptionTransaction
        { rtId = txId
        , rtUserId = userId
        , rtAirlineId = airlineId
        , rtItemId = itemId
        , rtTokenAmount = amount
        , rtTimestamp = timestamp
        , rtStatus = RedemptionPending
        , rtConfirmationCode = Nothing
        }

-- | Process redemption transaction
processRedemption :: Text -> TokenRegistry -> RedemptionCatalog -> RedemptionRegistry -> Either String (TokenRegistry, RedemptionRegistry)
processRedemption txId tokenRegistry catalog redemptionRegistry =
    case Map.lookup txId redemptionRegistry of
        Nothing -> Left "Redemption transaction not found"
        Just tx -> 
            case rtStatus tx of
                RedemptionPending -> executeRedemption tx tokenRegistry catalog redemptionRegistry
                _ -> Left "Transaction already processed"

-- | Execute redemption transaction
executeRedemption :: RedemptionTransaction -> TokenRegistry -> RedemptionCatalog -> RedemptionRegistry -> Either String (TokenRegistry, RedemptionRegistry)
executeRedemption tx tokenRegistry catalog redemptionRegistry = do
    -- Check if item exists and is available
    case getRedemptionItem (rtItemId tx) catalog of
        Nothing -> Left "Redemption item not found"
        Just item -> 
            if not (riAvailable item) then Left "Item not available"
            else if riCost item /= rtTokenAmount tx then Left "Token amount mismatch"
            else do
                -- Subtract tokens from user
                newTokenRegistry <- subtractTokens (rtUserId tx) (rtAirlineId tx) (rtTokenAmount tx) tokenRegistry
                -- Update transaction status
                let completedTx = tx { rtStatus = RedemptionCompleted, rtConfirmationCode = Just "CONF123" }
                let newRedemptionRegistry = Map.insert (rtId tx) completedTx redemptionRegistry
                Right (newTokenRegistry, newRedemptionRegistry)

-- | Get user redemption history
getUserRedemptions :: UserId -> RedemptionRegistry -> [RedemptionTransaction]
getUserRedemptions userId registry = 
    filter (\tx -> rtUserId tx == userId) $ Map.elems registry

-- smart-contracts/redemption/RedemptionValidators.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module RedemptionValidators where

import Redemption
import LoyaltyToken
import AirlineRegistry
import Data.Text (Text)
import qualified Data.Text as T

-- | Redemption validation result
data RedemptionValidationResult = RedemptionValidationResult
    { rvValid :: Bool
    , rvError :: Maybe Text
    } deriving (Show, Eq)

-- | Successful validation
validRedemptionResult :: RedemptionValidationResult
validRedemptionResult = RedemptionValidationResult True Nothing

-- | Failed validation with error message
invalidRedemptionResult :: Text -> RedemptionValidationResult
invalidRedemptionResult err = RedemptionValidationResult False (Just err)

-- | Validate redemption transaction
validateRedemption :: UserId -> AirlineId -> Text -> TokenAmount -> TokenRegistry -> RedemptionCatalog -> AirlineRegistry -> RedemptionValidationResult
validateRedemption userId airlineId itemId amount tokenRegistry catalog airlineRegistry
    | T.null userId = invalidRedemptionResult "User ID cannot be empty"
    | T.null airlineId = invalidRedemptionResult "Airline ID cannot be empty"
    | T.null itemId = invalidRedemptionResult "Item ID cannot be empty"
    | amount <= 0 = invalidRedemptionResult "Amount must be positive"
    | otherwise = validateRedemptionBusinessRules userId airlineId itemId amount tokenRegistry catalog airlineRegistry

-- | Validate redemption business rules
validateRedemptionBusinessRules :: UserId -> AirlineId -> Text -> TokenAmount -> TokenRegistry -> RedemptionCatalog -> AirlineRegistry -> RedemptionValidationResult
validateRedemptionBusinessRules userId airlineId itemId amount tokenRegistry catalog airlineRegistry =
    case getAirline airlineId airlineRegistry of
        Nothing -> invalidRedemptionResult "Airline not found"
        Just airline ->
            if not (aiActive airline) then invalidRedemptionResult "Airline inactive"
            else validateRedemptionItem itemId amount catalog airlineId userId tokenRegistry

-- | Validate redemption item
validateRedemptionItem :: Text -> TokenAmount -> RedemptionCatalog -> AirlineId -> UserId -> TokenRegistry -> RedemptionValidationResult
validateRedemptionItem itemId amount catalog airlineId userId tokenRegistry =
    case getRedemptionItem itemId catalog of
        Nothing -> invalidRedemptionResult "Redemption item not found"
        Just item ->
            if not (riAvailable item) then invalidRedemptionResult "Item not available"
            else if riAirlineId item /= airlineId then invalidRedemptionResult "Item not from specified airline"
            else if riCost item /= amount then invalidRedemptionResult "Incorrect token amount"
            else validateUserRedemptionBalance userId airlineId amount tokenRegistry

-- | Validate user has sufficient balance for redemption
validateUserRedemptionBalance :: UserId -> AirlineId -> TokenAmount -> TokenRegistry -> RedemptionValidationResult
validateUserRedemptionBalance userId airlineId amount tokenRegistry =
    case getTokenBalance userId airlineId tokenRegistry of
        Nothing -> invalidRedemptionResult "User has no tokens with this airline"
        Just balance ->
            if tbAmount balance >= amount
            then validRedemptionResult
            else invalidRedemptionResult "Insufficient balance for redemption"

-- | Validate redemption item creation
validateRedemptionItemCreation :: RedemptionItem -> RedemptionValidationResult
validateRedemptionItemCreation item
    | T.null (riId item) = invalidRedemptionResult "Item ID cannot be empty"
    | T.null (riName item) = invalidRedemptionResult "Item name cannot be empty"
    | T.null (riAirlineId item) = invalidRedemptionResult "Airline ID cannot be empty"
    | riCost item <= 0 = invalidRedemptionResult "Item cost must be positive"
    | T.null (riCategory item) = invalidRedemptionResult "Category cannot be empty"
    | otherwise = validRedemptionResult