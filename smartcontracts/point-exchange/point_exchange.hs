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

module PointExchange where

import           Plutus.Contract
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts
import           PlutusTx
import           PlutusTx.Prelude
import           Data.Aeson                (FromJSON, ToJSON)
import           GHC.Generics              (Generic)
import qualified Data.Map                  as Map
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Value              as Value
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Address            (Address)
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts

-- | Exchange Order Data
data ExchangeOrder = ExchangeOrder
    { eoOrderId         :: !BuiltinByteString   -- Unique order identifier
    , eoFromAirline     :: !BuiltinByteString   -- Source airline code
    , eoToAirline       :: !BuiltinByteString   -- Target airline code
    , eoFromAmount      :: !Integer             -- Amount to exchange from
    , eoToAmount        :: !Integer             -- Amount to receive
    , eoExchangeRate    :: !Rational            -- Exchange rate
    , eoCustomer        :: !PubKeyHash          -- Customer public key
    , eoCreatedAt       :: !POSIXTime           -- Order creation time
    , eoExpiresAt       :: !POSIXTime           -- Order expiry time
    , eoStatus          :: !OrderStatus         -- Order status
    , eoFees            :: !ExchangeFees        -- Exchange fees
    } deriving (Show, Generic)

instance ToJSON ExchangeOrder
instance FromJSON ExchangeOrder

PlutusTx.makeIsDataIndexed ''ExchangeOrder [('ExchangeOrder, 0)]
PlutusTx.makeLift ''ExchangeOrder

-- | Order Status
data OrderStatus
    = Pending       -- Order created, waiting for execution
    | Executing     -- Order being processed
    | Completed     -- Order successfully completed
    | Cancelled     -- Order cancelled by user
    | Expired       -- Order expired
    | Failed        -- Order failed due to insufficient funds/other issues
    deriving (Show, Generic)

instance ToJSON OrderStatus
instance FromJSON OrderStatus

PlutusTx.makeIsDataIndexed ''OrderStatus 
    [('Pending, 0), ('Executing, 1), ('Completed, 2), ('Cancelled, 3), ('Expired, 4), ('Failed, 5)]
PlutusTx.makeLift ''OrderStatus

-- | Exchange Fees Structure
data ExchangeFees = ExchangeFees
    { efPlatformFee   :: !Integer    -- Platform fee in lovelace
    , efExchangeFee   :: !Integer    -- Exchange fee in points
    , efGasFee        :: !Integer    -- Transaction gas fee
    , efAirlineFee    :: !Integer    -- Airline-specific fee
    } deriving (Show, Generic)

instance ToJSON ExchangeFees
instance FromJSON ExchangeFees

PlutusTx.makeIsDataIndexed ''ExchangeFees [('ExchangeFees, 0)]
PlutusTx.makeLift ''ExchangeFees

-- | Exchange Actions
data ExchangeAction
    = CreateOrder ExchangeOrder      -- Create new exchange order
    | ExecuteOrder                   -- Execute pending order
    | CancelOrder                    -- Cancel pending order
    | UpdateRate Rational            -- Update exchange rate
    | ClaimRefund                    -- Claim refund for failed order
    | UpdateFees ExchangeFees        -- Update exchange fees
    deriving (Show, Generic)

instance ToJSON ExchangeAction
instance FromJSON ExchangeAction

PlutusTx.makeIsDataIndexed ''ExchangeAction 
    [('CreateOrder, 0), ('ExecuteOrder, 1), ('CancelOrder, 2), 
     ('UpdateRate, 3), ('ClaimRefund, 4), ('UpdateFees, 5)]
PlutusTx.makeLift ''ExchangeAction

-- | Exchange Pool for specific airline pair
data ExchangePool = ExchangePool
    { epFromAirline    :: !BuiltinByteString   -- Source airline
    , epToAirline      :: !BuiltinByteString   -- Target airline
    , epFromReserves   :: !Integer             -- Source points in pool
    , epToReserves     :: !Integer             -- Target points in pool
    , epExchangeRate   :: !Rational            -- Current exchange rate
    , epTotalVolume    :: !Integer             -- Total exchange volume
    , epLastUpdate     :: !POSIXTime           -- Last rate update
    , epPoolOwner      :: !PubKeyHash          -- Pool owner/manager
    , epMinExchange    :: !Integer             -- Minimum exchange amount
    , epMaxExchange    :: !Integer             -- Maximum exchange amount
    } deriving (Show, Generic)

instance ToJSON ExchangePool
instance FromJSON ExchangePool

PlutusTx.makeIsDataIndexed ''ExchangePool [('ExchangePool, 0)]
PlutusTx.makeLift ''ExchangePool

-- | Point Exchange Validator
pointExchangeValidator :: ExchangeOrder -> ExchangeAction -> ScriptContext -> Bool
pointExchangeValidator datum action ctx =
    case action of
        CreateOrder newOrder ->
            traceIfFalse "Invalid order parameters" (validOrderParams newOrder) &&
            traceIfFalse "Unauthorized order creation" (txSignedBy info (eoCustomer newOrder)) &&
            traceIfFalse "Insufficient source points" (hasSourcePoints newOrder) &&
            traceIfFalse "Invalid exchange rate" (validExchangeRate newOrder) &&
            traceIfFalse "Order already exists" (eoStatus datum == Pending)
        
        ExecuteOrder ->
            traceIfFalse "Order not pending" (eoStatus datum == Pending) &&
            traceIfFalse "Order expired" (not $ orderExpired datum) &&
            traceIfFalse "Exchange pool insufficient" (poolHasLiquidity datum) &&
            traceIfFalse "Exchange rate changed" (rateStillValid datum) &&
            traceIfFalse "Fee payment missing" (feesPaid datum)
        
        CancelOrder ->
            traceIfFalse "Unauthorized cancellation" (txSignedBy info (eoCustomer datum)) &&
            traceIfFalse "Order not cancellable" (eoStatus datum `elem` [Pending, Failed]) &&
            traceIfFalse "Refund not processed" (refundProcessed datum)
        
        UpdateRate newRate ->
            traceIfFalse "Unauthorized rate update" (authorizedRateUpdate info) &&
            traceIfFalse "Invalid rate" (newRate > 0) &&
            traceIfFalse "Rate change too frequent" (rateUpdateAllowed datum)
        
        ClaimRefund ->
            traceIfFalse "Unauthorized refund claim" (txSignedBy info (eoCustomer datum)) &&
            traceIfFalse "Order not refundable" (eoStatus datum `elem` [Failed, Expired]) &&
            traceIfFalse "Refund already claimed" (not $ refundClaimed datum)
        
        UpdateFees newFees ->
            traceIfFalse "Unauthorized fee update" (authorizedFeeUpdate info) &&
            traceIfFalse "Invalid fee structure" (validFeeStructure newFees)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    validOrderParams :: ExchangeOrder -> Bool
    validOrderParams order =
        eoFromAmount order > 0 &&
        eoToAmount order > 0 &&
        eoFromAirline order /= eoToAirline order &&
        eoExpiresAt order > eoCreatedAt order

    hasSourcePoints :: ExchangeOrder -> Bool
    hasSourcePoints order =
        let sourceTokens = getSourceTokenValue info (eoFromAirline order)
        in sourceTokens >= eoFromAmount order

    validExchangeRate :: ExchangeOrder -> Bool
    validExchangeRate order =
        let calculatedRate = toRational (eoToAmount order) / toRational (eoFromAmount order)
        in abs (calculatedRate - eoExchangeRate order) < 0.001 -- Allow 0.1% tolerance

    orderExpired :: ExchangeOrder -> Bool
    orderExpired order =
        let currentTime = getCurrentTime info
        in currentTime > eoExpiresAt order

    poolHasLiquidity :: ExchangeOrder -> Bool
    poolHasLiquidity order =
        let poolLiquidity = getPoolLiquidity info (eoToAirline order)
        in poolLiquidity >= eoToAmount order

    rateStillValid :: ExchangeOrder -> Bool
    rateStillValid order =
        let currentRate = getCurrentRate info (eoFromAirline order) (eoToAirline order)
        in abs (currentRate - eoExchangeRate order) < 0.05 -- Allow 5% deviation

    feesPaid :: ExchangeOrder -> Bool
    feesPaid order =
        let paidFees = getTotalFeesPaid info
            requiredFees = calculateTotalFees (eoFees order)
        in paidFees >= requiredFees

    refundProcessed :: ExchangeOrder -> Bool
    refundProcessed order =
        let refundOutputs = getRefundOutputs info (eoCustomer order)
            expectedRefund = eoFromAmount order
        in refundOutputs >= expectedRefund

    authorizedRateUpdate :: TxInfo -> Bool
    authorizedRateUpdate txInfo =
        let authorizedUpdaters = getAuthorizedUpdaters txInfo
        in any (`txSignedBy` txInfo) authorizedUpdaters

    rateUpdateAllowed :: ExchangeOrder -> Bool
    rateUpdateAllowed order =
        let currentTime = getCurrentTime info
            lastUpdate = eoCreatedAt order -- Simplified
        in currentTime > lastUpdate + 3600000 -- 1 hour minimum between updates

    refundClaimed :: ExchangeOrder -> Bool
    refundClaimed _ = False -- Simplified - check if refund was already processed

    authorizedFeeUpdate :: TxInfo -> Bool
    authorizedFeeUpdate txInfo =
        let feeUpdaters = getFeeUpdaters txInfo
        in any (`txSignedBy` txInfo) feeUpdaters

    validFeeStructure :: ExchangeFees -> Bool
    validFeeStructure fees =
        efPlatformFee fees >= 0 &&
        efExchangeFee fees >= 0 &&
        efGasFee fees >= 0 &&
        efAirlineFee fees >= 0

    -- Helper functions
    getSourceTokenValue :: TxInfo -> BuiltinByteString -> Integer
    getSourceTokenValue txInfo airline = 
        let inputs = txInfoInputs txInfo
        in foldr (+) 0 $ map (getAirlineTokenAmount airline . txInInfoResolved) inputs

    getAirlineTokenAmount :: BuiltinByteString -> TxOut -> Integer
    getAirlineTokenAmount airline txOut =
        let val = txOutValue txOut
        in case findAirlineToken airline val of
            Nothing -> 0
            Just amount -> amount

    findAirlineToken :: BuiltinByteString -> Value -> Maybe Integer
    findAirlineToken airline val =
        let tokens = flattenValue val
        in case filter (\(_, tn, _) -> isAirlineToken airline tn) tokens of
            [] -> Nothing
            ((_, _, amt):_) -> Just amt

    isAirlineToken :: BuiltinByteString -> TokenName -> Bool
    isAirlineToken airline (TokenName tn) = 
        airline `isPrefixOf` tn

    getCurrentTime :: TxInfo -> POSIXTime
    getCurrentTime txInfo = 
        case txInfoValidRange txInfo of
            Interval (LowerBound (Finite time) _) _ -> time
            _ -> 0

    getPoolLiquidity :: TxInfo -> BuiltinByteString -> Integer
    getPoolLiquidity _ _ = 1000000 -- Simplified - get from exchange pool

    getCurrentRate :: TxInfo -> BuiltinByteString -> BuiltinByteString -> Rational
    getCurrentRate _ _ _ = 1.0 -- Simplified - get from oracle/pool

    getTotalFeesPaid :: TxInfo -> Integer
    getTotalFeesPaid txInfo =
        let outputs = txInfoOutputs txInfo
            feeOutputs = filter isFeeOutput outputs
        in foldr (+) 0 $ map (getLovelaceAmount . txOutValue) feeOutputs

    isFeeOutput :: TxOut -> Bool
    isFeeOutput txOut = 
        case txOutAddress txOut of
            Address (PubKeyCredential _) _ -> True -- Simplified
            _ -> False

    getLovelaceAmount :: Value -> Integer
    getLovelaceAmount val = getLovelace $ fromValue val

    calculateTotalFees :: ExchangeFees -> Integer
    calculateTotalFees fees =
        efPlatformFee fees + efGasFee fees + efAirlineFee fees

    getRefundOutputs :: TxInfo -> PubKeyHash -> Integer
    getRefundOutputs txInfo customer =
        let outputs = txInfoOutputs txInfo
            customerOutputs = filter (isCustomerOutput customer) outputs
        in foldr (+) 0 $ map (getLovelaceAmount . txOutValue) customerOutputs

    isCustomerOutput :: PubKeyHash -> TxOut -> Bool
    isCustomerOutput customer txOut =
        case txOutAddress txOut of
            Address (PubKeyCredential pkh) _ -> pkh == customer
            _ -> False

    getAuthorizedUpdaters :: TxInfo -> [PubKeyHash]
    getAuthorizedUpdaters _ = [] -- Get from registry

    getFeeUpdaters :: TxInfo -> [PubKeyHash]
    getFeeUpdaters _ = [] -- Get from registry

-- | Typed Script Instance
data PointExchange
instance Scripts.ValidatorTypes PointExchange where
    type instance DatumType PointExchange = ExchangeOrder
    type instance RedeemerType PointExchange = ExchangeAction

-- | Typed Validator
typedValidator :: Scripts.TypedValidator PointExchange
typedValidator = Scripts.mkTypedValidator @PointExchange
    $$(PlutusTx.compile [|| pointExchangeValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @ExchangeOrder @ExchangeAction

-- | Compiled Validator
validator :: Validator
validator = Scripts.validatorScript typedValidator

-- | Validator Hash
validatorHash :: ValidatorHash
validatorHash = Scripts.validatorHash typedValidator

-- | Script Address
scriptAddress :: Address
scriptAddress = Scripts.validatorAddress typedValidator

-- | Contract Schema
type ExchangeSchema = 
    Endpoint "create-exchange-order" CreateOrderParams
        .\/ Endpoint "execute-order" ExecuteOrderParams
        .\/ Endpoint "cancel-order" CancelOrderParams
        .\/ Endpoint "update-exchange-rate" UpdateRateParams
        .\/ Endpoint "claim-refund" ClaimRefundParams
        .\/ Endpoint "get-exchange-rate" GetRateParams
        .\/ Endpoint "get-pool-info" GetPoolParams

-- | Parameter Types
data CreateOrderParams = CreateOrderParams
    { copFromAirline :: BuiltinByteString
    , copToAirline   :: BuiltinByteString
    , copFromAmount  :: Integer
    , copToAmount    :: Integer
    } deriving (Show, Generic, ToJSON, FromJSON)

data ExecuteOrderParams = ExecuteOrderParams
    { eopOrderId :: BuiltinByteString
    } deriving (Show, Generic, ToJSON, FromJSON)

data CancelOrderParams = CancelOrderParams
    { caopOrderId :: BuiltinByteString
    } deriving (Show, Generic, ToJSON, FromJSON)

data UpdateRateParams = UpdateRateParams
    { urpFromAirline :: BuiltinByteString
    , urpToAirline   :: BuiltinByteString
    , urpNewRate     :: Rational
    } deriving (Show, Generic, ToJSON, FromJSON)

data ClaimRefundParams = ClaimRefundParams
    { crpOrderId :: BuiltinByteString
    } deriving (Show, Generic, ToJSON, FromJSON)

data GetRateParams = GetRateParams
    { grpFromAirline :: BuiltinByteString
    , grpToAirline   :: BuiltinByteString
    } deriving (Show, Generic, ToJSON, FromJSON)

data GetPoolParams = GetPoolParams
    { gppFromAirline :: BuiltinByteString
    , gppToAirline   :: BuiltinByteString
    } deriving (Show, Generic, ToJSON, FromJSON)

-- | Helper Functions
generateOrderId :: POSIXTime -> PubKeyHash -> Contract w s Text BuiltinByteString
generateOrderId time customer = do
    let timeBytes = encodeUtf8 $ show time
    let customerBytes = getPubKeyHash customer
    return $ sha2_256 (timeBytes <> customerBytes)

getCurrencySymbol :: BuiltinByteString -> CurrencySymbol
getCurrencySymbol bs = CurrencySymbol bs

getTokenName :: BuiltinByteString -> TokenName
getTokenName bs = TokenName bs

-- | Contract Implementation
pointExchangeContract :: Contract () ExchangeSchema Text ()
pointExchangeContract = do
    logInfo @String "Starting Point Exchange Contract"
    selectList [createOrder, executeOrder, cancelOrder, updateRate, claimRefund, getRate, getPoolInfo] >> pointExchangeContract
  where
    createOrder = endpoint @"create-exchange-order" $ \params -> do
        pkh <- ownPubKeyHash
        now <- currentTime
        orderId <- generateOrderId now pkh
        let expiry = now + 86400000 -- 24 hours expiry
        let fees = ExchangeFees 1000000 100 500000 50 -- Default fees
        let order = ExchangeOrder
                { eoOrderId = orderId
                , eoFromAirline = copFromAirline params
                , eoToAirline = copToAirline params
                , eoFromAmount = copFromAmount params
                , eoToAmount = copToAmount params
                , eoExchangeRate = toRational (copToAmount params) / toRational (copFromAmount params)
                , eoCustomer = pkh
                , eoCreatedAt = now
                , eoExpiresAt = expiry
                , eoStatus = Pending
                , eoFees = fees
                }
        let sourceTokenValue = Value.singleton (getCurrencySymbol $ copFromAirline params) 
                                              (getTokenName $ copFromAirline params) 
                                              (copFromAmount params)
        let tx = Constraints.mustPayToTheScript order sourceTokenValue
        ledgerTx <- submitTxConstraints typedValidator tx
        void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
        logInfo @String $ "Created exchange order: " ++ show orderId

    executeOrder = endpoint @"execute-order" $ \params -> do
        utxos <- utxosAt scriptAddress
        case Map.toList utxos of
            [] -> logError @String "No UTxOs found at script address"
            ((oref, ciTxOut):_) -> do
                let datum = case txOutDatum ciTxOut of
                        OutputDatumHash _ -> Nothing
                        OutputDatum (Datum d) -> PlutusTx.fromBuiltinData d
                        NoOutputDatum -> Nothing
                case datum of
                    Just order | eoOrderId order == eopOrderId params -> do
                        let redeemer = ExecuteOrder
                        let targetTokenValue = Value.singleton (getCurrencySymbol $ eoToAirline order) 
                                                              (getTokenName $ eoToAirline order) 
                                                              (eoToAmount order)
                        customerPkh <- ownPubKeyHash
                        let tx = Constraints.mustSpendScriptOutput oref redeemer <>
                                 Constraints.mustPayToPubKey customerPkh targetTokenValue
                        ledgerTx <- submitTxConstraints typedValidator tx
                        void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                        logInfo @String $ "Executed order: " ++ show (eopOrderId params)
                    _ -> logError @String "Order not found or invalid"

    cancelOrder = endpoint @"cancel-order" $ \params -> do
        utxos <- utxosAt scriptAddress
        case Map.toList utxos of
            [] -> logError @String "No UTxOs found at script address"
            ((oref, ciTxOut):_) -> do
                let datum = case txOutDatum ciTxOut of
                        OutputDatumHash _ -> Nothing
                        OutputDatum (Datum d) -> PlutusTx.fromBuiltinData d
                        NoOutputDatum -> Nothing
                case datum of
                    Just order | eoOrderId order == caopOrderId params -> do
                        let redeemer = CancelOrder
                        let refundValue = Value.singleton (getCurrencySymbol $ eoFromAirline order) 
                                                         (getTokenName $ eoFromAirline order) 
                                                         (eoFromAmount order)
                        customerPkh <- ownPubKeyHash
                        let tx = Constraints.mustSpendScriptOutput oref redeemer <>
                                 Constraints.mustPayToPubKey customerPkh refundValue
                        ledgerTx <- submitTxConstraints typedValidator tx
                        void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                        logInfo @String $ "Cancelled order: " ++ show (caopOrderId params)
                    _ -> logError @String "Order not found or not cancellable"

    updateRate = endpoint @"update-exchange-rate" $ \params -> do
        -- This would typically be restricted to authorized rate updaters
        logInfo @String $ "Updated exchange rate for " ++ show (urpFromAirline params) ++ 
                         " to " ++ show (urpToAirline params) ++ 
                         " at rate " ++ show (urpNewRate params)

    claimRefund = endpoint @"claim-refund" $ \params -> do
        utxos <- utxosAt scriptAddress
        case Map.toList utxos of
            [] -> logError @String "No UTxOs found at script address"
            ((oref, ciTxOut):_) -> do
                let datum = case txOutDatum ciTxOut of
                        OutputDatumHash _ -> Nothing
                        OutputDatum (Datum d) -> PlutusTx.fromBuiltinData d
                        NoOutputDatum -> Nothing
                case datum of
                    Just order | eoOrderId order == crpOrderId params -> do
                        let redeemer = ClaimRefund
                        let refundValue = Value.singleton (getCurrencySymbol $ eoFromAirline order) 
                                                         (getTokenName $ eoFromAirline order) 
                                                         (eoFromAmount order)
                        customerPkh <- ownPubKeyHash
                        let tx = Constraints.mustSpendScriptOutput oref redeemer <>
                                 Constraints.mustPayToPubKey customerPkh refundValue
                        ledgerTx <- submitTxConstraints typedValidator tx
                        void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                        logInfo @String $ "Claimed refund for order: " ++ show (crpOrderId params)
                    _ -> logError @String "Order not found or not refundable"

    getRate = endpoint @"get-exchange-rate" $ \params -> do
        -- This would query the current exchange rate from oracle or pool
        let currentRate = 1.0 :: Rational -- Simplified
        logInfo @String $ "Current rate from " ++ show (grpFromAirline params) ++ 
                         " to " ++ show (grpToAirline params) ++ 
                         " is " ++ show currentRate

    getPoolInfo = endpoint @"get-pool-info" $ \params -> do
        -- This would query pool information
        let poolInfo = ExchangePool
                { epFromAirline = gppFromAirline params
                , epToAirline = gppToAirline params
                , epFromReserves = 1000000
                , epToReserves = 1000000
                , epExchangeRate = 1.0
                , epTotalVolume = 5000000
                , epLastUpdate = 0
                , epPoolOwner = "pool_owner_pkh"
                , epMinExchange = 100
                , epMaxExchange = 100000
                }
        logInfo @String $ "Pool info: " ++ show poolInfo

-- | Utility Functions for Off-Chain Code
findOrderUtxo :: BuiltinByteString -> Map TxOutRef ChainIndexTxOut -> Maybe (TxOutRef, ChainIndexTxOut)
findOrderUtxo orderId utxos =
    let matchingUtxos = filter (\(_, ciTxOut) -> 
            case txOutDatum ciTxOut of
                OutputDatum (Datum d) -> 
                    case PlutusTx.fromBuiltinData d of
                        Just order -> eoOrderId order == orderId
                        Nothing -> False
                _ -> False
        ) $ Map.toList utxos
    in case matchingUtxos of
        (utxo:_) -> Just utxo
        [] -> Nothing

validateOrderParams :: CreateOrderParams -> Either String ()
validateOrderParams params
    | copFromAmount params <= 0 = Left "From amount must be positive"
    | copToAmount params <= 0 = Left "To amount must be positive"
    | copFromAirline params == copToAirline params = Left "Cannot exchange same airline points"
    | otherwise = Right ()

calculateExchangeRate :: Integer -> Integer -> Rational
calculateExchangeRate fromAmount toAmount = toRational toAmount / toRational fromAmount

-- | Export functions for use in other modules
exports :: [String]
exports = 
    [ "pointExchangeContract"
    , "validator"
    , "validatorHash"
    , "scriptAddress"
    , "ExchangeSchema"
    , "CreateOrderParams"
    , "ExecuteOrderParams"
    , "CancelOrderParams"
    , "UpdateRateParams"
    , "ClaimRefundParams"
    , "GetRateParams"
    , "GetPoolParams"
    ]