{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveAnyClass              #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE DerivingStrategies          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE NumericUnderscores          #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeApplications            #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TypeOperators               #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-unused-matches        #-}

module Morbid where

{- DAVY JONES' LOCKER
A dead-man's switch contract where you can Create Chest
and the chest can only be unlocked after a period of time
of not being postponed. You can also Add Treasure to the
chest. Anyone can redeem the treasure when the deadline
has passed.
-}

import Control.Monad (void)

import Data.ByteString.Char8 qualified as C8
import Data.Map              qualified as M
import Data.Maybe            (catMaybes)

import Ledger               qualified
import Ledger.Constraints   (mustIncludeDatum)
import Ledger.Constraints   (mustPayToTheScript)
import Ledger.Constraints   (mustSpendScriptOutput)
import Ledger.Constraints   (otherScript)
import Ledger.Constraints   (typedValidatorLookups)
import Ledger.Constraints   (unspentOutputs)
import Ledger.Contexts      (ScriptContext (..))
import Ledger.Tx            (ChainIndexTxOut (..))
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value         (Value)

import Playground.Contract
import Plutus.Contract
import PlutusTx qualified
import PlutusTx.Prelude
import Prelude  qualified as Haskell

------------------------------------------------------------ DATATYPE DECLARATIONS ------------------------------------------------------------

-- | Parameter of endpoints
data CreateChest
    = CreateChest
    { _initialDeposit :: Value
    , _lockForSlots   :: Integer
    , _createPassword :: Haskell.String
    }
    deriving Generic
    deriving anyclass
    ( FromJSON
    , ToJSON
    , ToSchema
    , ToArgument
    )
newtype AddTreasure
    = AddTreasure
    { _deposit :: Value
    }
    deriving Generic
    deriving anyclass
    ( FromJSON
    , ToJSON
    , ToSchema
    , ToArgument
    )
data DelayUnlock
    = DelayUnlock
    { _postponeForSlots :: Integer
    , _password         :: Haskell.String
    }
    deriving Generic
    deriving anyclass
    ( FromJSON
    , ToJSON
    , ToSchema
    , ToArgument
    )
type UnlockChest = ()

-- | Contract actions enum
data MorbidAction
    = ActionCreateChest
    | ActionAddTreasure
    | ActionDelayUnlock
    | ActionUnlockChest
    deriving Show
PlutusTx.unstableMakeIsData ''MorbidAction
PlutusTx.makeLift ''MorbidAction

-- | Datum parameters
data ChestDatum
    = ChestDatum
    { _chestDeadline :: Ledger.POSIXTime
    , _chestCreator  :: Ledger.PaymentPubKeyHash
    , _chestPassword :: BuiltinByteString
    }
    deriving Show
PlutusTx.unstableMakeIsData ''ChestDatum

-- | Redeemer parameters
data ChestRedeemer
    = ChestRedeemer
    { _redeemTime     :: Ledger.POSIXTime
    , _redeemPKH      :: Ledger.PaymentPubKeyHash
    , _redeemPassword :: BuiltinByteString
    , _redeemAction   :: MorbidAction
    }
    deriving Show
PlutusTx.unstableMakeIsData ''ChestRedeemer

data Morbid
instance Scripts.ValidatorTypes Morbid where
    type instance DatumType     Morbid = ChestDatum
    type instance RedeemerType  Morbid = ChestRedeemer

------------------------------------------------------------ VALIDATOR FUNCTIONS ------------------------------------------------------------

{-# INLINABLE validate #-}
validate :: ChestDatum -> ChestRedeemer -> ScriptContext ->
    Bool
validate datum redeemer context =
    case _redeemAction redeemer of
        ActionCreateChest -> True -- off-chain validation
        ActionAddTreasure -> True -- anyone can add treasures
        ActionDelayUnlock -> traceIfFalse -- just validate either owner or password
            "On-chain validation ERROR ActionDelayUnlock: You're not the chest creator or Invalid Password" $
            _chestCreator  datum == _redeemPKH      redeemer ||
            _chestPassword datum == _redeemPassword redeemer
        ActionUnlockChest -> traceIfFalse -- just validate deadline
            "On-chain validation ERROR ActionUnlockChest: Chest deadline has not been reached yet!" $
            _chestDeadline datum <= _redeemTime redeemer
        
    

typedValidator :: Scripts.TypedValidator Morbid
typedValidator = Scripts.mkTypedValidator @Morbid
    $$(PlutusTx.compile [|| validate ||])
    $$(PlutusTx.compile [||   wrap   ||])
    where wrap = Scripts.wrapValidator
                @ChestDatum
                @ChestRedeemer
            
------------------------------------------------------------ ALIAS DECLARATIONS ------------------------------------------------------------

type MorbidContract = Contract () MorbidSchema
type MorbidPromise  = Promise  () MorbidSchema

------------------------------------------------------------ UTILITY FUNCTIONS ------------------------------------------------------------

-- | Script contract address
contractAddress :: Ledger.Address
contractAddress = Scripts.validatorAddress typedValidator

-- | Script contract validator
contractValidator :: Ledger.Validator
contractValidator = Scripts.validatorScript typedValidator

-- | Convert Haskell.String to hashed BuiltinByteString
hashString :: Haskell.String ->
    BuiltinByteString
hashString = sha2_256 . toBuiltin . C8.pack

-- | Log 2 Haskell.String as logDebug | logInfo | logWarn | logError
logAs :: (AsContractError x) =>
    (Haskell.String -> MorbidContract x ()) -> Haskell.String -> Haskell.String ->
    MorbidContract x ()
logAs doLog str = doLog . (str++)

-- | Log Haskell.String as logDebug | logInfo | logWarn | logError
logStrAs :: (AsContractError x) =>
    (Haskell.String -> MorbidContract x ()) -> Haskell.String ->
    MorbidContract x ()
logStrAs doLog = logAs doLog ""

-- | Log Haskell.String and Haskell.show s as logDebug | logInfo | logWarn | logError
logStrShowAs :: (Show s, AsContractError x) =>
    (Haskell.String -> MorbidContract x ()) -> Haskell.String -> s ->
    MorbidContract x ()
logStrShowAs doLog str = (logAs doLog str) . Haskell.show

-- | Log Haskell.show s as logDebug | logInfo | logWarn | logError
logShowAs :: (Show s, AsContractError x) =>
    (Haskell.String -> MorbidContract x ()) -> s ->
    MorbidContract x ()
logShowAs doLog = logStrShowAs doLog ""

-- | Check if chest has any UTXOs
isChestAvailable :: (AsContractError x) =>
    MorbidContract x Bool
isChestAvailable = utxosAt contractAddress >>= return . Haskell.not . M.null

-- | DEPRECATED: Modify chest state
accessChest :: (AsContractError x) =>
    Ledger.PaymentPubKeyHash -> Integer -> Value -> Haskell.String ->
    MorbidContract x ()
accessChest pkh slot deposit message = do
    now <- currentTime
    logStrShowAs logInfo "Curr slot time = " now
    
    --  let you = ChestDatum $ now + Haskell.fromInteger (slot * 1_000) -- 2_592_000_000 -- 30 x 24 x 3600 x 1000ms
    let you = ChestDatum
            { _chestDeadline = now + Haskell.fromInteger (slot * 1_000)
            , _chestCreator  = pkh
            , _chestPassword = hashString message
            }
        txn = you `mustPayToTheScript` deposit
    logStrAs logInfo message
    
    logShowAs logInfo you
    void $ submitTxConstraints typedValidator txn

type WhenTrue  = MorbidContract
type WhenFalse = MorbidContract
-- | Do action based on chest existence
whenChestExists :: (AsContractError x) =>
    WhenTrue x () -> WhenFalse x () ->
    MorbidContract x ()
whenChestExists doTrue doFalse = do
    chestExists <- isChestAvailable
    if chestExists then doTrue else doFalse

-- | Extract ChestDatum from ChainIndexTxOut
extractChestDatum :: ChainIndexTxOut ->
    Maybe ChestDatum
extractChestDatum o = do
    Ledger.Datum d <- either (const Nothing) Just (_ciTxOutDatum o)
    PlutusTx.fromBuiltinData d

-- | Find ChestDatum from the UTXOs
getChestDatumFrom :: M.Map TxOutRef ChainIndexTxOut ->
    Maybe ChestDatum
getChestDatumFrom =
    listToMaybe . catMaybes . M.elems . M.map extractChestDatum

------------------------------------------------------------ ENDPOINT FUNCTIONS ------------------------------------------------------------

-- | Create Chest contract endpoint, there can only be one chest
createChest :: (AsContractError x) =>
    MorbidPromise x ()
createChest = endpoint @"1. Create Chest" $ \ params -> do
    whenChestExists
        (logStrAs logError "ERROR createChest: There is an active chest already!"){-
    otherwise-}-- >>
        (do pkh <- ownPaymentPubKeyHash
            logStrShowAs logInfo "OwnPubKeyHash is " pkh
            
            now <- currentTime
            logStrShowAs logInfo "Curr slot time = " now
            
            let initialDeposit = _initialDeposit params
                deadline       = _lockForSlots   params
                password       = _createPassword params
                you = ChestDatum
                    { _chestDeadline = now + Haskell.fromInteger (deadline * 1_000)
                    , _chestCreator  = pkh
                    , _chestPassword = hashString password
                    }
                txn = you `mustPayToTheScript` initialDeposit
            
            logStrShowAs logInfo "Creating chest for " you
            void $ submitTxConstraints typedValidator txn
        )   -- (accessChest pkh deadline initialDeposit "Creating Chest")
    

-- | Add Treasure contract endpoint, anyone can deposit treasures to the chest
addTreasure :: (AsContractError x) =>
    MorbidPromise x ()
addTreasure = endpoint @"2. Add Treasure" $ \ params -> do
    whenChestExists
        (do utxoS <- utxosAt contractAddress
            logStrShowAs logInfo "UTXOs are " utxoS
            
            case (getChestDatumFrom utxoS, [(txOutRef, scriptChainIndexTxOut) | (txOutRef, scriptChainIndexTxOut) <- M.toList utxoS]) of
                (Just you@(ChestDatum chestDeadline chestCreator chestKey), (txOutRef, scriptChainIndexTxOut):_) -> do
                    pkh <- ownPaymentPubKeyHash
                    logStrShowAs logInfo "OwnPubKeyHash is " pkh

                    let validity       =    (unspentOutputs $ txOutRef `M.singleton` scriptChainIndexTxOut
                                            ) Haskell.<>
                                            (typedValidatorLookups typedValidator
                                            ) Haskell.<>
                                            (otherScript contractValidator
                                            )
                        builinDatum    = Ledger.Datum
                                        $ PlutusTx.toBuiltinData you
                        builinRedeemer = Ledger.Redeemer
                                        $ PlutusTx.toBuiltinData
                                            ChestRedeemer
                                            { _redeemTime     = chestDeadline
                                            , _redeemPKH      = pkh
                                            , _redeemPassword = hashString ""
                                            , _redeemAction   = ActionAddTreasure
                                            }
                        txn            =    (you `mustPayToTheScript` (_ciTxOutValue scriptChainIndexTxOut + _deposit params)
                                            ) <>
                                            (mustSpendScriptOutput txOutRef builinRedeemer
                                            ) <>
                                            (mustIncludeDatum builinDatum
                                            )
                    logStrShowAs logInfo "Adding treasure for " you
                    void $ submitTxConstraintsWith @Morbid validity txn
                _ -> do
                    logStrShowAs logError "ERROR addTreasure: UTXOs are " utxoS
        ){-
    otherwise-}-- >>
        (logStrAs logError "ERROR addTreasure: There is no chest yet, please create one first!")
    

-- | Delay Unlock contract endpoint, not anyone can postpone the chest unlocking
delayUnlock :: (AsContractError x) =>
    MorbidPromise x ()
delayUnlock = endpoint @"3. Delay Unlock" $ \ params -> do
    whenChestExists
        (do utxoS <- utxosAt contractAddress
            logStrShowAs logInfo "UTXOs are " utxoS
            
            case (getChestDatumFrom utxoS, [(txOutRef, scriptChainIndexTxOut) | (txOutRef, scriptChainIndexTxOut) <- M.toList utxoS]) of
                (Just d@(ChestDatum chestDeadline chestCreator chestKey), (txOutRef, scriptChainIndexTxOut):_) -> do
                    logStrShowAs logInfo "Chest creator is " chestCreator
                    
                    pkh <- ownPaymentPubKeyHash
                    logStrShowAs logInfo "OwnPubKeyHash is " pkh
                    
                    now <- currentTime
                    logStrShowAs logInfo "Curr slot time = " now
                    
                    let deadline       =    now + Haskell.fromInteger (_postponeForSlots params * 1_000)
                        validity       =    (unspentOutputs $ txOutRef `M.singleton` scriptChainIndexTxOut
                                            ) Haskell.<>
                                            (typedValidatorLookups typedValidator
                                            ) Haskell.<>
                                            (otherScript contractValidator
                                            )
                        you            = d { _chestDeadline = deadline }
                        builinDatum    = Ledger.Datum
                                        $ PlutusTx.toBuiltinData you
                        builinRedeemer = Ledger.Redeemer
                                        $ PlutusTx.toBuiltinData
                                            ChestRedeemer
                                            { _redeemTime     = deadline
                                            , _redeemPKH      = pkh
                                            , _redeemPassword = hashString $ _password params
                                            , _redeemAction   = ActionDelayUnlock
                                            }
                        txn            =    (you `mustPayToTheScript` (_ciTxOutValue scriptChainIndexTxOut)
                                            ) <>
                                            (mustSpendScriptOutput txOutRef builinRedeemer
                                            ) <>
                                            (mustIncludeDatum builinDatum
                                            )
                    logStrShowAs logInfo "Delaying unlock for " you
                    void $ submitTxConstraintsWith @Morbid validity txn
                _ -> do
                    logStrShowAs logError "ERROR delayUnlock: UTXOs are " utxoS
        ){-
    otherwise-}-- >>
        (logStrAs logError "ERROR delayUnlock: Cannot delay unlock as there is no chest yet, please create one first!")
    

-- | Unlock Chest contract endpoint, anyone can redeem the chest contents when the deadline has passed
unlockChest :: (AsContractError x) =>
    MorbidPromise x ()
unlockChest = endpoint @"4. Unlock Chest" $ \ _ -> do
    whenChestExists
        (do utxoS <- utxosAt contractAddress
            logStrShowAs logInfo "UTXOs are " utxoS
            
            case getChestDatumFrom utxoS of
                Just (ChestDatum chestDeadline _ _) -> do
                    now <- currentTime
                    logStrShowAs logInfo "Curr slot time = " now
                    
                    pkh <- ownPaymentPubKeyHash
                    logStrShowAs logInfo "OwnPubKeyHash is " pkh
                    
                    let you = ChestRedeemer
                            { _redeemTime     = now
                            , _redeemPKH      = pkh
                            , _redeemPassword = hashString ""
                            , _redeemAction   = ActionUnlockChest
                            }
                        txn = collectFromScript utxoS you
                    
                    logStrShowAs logInfo "Unlocking chest for " you
                    void $ submitTxConstraintsSpending typedValidator utxoS txn
                _ -> do
                    logStrShowAs logError "ERROR unlockChest: UTXOs are " utxoS
        ){-
    otherwise-}-- >>
        (logStrAs logError "ERROR unlockChest: There is no chest to unlock!")
    

{-dummy :: (AsContractError x) =>
    MorbidPromise x ()
dummy = endpoint @"Dummy" $ \ _ -> do
    pkh <- ownPaymentPubKeyHash
    logStrShowAs logInfo "Dummy PKH is " pkh-}

------------------------------------------------------------ CONTRACT DEFINITIONS ------------------------------------------------------------

endpoints :: (AsContractError x) =>
    MorbidContract x ()
endpoints = selectList
            [ createChest
            , addTreasure
            , delayUnlock
            , unlockChest
            -- , dummy
            ]
type MorbidSchema = Endpoint "1. Create Chest" CreateChest
                .\/ Endpoint "2. Add Treasure" AddTreasure
                .\/ Endpoint "3. Delay Unlock" DelayUnlock
                .\/ Endpoint "4. Unlock Chest" UnlockChest
                -- .\/ Endpoint "Dummy" ()
mkSchemaDefinitions ''MorbidSchema

$(mkKnownCurrencies [])
