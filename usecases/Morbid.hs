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

import Data.Map   qualified as M
import Data.Maybe (catMaybes)

import Ledger               qualified
import Ledger.Ada           qualified as Ada
import Ledger.Constraints   (mustPayToTheScript)
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
    }
    deriving Generic
    deriving anyclass
    ( FromJSON
    , ToJSON
    , ToSchema
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
    )
newtype DelayUnlock
    = DelayUnlock
    { _postponeForSlots :: Integer
    }
    deriving Generic
    deriving anyclass
    ( FromJSON
    , ToJSON
    , ToSchema
    )
type UnlockChest = ()

-- | Datum parameters
data ChestDatum
    = ChestDatum
    { _chestDeadline :: Ledger.POSIXTime
    , _chestCreator  :: Ledger.PaymentPubKeyHash
    }
    deriving Show
PlutusTx.unstableMakeIsData ''ChestDatum

-- | Redeemer parameters
newtype ChestRedeemer
    = ChestRedeemer
    { _redeemTime :: Ledger.POSIXTime
    }
    deriving Show
    deriving newtype
    ( PlutusTx.FromData
    , PlutusTx.ToData
    , PlutusTx.UnsafeFromData
    )
PlutusTx.makeLift ''ChestRedeemer

data Morbid
instance Scripts.ValidatorTypes Morbid where
    type instance DatumType     Morbid = ChestDatum
    type instance RedeemerType  Morbid = ChestRedeemer

------------------------------------------------------------ VALIDATOR FUNCTIONS ------------------------------------------------------------

{-# INLINABLE validate #-}
validate :: ChestDatum -> ChestRedeemer -> ScriptContext ->
    Bool
validate datum redeemer context = traceBool
    "Chest is eligible to be unlocked, congrats!"
    "Chest deadline has not been reached yet!"
    $ _chestDeadline datum < _redeemTime redeemer

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

-- | Modify chest state
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

-- | The "Create Chest" contract endpoint
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
                you = ChestDatum
                    { _chestDeadline = now + Haskell.fromInteger (deadline * 1_000)
                    , _chestCreator  = pkh
                    }
                txn = you `mustPayToTheScript` initialDeposit
            
            logStrShowAs logInfo "Creating chest for " you
            void $ submitTxConstraints typedValidator txn
        )   -- (accessChest pkh deadline initialDeposit "Creating Chest")
    

-- | The "Add Treasure" contract endpoint
addTreasure :: (AsContractError x) =>
    MorbidPromise x ()
addTreasure = endpoint @"2. Add Treasure" $ \ params -> do
    whenChestExists -- (accessChest pkh 30 deposit "Adding Treasure")
        (do logStrAs logInfo "Adding Treasure"
        ){-
    otherwise-}-- >>
        (logStrAs logError "ERROR addTreasure: There is no chest yet, please create one first!")
    

-- | The "Delay Unlock" contract endpoint
delayUnlock :: (AsContractError x) =>
    MorbidPromise x ()
delayUnlock = endpoint @"3. Delay Unlock" $ \ params -> do
    whenChestExists -- (accessChest pkh 30 (Ada.lovelaceValueOf 0) "Delaying Unlock")
        (do utxoS <- utxosAt contractAddress
            logStrShowAs logInfo "UTXOs are " utxoS
            
            case getChestDatumFrom utxoS of
                Just d@(ChestDatum _ chestCreator) -> do
                    logStrShowAs logInfo "Chest creator is " chestCreator
                    
                    pkh <- ownPaymentPubKeyHash
                    logStrShowAs logInfo "OwnPubKeyHash is " pkh
                    
                    if pkh == chestCreator
                        then do
                            now <- currentTime
                            logStrShowAs logInfo "Curr slot time = " now
                            
                            let deadline = _postponeForSlots params
                                you = d { _chestDeadline = now + Haskell.fromInteger (deadline * 1_000) }
                                txn = you `mustPayToTheScript` (Ada.lovelaceValueOf 0)
                            
                            logStrShowAs logInfo "Delaying unlock for " you
                            void $ submitTxConstraints typedValidator txn
                        else do
                            logStrAs logError "ERROR delayUnlock: You're not the chest creator!"
                Nothing -> do
                    logStrShowAs logError "ERROR delayUnlock . getChestDatumFrom $ utxoS: UTXOs are " utxoS
        ){-
    otherwise-}-- >>
        (logStrAs logError "ERROR delayUnlock: Cannot delay unlock as there is no chest yet, please create one first!")
    

-- | The "Unlock Chest" contract endpoint
unlockChest :: (AsContractError x) =>
    MorbidPromise x ()
unlockChest = endpoint @"4. Unlock Chest" $ \ _ -> do
    whenChestExists
        (do utxoS <- utxosAt contractAddress
            logStrShowAs logInfo "UTXOs are " utxoS
            
            now <- currentTime
            logStrShowAs logInfo "Curr slot time = " now
            
            let you = ChestRedeemer now
                txn = collectFromScript utxoS you
            
            logStrShowAs logInfo "Unlocking chest for " you
            void $ submitTxConstraintsSpending typedValidator utxoS txn
        ){-
    otherwise-}-- >>
        (logStrAs logError "ERROR unlockChest: There is no chest to unlock!")
    

------------------------------------------------------------ CONTRACT DEFINITIONS ------------------------------------------------------------

endpoints :: (AsContractError x) =>
    MorbidContract x ()
endpoints = selectList
            [ createChest
            , addTreasure
            , delayUnlock
            , unlockChest
            ]
type MorbidSchema = Endpoint "1. Create Chest" CreateChest
                .\/ Endpoint "2. Add Treasure" AddTreasure
                .\/ Endpoint "3. Delay Unlock" DelayUnlock
                .\/ Endpoint "4. Unlock Chest" UnlockChest
mkSchemaDefinitions ''MorbidSchema

$(mkKnownCurrencies [])
