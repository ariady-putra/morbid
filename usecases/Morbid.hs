{-# LANGUAGE DataKinds                   #-}
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
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Morbid where

{- DAVY JONES' LOCKER
A dead-man's switch contract where you can Create Chest and the chest can only be unlocked after 30 days of not being
postponed by the creator. You can also Add Treasure to the chest, and of course Delay Unlock by 30 days. Anyone can
redeem the treasure when the chest has passed the deadline.
-}
import Control.Monad        (void)
import Data.Map             qualified as Map

import Ledger               qualified
import Ledger.Ada           qualified as Ada
import Ledger.Constraints   (mustPayToTheScript)
import Ledger.Contexts      (ScriptContext (..))
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value         (Value)

import Playground.Contract
import Plutus.Contract
import PlutusTx             qualified
import PlutusTx.Prelude
import Prelude              qualified as Haskell

------------------------------------------------------------ DATATYPE DECLARATIONS ------------------------------------------------------------

-- | Parameter of endpoints
type CreateChest = Value
type AddTreasure = Value
type DelayUnlock = ()
type UnlockChest = ()

-- | Datum parameters
newtype ChestDatum
    = ChestDatum
    { _datumDeadline :: Ledger.POSIXTime
    }
    deriving (Show)
    deriving newtype
    ( PlutusTx.FromData
    , PlutusTx.ToData
    , PlutusTx.UnsafeFromData
    )
PlutusTx.makeLift ''ChestDatum

-- | Redeemer parameters
newtype ChestRedeemer
    = ChestRedeemer
    { _redeemTime :: Ledger.POSIXTime
    }
    deriving (Show)
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
validate :: ChestDatum -> ChestRedeemer -> ScriptContext -> Bool
validate datum redeemer context = traceBool
    "Chest is eligible to be unlocked, congrats!"
    "Chest deadline has not been reached yet!"
    $ _datumDeadline datum < _redeemTime redeemer

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

contractAddress :: Ledger.Address
contractAddress = Scripts.validatorAddress typedValidator

isChestAvailable :: AsContractError x => MorbidContract x Bool
isChestAvailable = utxosAt contractAddress >>= return . Haskell.not . Map.null

accessChest :: AsContractError x => Integer -> Value -> Haskell.String -> MorbidContract x ()
accessChest slot deposit message = do
    now  <- currentTime
    logInfo @Haskell.String $ "Current slot time is " ++ Haskell.show now
    
    let you = ChestDatum $ now + Haskell.fromInteger (slot * 1_000) -- 2_592_000_000 -- 30 x 24 x 3600 x 1000ms
        txn = you `mustPayToTheScript` deposit
    logInfo @Haskell.String message
    
    logInfo @Haskell.String $ Haskell.show you
    void $ submitTxConstraints typedValidator txn

type WhenTrue  = MorbidContract
type WhenFalse = MorbidContract
whenChestExists :: AsContractError x => WhenTrue x () -> WhenFalse x () -> MorbidContract x ()
whenChestExists doTrue doFalse = do
    chestExists <- isChestAvailable
    if chestExists then doTrue else doFalse

------------------------------------------------------------ ENDPOINT FUNCTIONS ------------------------------------------------------------

-- | The "Create Chest" contract endpoint
createChest :: AsContractError x => MorbidPromise x ()
createChest = endpoint @"1. Create Chest" $ \ initialDeposit -> do
    whenChestExists
        (logError @Haskell.String "There is an active chest already!")
        (accessChest 30 initialDeposit "Creating Chest")
    

-- | The "Add Treasure" contract endpoint
addTreasure :: AsContractError x => MorbidPromise x ()
addTreasure = endpoint @"2. Add Treasure" $ \ deposit -> do
    whenChestExists
        (accessChest 30 deposit "Adding Treasure")
        (logError @Haskell.String "There is no chest yet, please create one first!")
    

-- | The "Delay Unlock" contract endpoint
delayUnlock :: AsContractError x => MorbidPromise x ()
delayUnlock = endpoint @"3. Delay Unlock" $ \ _ -> do
    whenChestExists
        (accessChest 30 (Ada.lovelaceValueOf 0) "Delaying Unlock")
        (logError @Haskell.String "Cannot delay unlock as there is no chest yet, please create one first!")
    

-- | The "Unlock Chest" contract endpoint
unlockChest :: AsContractError x => MorbidPromise x ()
unlockChest = endpoint @"4. Unlock Chest" $ \ _ -> do
    whenChestExists
        (do utxoS  <- utxosAt contractAddress
            now    <- currentTime
            
            let you = ChestRedeemer now
                txn = collectFromScript utxoS you
            
            logInfo @Haskell.String $ "Unlocking chest for " ++ Haskell.show you
            void $ submitTxConstraintsSpending typedValidator utxoS txn
        )   -- otherwise:
        (logError @Haskell.String "There is no chest to unlock!")
    

------------------------------------------------------------ CONTRACT DEFINITIONS ------------------------------------------------------------

morbidContract :: AsContractError x => MorbidContract x ()
morbidContract = selectList
    [ createChest
    , addTreasure
    , delayUnlock
    , unlockChest
    ]
endpoints :: AsContractError x => MorbidContract x ()
endpoints = morbidContract

type MorbidSchema = Endpoint "1. Create Chest" CreateChest
                .\/ Endpoint "2. Add Treasure" AddTreasure
                .\/ Endpoint "3. Delay Unlock" DelayUnlock
                .\/ Endpoint "4. Unlock Chest" UnlockChest
mkSchemaDefinitions ''MorbidSchema

$(mkKnownCurrencies [])
