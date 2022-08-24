{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-ignore-interface-pragmas   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports        #-}
{-# OPTIONS_GHC -fno-warn-unused-matches        #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Morbid where

{- DAVY JONES' LOCKER
A dead-man's switch contract where you can Create Chest and
the chest can only be unlocked after 30 days of not being
postponed by the creator. You can also Add Treasure to the
chest, and of course Delay Unlock by 30 days. Anyone can
redeem the treasure when the chest has passed the deadline.
-}
import           Control.Lens            (view)
import           Control.Monad           (void, when)

import           Data.Default            (Default (def))
import qualified Data.Map                 as Map
import qualified Data.Text                as T

import           Ledger                  (Address, Validator)
import           Ledger                  (PaymentPubKeyHash (unPaymentPubKeyHash))
import           Ledger                  (POSIXTime, POSIXTimeRange)
import qualified Ledger.Ada               as Ada
import           Ledger.Constraints      (TxConstraints)
import           Ledger.Constraints      (mustBeSignedBy, mustPayToTheScript, mustValidateIn)
import qualified Ledger.Constraints       as Constraints
import           Ledger.Contexts         (ScriptContext (..), TxInfo (..))
import qualified Ledger.Contexts          as Validation
import qualified Ledger.Interval          as Interval
import qualified Ledger.TimeSlot          as TimeSlot
import qualified Ledger.Tx                as Tx
import qualified Ledger.Typed.Scripts     as Scripts
import           Ledger.Value            (Value)
import qualified Ledger.Value             as Value

import           Playground.Contract
import           Plutus.Contract
import           Plutus.Contract.Test
import qualified Plutus.Contract.Typed.Tx as Typed
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude                  as Haskell

------------------------------------------------------------

-- | Parameter of endpoints
type CreateChest = Value
type AddTreasure = Value
type DelayUnlock = ()
type UnlockChest = ()

-- | Datum parameters
newtype ChestDatum
    = ChestDatum
    { _dDeadline :: POSIXTime
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
    = ChestRedeemer ()
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

{-# INLINABLE validate #-}
validate :: ChestDatum -> ChestRedeemer -> ScriptContext -> Bool
validate datum redeemer context = True

typedValidator :: Scripts.TypedValidator Morbid
typedValidator = Scripts.mkTypedValidator @Morbid
    $$(PlutusTx.compile [|| validate ||])
    $$(PlutusTx.compile [||   wrap   ||])
    where wrap = Scripts.wrapValidator
                @ChestDatum
                @ChestRedeemer
            
        
    

contractAddress :: Ledger.Address
contractAddress = Scripts.validatorAddress typedValidator

morbidContract :: AsContractError x => Contract () MorbidSchema x ()
morbidContract = selectList
    [ createChest
    , addTreasure
    , delayUnlock
    , unlockChest
    ]

-- | The "Create Chest" contract endpoint
createChest :: AsContractError x => Promise () MorbidSchema x ()
createChest = endpoint @"1. Create Chest" $ \ initialDeposit -> do
    let you = ChestDatum $ TimeSlot.scSlotZeroTime def + 2_592_000_000 -- 30 x 24 x 3600 x 1000ms
        txn = you `Constraints.mustPayToTheScript` initialDeposit
    logInfo @Haskell.String $ "Creating chest for " ++ Haskell.show you
    void $ submitTxConstraints typedValidator txn

-- | The "Add Treasure" contract endpoint
addTreasure :: AsContractError x => Promise () MorbidSchema x ()
addTreasure = endpoint @"2. Add Treasure" $ \ deposit -> do
    error ()

-- | The "Delay Unlock" contract endpoint
delayUnlock :: AsContractError x => Promise () MorbidSchema x ()
delayUnlock = endpoint @"3. Delay Unlock" $ \ _ -> do
    error ()

-- | The "Unlock Chest" contract endpoint
unlockChest :: AsContractError x => Promise () MorbidSchema x ()
unlockChest = endpoint @"4. Unlock Chest" $ \ _ -> do
    utxoS  <- utxosAt contractAddress
    
    let you = ChestRedeemer ()
        txn = collectFromScript utxoS you
    logInfo @Haskell.String $ "Unlocking chest"
    void $ submitTxConstraintsSpending typedValidator utxoS txn

endpoints :: AsContractError x => Contract () MorbidSchema x ()
endpoints = morbidContract

type MorbidSchema = Endpoint "1. Create Chest" CreateChest
                .\/ Endpoint "2. Add Treasure" AddTreasure
                .\/ Endpoint "3. Delay Unlock" DelayUnlock
                .\/ Endpoint "4. Unlock Chest" UnlockChest
mkSchemaDefinitions ''MorbidSchema

$(mkKnownCurrencies [])
