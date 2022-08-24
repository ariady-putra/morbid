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
redeem the ADA as long as the chest is unlockedable.
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
import           PlutusTx.Prelude         hiding (Semigroup (..), fold)
import qualified Prelude                  as Haskell (Semigroup (..), show)

------------------------------------------------------------

-- | Parameter of endpoints
type CreateChest = Value
type AddTreasure = Value
type DelayUnlock = ()
type UnlockChest = ()

data Morbid
instance Scripts.ValidatorTypes Morbid where
    type instance DatumType     Morbid = ()
    type instance RedeemerType  Morbid = ()

{-# INLINABLE validate #-}
validate :: () -> () -> ScriptContext -> Bool
validate _ _ sc = True

typedValidator :: Scripts.TypedValidator Morbid
typedValidator = Scripts.mkTypedValidator @Morbid
    $$(PlutusTx.compile [||         validate      ||])
    $$(PlutusTx.compile [|| Scripts.wrapValidator ||])

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
    let tvl = Constraints.typedValidatorLookups typedValidator
        txn = Constraints.mustPayToTheScript () initialDeposit
    constraints <- mkTxConstraints tvl txn
    
    let aut = Constraints.adjustUnbalancedTx constraints
    void $ submitUnbalancedTx aut

-- | The "Add Treasure" contract endpoint.
addTreasure :: AsContractError x => Promise () MorbidSchema x ()
addTreasure = endpoint @"2. Add Treasure" $ \ deposit -> do
    -- let tvl = Constraints.typedValidatorLookups typedValidator
    --     txn = Constraints.mustPayToTheScript () deposit
    -- constraints <- mkTxConstraints tvl txn
    
    -- let aut = Constraints.adjustUnbalancedTx constraints
    -- void $ submitUnbalancedTx aut
    error ()

-- | The "Delay Unlock" contract endpoint.
delayUnlock :: AsContractError x => Promise () MorbidSchema x ()
delayUnlock = endpoint @"3. Delay Unlock" $ \ _ -> do
    -- utxoS  <- utxosAt contractAddress
    
    -- let txn = collectFromScript utxoS ()
    -- void $ submitTxConstraintsSpending typedValidator utxoS txn
    error ()

-- | The "Unlock Chest" contract endpoint.
unlockChest :: AsContractError x => Promise () MorbidSchema x ()
unlockChest = endpoint @"4. Unlock Chest" $ \ _ -> do
    utxoS  <- utxosAt contractAddress
    
    let txn = collectFromScript utxoS ()
    void $ submitTxConstraintsSpending typedValidator utxoS txn

endpoints :: AsContractError x => Contract () MorbidSchema x ()
endpoints = morbidContract

type MorbidSchema = Endpoint "1. Create Chest" CreateChest
                .\/ Endpoint "2. Add Treasure" AddTreasure
                .\/ Endpoint "3. Delay Unlock" DelayUnlock
                .\/ Endpoint "4. Unlock Chest" UnlockChest
mkSchemaDefinitions ''MorbidSchema

$(mkKnownCurrencies [])
