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
import           Prelude                  as Haskell (Semigroup (..), show)

------------------------------------------------------------

-- | Parameter of endpoints
-- type CreateChest = Value
-- type AddTreasure = Value

type MorbidSchema =
        Endpoint "Create Chest" ()
        .\/ Endpoint "Unlock Chest" Value

-- | Tranche of a vesting scheme.
data MorbidTranche = MorbidTranche {
    morbidTrancheDate   :: POSIXTime,
    morbidTrancheAmount :: Value
    } deriving Generic

PlutusTx.makeLift ''MorbidTranche

-- | A vesting scheme consisting of two tranches. Each tranche defines a date
--   (POSIX time) after which an additional amount can be spent.
data MorbidParams = MorbidParams {
    morbidTranche1 :: MorbidTranche,
    morbidTranche2 :: MorbidTranche,
    morbidOwner    :: PaymentPubKeyHash
    } deriving Generic

PlutusTx.makeLift ''MorbidParams

{-# INLINABLE totalAmount #-}
-- | The total amount vested
totalAmount :: MorbidParams -> Value
totalAmount MorbidParams{morbidTranche1,morbidTranche2} =
    morbidTrancheAmount morbidTranche1 + morbidTrancheAmount morbidTranche2

{-# INLINABLE availableFrom #-}
-- | The amount guaranteed to be available from a given tranche in a given time range.
availableFrom :: MorbidTranche -> POSIXTimeRange -> Value
availableFrom (MorbidTranche d v) range =
    -- The valid range is an open-ended range starting from the tranche vesting date
    let validRange = Interval.from d
    -- If the valid range completely contains the argument range (meaning in particular
    -- that the start time of the argument range is after the tranche vesting date), then
    -- the money in the tranche is available, otherwise nothing is available.
    in if validRange `Interval.contains` range then v else zero

availableAt :: MorbidParams -> POSIXTime -> Value
availableAt MorbidParams{morbidTranche1, morbidTranche2} sl =
    let f MorbidTranche{morbidTrancheDate, morbidTrancheAmount} =
            if sl >= morbidTrancheDate then morbidTrancheAmount else mempty
    in foldMap f [morbidTranche1, morbidTranche2]

{-# INLINABLE remainingFrom #-}
-- | The amount that has not been released from this tranche yet
remainingFrom :: MorbidTranche -> POSIXTimeRange -> Value
remainingFrom t@MorbidTranche{morbidTrancheAmount} range =
    morbidTrancheAmount - availableFrom t range

{-# INLINABLE validate #-}
validate :: MorbidParams -> () -> () -> ScriptContext -> Bool
validate MorbidParams{morbidTranche1, morbidTranche2, morbidOwner} () () ctx@ScriptContext{scriptContextTxInfo=txInfo@TxInfo{txInfoValidRange}} =
    let
        remainingActual  = Validation.valueLockedBy txInfo (Validation.ownHash ctx)

        remainingExpected =
            remainingFrom morbidTranche1 txInfoValidRange
            + remainingFrom morbidTranche2 txInfoValidRange

    in remainingActual `Value.geq` remainingExpected
            -- The policy encoded in this contract
            -- is "vestingOwner can do with the funds what they want" (as opposed
            -- to "the funds must be paid to vestingOwner"). This is enforcey by
            -- the following condition:
            && Validation.txSignedBy txInfo (unPaymentPubKeyHash morbidOwner)
            -- That way the recipient of the funds can pay them to whatever address they
            -- please, potentially saving one transaction.

data Morbid
instance Scripts.ValidatorTypes Morbid where
    type instance RedeemerType Morbid = ()
    type instance DatumType Morbid = ()

morbidScript :: MorbidParams -> Validator
morbidScript = Scripts.validatorScript . typedValidator

typedValidator :: MorbidParams -> Scripts.TypedValidator Morbid
typedValidator = Scripts.mkTypedValidatorParam @Morbid
    $$(PlutusTx.compile [|| validate ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator

contractAddress :: MorbidParams -> Ledger.Address
contractAddress = Scripts.validatorAddress . typedValidator

morbidContract :: MorbidParams -> Contract () MorbidSchema T.Text ()
morbidContract morbid = selectList [chest, retrieve]
  where
    chest = endpoint @"Create Chest" $ \_ -> chestFundsC morbid
    retrieve = endpoint @"Unlock Chest" $ \payment -> do
        liveness <- retrieveFundsC morbid payment
        case liveness of
            Alive -> awaitPromise retrieve
            Dead  -> pure ()

payIntoContract :: Value -> TxConstraints () ()
payIntoContract = mustPayToTheScript ()

chestFundsC
    :: MorbidParams
    -> Contract () s T.Text ()
chestFundsC morbid = do
    let txn = payIntoContract (totalAmount morbid)
    mkTxConstraints (Constraints.typedValidatorLookups $ typedValidator morbid) txn
      >>= void . submitUnbalancedTx . Constraints.adjustUnbalancedTx

data Liveness = Alive | Dead

retrieveFundsC
    :: MorbidParams
    -> Value
    -> Contract () s T.Text Liveness
retrieveFundsC morbid payment = do
    let inst = typedValidator morbid
        addr = Scripts.validatorAddress inst
    nextTime <- awaitTime 0
    unspentOutputs <- utxosAt addr
    let
        currentlyLocked = foldMap (view Tx.ciTxOutValue) (Map.elems unspentOutputs)
        remainingValue = currentlyLocked - payment
        mustRemainLocked = totalAmount morbid - availableAt morbid nextTime
        maxPayment = currentlyLocked - mustRemainLocked

    when (remainingValue `Value.lt` mustRemainLocked)
        $ throwError
        $ T.unwords
            [ "Cannot take out"
            , T.pack (show payment) `T.append` "."
            , "The maximum is"
            , T.pack (show maxPayment) `T.append` "."
            , "At least"
            , T.pack (show mustRemainLocked)
            , "must remain locked by the script."
            ]

    let liveness = if remainingValue `Value.gt` mempty then Alive else Dead
        remainingOutputs = case liveness of
                            Alive -> payIntoContract remainingValue
                            Dead  -> mempty
        txn = Typed.collectFromScript unspentOutputs ()
                <> remainingOutputs
                <> mustValidateIn (Interval.from nextTime)
                <> mustBeSignedBy (morbidOwner morbid)
                -- we don't need to add a pubkey output for 'vestingOwner' here
                -- because this will be done by the wallet when it balances the
                -- transaction.
    mkTxConstraints (Constraints.typedValidatorLookups inst
                  <> Constraints.unspentOutputs unspentOutputs) txn
      >>= void . submitUnbalancedTx . Constraints.adjustUnbalancedTx
    return liveness

endpoints :: Contract () MorbidSchema T.Text ()
endpoints = morbidContract morbidParams
  where
    morbidOwner = mockWalletPaymentPubKeyHash w1
    morbidParams =
        MorbidParams {morbidTranche1, morbidTranche2, morbidOwner}
    morbidTranche1 =
        MorbidTranche
            {morbidTrancheDate = TimeSlot.scSlotZeroTime def + 20000, morbidTrancheAmount = Ada.lovelaceValueOf 50_000_000}
    morbidTranche2 =
        MorbidTranche
            {morbidTrancheDate = TimeSlot.scSlotZeroTime def + 40000, morbidTrancheAmount = Ada.lovelaceValueOf 30_000_000}

mkSchemaDefinitions ''MorbidSchema

$(mkKnownCurrencies [])
