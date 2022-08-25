{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveAnyClass              #-}
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
A dead-man's switch contract where you can Create Chest
and the chest can only be unlocked after a period of time
of not being postponed. You can also Add Treasure to the
chest. Anyone can redeem the treasure when the deadline
has passed.
-}
import Control.Monad         (void)
import Data.Map              qualified as Map
import Data.ByteString.Char8 qualified as C8

import Ledger                qualified
import Ledger.Ada            qualified as Ada
import Ledger.Constraints    (mustPayToTheScript)
import Ledger.Contexts       (ScriptContext (..))
import Ledger.Typed.Scripts  qualified as Scripts
import Ledger.Value          (Value)

import Playground.Contract
import Plutus.Contract
import PlutusTx              qualified
import PlutusTx.Prelude
import Prelude               qualified as Haskell

------------------------------------------------------------ DATATYPE DECLARATIONS ------------------------------------------------------------

-- | Parameter of endpoints
data CreateChest
    = CreateChest
    { _initialDeposit :: Value
    , _lockForSlots   :: Integer
    , _createPassword :: Haskell.String
    }
    deriving (Generic)
    deriving anyclass
    ( FromJSON
    , ToJSON
    , ToSchema
    )
type AddTreasure = Value
type DelayUnlock = ()
type UnlockChest = ()

-- | Datum parameters
data ChestDatum
    = ChestDatum
    { _chestDeadline :: Ledger.POSIXTime
    , _chestPassword :: BuiltinByteString
    }
    deriving (Show)
PlutusTx.unstableMakeIsData ''ChestDatum

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

-- | Convert Haskell.String to hashed BuiltinByteString
hashString :: Haskell.String -> BuiltinByteString
hashString = sha2_256 . toBuiltin . C8.pack

-- | Log info from 2 Haskell.String
logInfo2 :: (AsContractError x) => Haskell.String -> Haskell.String -> MorbidContract x ()
logInfo2 str = logInfo . (str++)

-- | Log info from Haskell.show s
logInfoShow :: (Show s, AsContractError x) => s -> MorbidContract x ()
logInfoShow = logInfoStrShow ""

-- | Log info from Haskell.String and Haskell.show s
logInfoStrShow :: (Show s, AsContractError x) => Haskell.String -> s -> MorbidContract x ()
logInfoStrShow str = (logInfo2 str) . Haskell.show

-- | Log info from Haskell.String
logInfoStr :: (AsContractError x) => Haskell.String -> MorbidContract x ()
logInfoStr = logInfo

-- | Log error from Haskell.String
logErrorStr :: (AsContractError x) => Haskell.String -> MorbidContract x ()
logErrorStr = logError

-- | Check if chest has any UTXOs
isChestAvailable :: (AsContractError x) => MorbidContract x Bool
isChestAvailable = utxosAt contractAddress >>= return . Haskell.not . Map.null

-- | Modify chest state
accessChest :: (AsContractError x) => Integer -> Value -> Haskell.String -> MorbidContract x ()
accessChest slot deposit message = do
    utxoS <- utxosAt contractAddress
    logInfoShow utxoS
    
    now   <- currentTime
    logInfoStrShow "Current slot time is " now
    
    --  let you = ChestDatum $ now + Haskell.fromInteger (slot * 1_000) -- 2_592_000_000 -- 30 x 24 x 3600 x 1000ms
    let you = ChestDatum
            { _chestDeadline = now + Haskell.fromInteger (slot * 1_000)
            , _chestPassword = hashString message
            }
        txn = you `mustPayToTheScript` deposit
    logInfoStr message
    
    logInfoShow you
    void $ submitTxConstraints typedValidator txn

type WhenTrue  = MorbidContract
type WhenFalse = MorbidContract
-- | Do action based on chest existence
whenChestExists :: (AsContractError x) => WhenTrue x () -> WhenFalse x () -> MorbidContract x ()
whenChestExists doTrue doFalse = do
    chestExists <- isChestAvailable
    if chestExists then doTrue else doFalse

------------------------------------------------------------ ENDPOINT FUNCTIONS ------------------------------------------------------------

-- | The "Create Chest" contract endpoint
createChest :: (AsContractError x) => MorbidPromise x ()
createChest = endpoint @"1. Create Chest" $ \ params -> do
    whenChestExists
        (logErrorStr "There is an active chest already!"){-
    otherwise-}-- >>
        (do now   <- currentTime
            logInfoStrShow "Current slot time is " now
            
            let initialDeposit = _initialDeposit params
                deadline       = _lockForSlots   params
                password       = _createPassword params
                you = ChestDatum
                    { _chestDeadline = now + Haskell.fromInteger (deadline * 1_000)
                    , _chestPassword = hashString password
                    }
                txn = you `mustPayToTheScript` initialDeposit
            
            logInfoStrShow "Creating chest for " you
            void $ submitTxConstraints typedValidator txn
        )   -- (accessChest deadline initialDeposit "Creating Chest")
    

-- | The "Add Treasure" contract endpoint
addTreasure :: (AsContractError x) => MorbidPromise x ()
addTreasure = endpoint @"2. Add Treasure" $ \ params -> do
    whenChestExists -- (accessChest 30 deposit "Adding Treasure")
        (do logInfoStr "Adding Treasure"
        ){-
    otherwise-}-- >>
        (logErrorStr "There is no chest yet, please create one first!")
    

-- | The "Delay Unlock" contract endpoint
delayUnlock :: (AsContractError x) => MorbidPromise x ()
delayUnlock = endpoint @"3. Delay Unlock" $ \ _ -> do
    whenChestExists -- (accessChest 30 (Ada.lovelaceValueOf 0) "Delaying Unlock")
        (do utxoS <- utxosAt contractAddress
            logInfoShow utxoS
            
            now   <- currentTime
            logInfoStrShow "Current slot time is " now
            
            let you = ChestDatum
                    { _chestDeadline = now + Haskell.fromInteger 30_000
                    , _chestPassword = hashString ("" :: Haskell.String)
                    }
                txn = you `mustPayToTheScript` (Ada.lovelaceValueOf 0)
            
            logInfoStrShow "Delaying unlock for " you
            void $ submitTxConstraints typedValidator txn
        ){-
    otherwise-}-- >>
        (logErrorStr "Cannot delay unlock as there is no chest yet, please create one first!")
    

-- | The "Unlock Chest" contract endpoint
unlockChest :: (AsContractError x) => MorbidPromise x ()
unlockChest = endpoint @"4. Unlock Chest" $ \ _ -> do
    whenChestExists
        (do utxoS  <- utxosAt contractAddress
            logInfoShow utxoS
            
            now    <- currentTime
            logInfoStrShow "Current slot time is " now
            
            let you = ChestRedeemer now
                txn = collectFromScript utxoS you
            
            logInfoStrShow "Unlocking chest for " you
            void $ submitTxConstraintsSpending typedValidator utxoS txn
        ){-
    otherwise-}-- >>
        (logErrorStr "There is no chest to unlock!")
    

------------------------------------------------------------ CONTRACT DEFINITIONS ------------------------------------------------------------

endpoints :: (AsContractError x) => MorbidContract x ()
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
