{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module MorbidSimulations where

import Ledger.Ada            (lovelaceValueOf)
import Ledger.Value          (Value)

import Morbid                (registeredKnownCurrencies)

import Playground.Types      (ContractCall (AddBlocks))
import Playground.Types      (Simulation (Simulation))
import Playground.Types      (SimulatorAction)
import Playground.Types      (simulationActions)
import Playground.Types      (simulationId)
import Playground.Types      (simulationName)
import Playground.Types      (simulationWallets)

import SimulationUtils       (callEndpoint)
import SimulationUtils       (simulatorWallet)

import Wallet.Emulator.Types (WalletNumber (..))

createChest :: WalletNumber -> Value -> SimulatorAction
createChest caller = callEndpoint caller "1. Create Chest"

addTreasure :: WalletNumber -> Value -> SimulatorAction
addTreasure caller = callEndpoint caller "2. Add Treasure"

delayUnlock :: WalletNumber -> SimulatorAction
delayUnlock caller = callEndpoint caller "3. Delay Unlock" ()

unlockChest :: WalletNumber -> SimulatorAction
unlockChest caller = callEndpoint caller "4. Unlock Chest" ()

simulations :: [Simulation]
simulations = [davyJonesLocker, tooSoon]
  where
    wallet = map (WalletNumber) [1..4]
    davyJonesLocker =
        Simulation
            { simulationName = "Davy Jones' Locker"
            , simulationId = 1
            , simulationWallets = simulatorWallet registeredKnownCurrencies 100_000_000 <$> wallet
            , simulationActions =
                  [ createChest (wallet !! 1) (lovelaceValueOf 50_000_000)
                  , AddBlocks 1
                  , addTreasure (wallet !! 2) (lovelaceValueOf 25_000_000)
                  , AddBlocks 1
                  , delayUnlock (wallet !! 3)
                  , AddBlocks 15
                  , AddBlocks 15
                  , unlockChest (wallet !! 4)
                  , AddBlocks 1
                  ]
            }
    tooSoon =
        Simulation
            { simulationName = "Too Soon"
            , simulationId = 2
            , simulationWallets = simulatorWallet registeredKnownCurrencies 100_000_000 <$> wallet
            , simulationActions =
                  [ createChest (wallet !! 1) (lovelaceValueOf 50_000_000)
                  , AddBlocks 1
                  , addTreasure (wallet !! 2) (lovelaceValueOf 25_000_000)
                  , AddBlocks 1
                  , delayUnlock (wallet !! 3)
                  , AddBlocks 15
                  , AddBlocks 14
                  , unlockChest (wallet !! 4)
                  , AddBlocks 1
                  ]
            }
        
    
