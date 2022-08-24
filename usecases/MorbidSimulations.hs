{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    wallet1 = WalletNumber 1
    wallet2 = WalletNumber 2
    wallet3 = WalletNumber 3
    davyJonesLocker =
        Simulation
            { simulationName = "Davy Jones' Locker"
            , simulationId = 1
            , simulationWallets = simulatorWallet registeredKnownCurrencies 100_000_000 <$> [wallet1, wallet2, wallet3]
            , simulationActions =
                  [ createChest wallet1 (lovelaceValueOf 50_000_000)
                  , AddBlocks 100_000
                  , addTreasure wallet1 (lovelaceValueOf 20_000_000)
                  , AddBlocks 500_000
                  , delayUnlock wallet1
                  , AddBlocks 1_000_000
                  , unlockChest wallet2
                  , AddBlocks 1_000_000
                  , unlockChest wallet3
                  , AddBlocks 1_000
                  ]
            }
    tooSoon =
        Simulation
            { simulationName = "Too Soon"
            , simulationId = 2
            , simulationWallets = simulatorWallet registeredKnownCurrencies 100_000_000 <$> [wallet1, wallet2, wallet3]
            , simulationActions =
                  [ createChest wallet1 (lovelaceValueOf 50_000_000)
                  , AddBlocks 100
                  , addTreasure wallet1 (lovelaceValueOf 20_000_000)
                  , AddBlocks 500
                  , delayUnlock wallet1
                  , AddBlocks 1_000
                  , unlockChest wallet2
                  , AddBlocks 1_000
                  , unlockChest wallet3
                  , AddBlocks 1
                  ]
            }
        
    
