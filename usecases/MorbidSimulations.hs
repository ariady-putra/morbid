{-# LANGUAGE NamedFieldPuns     #-}
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

simulations :: [Simulation]
simulations = [basicMorbid, badGuess]
  where
    wallet1 = WalletNumber 1
    wallet2 = WalletNumber 2
    wallet3 = WalletNumber 3
    basicMorbid =
        Simulation
            { simulationName = "Basic Morbid"
            , simulationId = 1
            , simulationWallets = simulatorWallet registeredKnownCurrencies 100_000_000 <$> [wallet1, wallet2, wallet3]
            , simulationActions =
                  [ chestFunds wallet2
                  , AddBlocks 20
                  , unlockChest wallet1 (lovelaceValueOf 40_000_000)
                  , AddBlocks 40
                  , unlockChest wallet1 (lovelaceValueOf 40_000_000)
                  , AddBlocks 1
                  ]
            }
    badGuess =
        Simulation
            { simulationName = "One Bad Guess"
            , simulationId = 2
            , simulationWallets = simulatorWallet registeredKnownCurrencies 100_000_000 <$> [wallet1, wallet2, wallet3]
            , simulationActions =
                  [ chestFunds wallet2
                  , AddBlocks 20
                  , unlockChest wallet1 (lovelaceValueOf 40_000_000)
                  , AddBlocks 40
                  , unlockChest wallet1 (lovelaceValueOf 40_000_000)
                  , AddBlocks 1
                  ]
            }

chestFunds :: WalletNumber -> SimulatorAction
chestFunds caller = callEndpoint caller "Create Chest" ()

unlockChest :: WalletNumber -> Value -> SimulatorAction
unlockChest caller = callEndpoint caller "Unlock Chest"
