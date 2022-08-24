{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -fno-ignore-interface-pragmas   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports        #-}
{-# OPTIONS_GHC -fno-warn-unused-matches        #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

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
                  , AddBlocks 1
                  , AddBlocks 50
                  , unlockChest wallet2
                  , AddBlocks 1
                  ]
            }
    tooSoon =
        Simulation
            { simulationName = "Too Soon"
            , simulationId = 2
            , simulationWallets = simulatorWallet registeredKnownCurrencies 100_000_000 <$> [wallet1, wallet2, wallet3]
            , simulationActions =
                  [ createChest wallet1 (lovelaceValueOf 50_000_000)
                  , AddBlocks 1
                  , AddBlocks 49
                  , unlockChest wallet2
                  , AddBlocks 1
                  ]
            }
        
    
