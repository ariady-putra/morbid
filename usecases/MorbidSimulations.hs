{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module MorbidSimulations where

import Ledger.Ada qualified as Ada

import Morbid (CreateChest (..))
import Morbid (AddTreasure (..))
import Morbid (DelayUnlock (..))
import Morbid (UnlockChest)
import Morbid (registeredKnownCurrencies)

import Playground.Types (ContractCall (AddBlocks))
import Playground.Types (Simulation  (Simulation))
import Playground.Types (SimulatorAction)
import Playground.Types (simulationActions)
import Playground.Types (simulationId)
import Playground.Types (simulationName)
import Playground.Types (simulationWallets)

import SimulationUtils (callEndpoint)
import SimulationUtils (simulatorWallet)

import Wallet.Emulator.Types (WalletNumber (..))

type Deposit  = Integer
type Slot     = Integer
type Password = String

createChest :: WalletNumber -> CreateChest ->
    SimulatorAction
createChest = (`callEndpoint` "1. Create Chest")

addTreasure :: WalletNumber -> AddTreasure ->
    SimulatorAction
addTreasure = (`callEndpoint` "2. Add Treasure")

delayUnlock :: WalletNumber -> DelayUnlock ->
    SimulatorAction
delayUnlock = (`callEndpoint` "3. Delay Unlock")

unlockChest :: WalletNumber -> UnlockChest ->
    SimulatorAction
unlockChest = (`callEndpoint` "4. Unlock Chest")

createChestFor :: Password -> Deposit ->
    CreateChest
createChestFor password deposit = CreateChest
    { _initialDeposit = Ada.lovelaceValueOf deposit
    , _lockForSlots   = 30
    , _createPassword = password
    }

depositTreasure :: Deposit ->
    AddTreasure
depositTreasure deposit = AddTreasure
    { _deposit = Ada.lovelaceValueOf deposit
    }

postponeUnlock :: Password ->
    DelayUnlock
postponeUnlock password = DelayUnlock
    { _postponeForSlots = 30
    , _password         = password
    }

simulations :: [Simulation]
simulations =
    [ davyJonesLocker
    , duplicateChest
    , noChestToDelay
    , invalidPassword
    , noChestToUnlock
    , unlockToSoon
    ] where wallet = map (WalletNumber) [1..]
            davyJonesLocker =
                Simulation
                { simulationName = "Davy Jones' Locker"
                , simulationId = 1
                , simulationWallets = simulatorWallet registeredKnownCurrencies 100_000_000 <$> take 4 wallet
                , simulationActions =
                    [ createChest (wallet !! 0) (createChestFor "DavyJones" 50_000_000)
                    , AddBlocks 10
                    , addTreasure (wallet !! 1) (depositTreasure 25_000_000)
                    , AddBlocks 10
                    , delayUnlock (wallet !! 2) (postponeUnlock "DavyJones")
                    , AddBlocks 30
                    , unlockChest (wallet !! 3) ()
                    , AddBlocks 10
                    ]
                }
            duplicateChest =
                Simulation
                    { simulationName = "Duplicate Chest"
                    , simulationId = 2
                    , simulationWallets = simulatorWallet registeredKnownCurrencies 100_000_000 <$> take 2 wallet
                    , simulationActions =
                        [ createChest (wallet !! 0) (createChestFor "DavyJones" 50_000_000)
                        , AddBlocks 10
                        , createChest (wallet !! 1) (createChestFor "FlyingDutchman" 25_000_000)
                        , AddBlocks 10
                        ]
                    }
            noChestToDelay =
                Simulation
                    { simulationName = "No chest to Postpone"
                    , simulationId = 3
                    , simulationWallets = simulatorWallet registeredKnownCurrencies 100_000_000 <$> take 1 wallet
                    , simulationActions =
                        [ delayUnlock (wallet !! 0) (postponeUnlock "DavyJones")
                        , AddBlocks 10
                        ]
                    }
            invalidPassword =
                Simulation
                    { simulationName = "Wrong Password"
                    , simulationId = 4
                    , simulationWallets = simulatorWallet registeredKnownCurrencies 100_000_000 <$> take 2 wallet
                    , simulationActions =
                        [ createChest (wallet !! 0) (createChestFor "DavyJones" 50_000_000)
                        , AddBlocks 10
                        , delayUnlock (wallet !! 1) (postponeUnlock "FlyingDutchman")
                        , AddBlocks 10
                        ]
                    }
            noChestToUnlock =
                Simulation
                    { simulationName = "No chest to Unlock"
                    , simulationId = 5
                    , simulationWallets = simulatorWallet registeredKnownCurrencies 100_000_000 <$> take 1 wallet
                    , simulationActions =
                        [ unlockChest (wallet !! 0) ()
                        , AddBlocks 10
                        ]
                    }
            unlockToSoon =
                Simulation
                    { simulationName = "Unlock too Soon"
                    , simulationId = 6
                    , simulationWallets = simulatorWallet registeredKnownCurrencies 100_000_000 <$> take 4 wallet
                    , simulationActions =
                        [ createChest (wallet !! 0) (createChestFor "DavyJones" 50_000_000)
                        , AddBlocks 10
                        , addTreasure (wallet !! 1) (depositTreasure 25_000_000)
                        , AddBlocks 10
                        , delayUnlock (wallet !! 2) (postponeUnlock "DavyJones")
                        , AddBlocks 29
                        , unlockChest (wallet !! 3) ()
                        , AddBlocks 10
                        ]
                    }
                
            
        
    
