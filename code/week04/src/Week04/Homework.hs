{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Homework where

import           Control.Monad.Freer.Extras as Extras
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Functor               (void)
import           Data.Text                  (Text, unpack)
import           GHC.Generics               (Generic)
import           Ledger
import           Ledger.Ada                 as Ada
import           Ledger.Constraints         as Constraints
import           Plutus.Contract            as Contract
import           Plutus.Trace.Emulator      as Emulator
import           Wallet.Emulator.Wallet

data PayParams = PayParams
    { ppRecipient :: PubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- endpoint @"pay"
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    void $ submitTx tx
    payContract

c :: Contract () PaySchema Text ()
c = Contract.handleError (\err -> payContract) payContract

-- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
-- recipient, but with amounts given by the two arguments. There should be a delay of one slot
-- after each endpoint call.
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace amount1 amount2 = do
    h1 <- activateContractWallet (Wallet 1) c
    Extras.logInfo @String "Calling endpoint first time"
    callEndpoint @"pay" h1 $ PayParams
        {
            ppRecipient= pubKeyHash $ walletPubKey $ Wallet 2,
            ppLovelace= amount1
        }

    Extras.logInfo @String "waiting one slot"
    void $ Emulator.waitNSlots 1
    Extras.logInfo @String "Calling endpoint second time"
    callEndpoint @"pay" h1 $ PayParams
        {
            ppRecipient= pubKeyHash $ walletPubKey $ Wallet 2,
            ppLovelace= amount2
        }
    s <- Emulator.waitNSlots 1
    Extras.logInfo $ "reached end" ++ show s



payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 1000000 2000000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000000000 2000000
