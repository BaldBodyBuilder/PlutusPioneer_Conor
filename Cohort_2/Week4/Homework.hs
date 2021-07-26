{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Homework where

import Data.Aeson            (FromJSON, ToJSON)
import Data.Functor          (void)
import Data.Text             (Text)
import GHC.Generics          (Generic)
import Ledger
import Ledger.Ada            as Ada
import Ledger.Constraints    as Constraints
import Plutus.Contract       as Contract
import Plutus.Trace.Emulator as Emulator

data PayParams = PayParams
    { ppRecipient :: PubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- endpoint @"pay"
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    handleError(\err -> Contract.LogInfo $ "Caught Error: " ++ unpack err) $ void $ submitTx tx
    -- void $ submitTx tx // This doesn't account for the potential for incorrect sx /rx
    payContract

-- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
-- recipient, but with amounts given by the two arguments. There should be a delay of one slot
-- after each endpoint call.
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace a b = do
     h1 <- activateContractWallet (Wallet 1) payContract
     callEndpoint @"pay" h1 $ PayParams
     { ppRecipient = pubKeyHash $ walletPubKey $ Wallet 2
     , ppLovelace  = a
     }
     void $ Emulator.waitNSlots 1
     callEndpoint @"pay" h1 $ PayParams
     { ppRecipient = pubKeyHash $ walletPubKey $ Wallet 2
     , ppLovelace  = b
     }
     void $ Emulator.waitNSlots 1
     

     {-
     Load wallet 1
     Load wallet 2
     w1 pay w2 // checks tx value
     wait
     w1 pay w2 // checks tx vaue
     wait
     function to ensure the sending amount is present within the sx wallet

     -}

payTest1 :: IO () -- 1 A 2 A
payTest1 = runEmulatorTraceIO $ payTrace 1000000 2000000

payTest2 :: IO () -- 1000 A 2000 A
payTest2 = runEmulatorTraceIO $ payTrace 1000000000 2000000

-- Also need to make sure that the sending wallets have the ADA value within them to send
    -- See the payContract