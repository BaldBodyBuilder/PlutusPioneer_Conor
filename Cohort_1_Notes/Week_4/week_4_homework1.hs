{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Homework where

import Data.Aeson                 (FromJSON, ToJSON)
import Data.Functor               (void)
import Data.Text                  (Text, unpack)
import GHC.Generics               (Generic)
import Ledger
import Ledger.Ada                 as Ada
import Ledger.Constraints         as Constraints
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

data PayParams = PayParams
    { ppRecipient :: PubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = BlockchainActions .\/ Endpoint "pay" PayParams

payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- endpoint @"pay"
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp -- the void submitTx tx below basically makes it such that you void the tx because its too much 
	handleError (\err -> Contract.logInfo $ "We've caught an error Captain!: " ++ unpack err) $ void $ submitTx tx -- creates an error function that uses the unpack error list to string and 
    void $ submitTx tx
    payContract

-- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
-- recipient, but with amounts given by the two arguments. There should be a delay of one slot
-- after each endpoint call.

--Trace should run the pay contract on wallet 1 and twice send money to wallet 2, amount of lovelace to send is the x and the y, send x wait then y
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace x y = do
               h <- activateContractWallet (Wallet 1) payContract -- sets the two wallet handlers
			   callEndpoint @"pay" h $ PayParams
			       { ppRecipient = pubKeyHash $ walletPubKey $ Wallet 2
				   , ppLovelace = x -- giving the second wallet the value of x or 1000000
				   }
			   void $ Emulator.waitNSlots 1 --waiting the specified slots
               callEndpoint @"pay" h $ PayParams
			       { ppRecipient = pubKeyHash $ walletPubKey $ Wallet 2
				   , ppLovelace = y -- giving the second wallet the value of x or 20000000
				   }
               void $ Emulator.waitNSlots 1

payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 1000000 2000000 -- will work because each wallet has the safe amount of lovelace to send.

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000000000 2000000 --second trace should fail because the 1,000,000,000 is more than exists in the wallet thus add an error catch