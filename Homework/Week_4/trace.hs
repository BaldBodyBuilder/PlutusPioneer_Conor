import Control.Monad.Freer.Extras as Extras
import Data.Functor              (void)
import Ledger
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

import Week04.Vesting

-- Contract w s e a 
-- EmulatorTrace a

test :: IO ()
test = runEmulatorTraceIO  myTrace


myTrace :: EmulatorTrace ()
myTrace = do
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"give" h1 $ GiveParams
        { gpBeneficiary = pubKeyHash $ walletPubKey $ Wallet 2
        , gpDeadline = Slot 20
        , gpAmount = 1000
        }
    void $ waitUntilSlot 20 -- We call the warning but use the functor void to avoid the actual stating of the sideffects, it happens quietly
    callEndpoint @"grab" h2 ()
	void $ waitNSlots 1