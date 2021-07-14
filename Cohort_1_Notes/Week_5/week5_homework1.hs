{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week05.Homework1 where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Aeson                 (ToJSON, FromJSON)
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import           GHC.Generics               (Generic)
import           Plutus.Contract            as Contract hiding (when)
import           Plutus.Trace.Emulator      as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Ledger                     hiding (singleton)
import           Ledger.Constraints         as Constraints
import qualified Ledger.Typed.Scripts       as Scripts
import           Ledger.Value               as Value
import           Playground.Contract        (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH              (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types           (KnownCurrency (..))
import           Prelude                    (Semigroup (..))
import           Text.Printf                (printf)
import           Wallet.Emulator.Wallet

{-# INLINABLE mkPolicy #-}
-- This policy should only allow minting (or burning) of tokens if the owner of the specified PubKeyHash
-- has signed the transaction and if the specified deadline has not passed.
-- This is essentially the replication of what Mary hardfork can do. but leaves out some more complex things
mkPolicy :: PubKeyHash -> Slot -> ScriptContext -> Bool
mkPolicy pkh deadline ctx = traceIfFalse "Transaction not signed" checkIfSigned         &&
                            traceIfFalse "Deadline has passed"    checkDeadline
         where
		     info :: TxInfo -- doesn't seem like we need TxInfo
             info = scriptContextTxInfo ctx

             checkIfSigned :: Bool
             checkIfSigned = txSignedBy info pkh --PubKeyHash pkh `elem` txInfoSignatories info

             checkDeadline :: Bool
             checkDeadline = to (deadline) `contains` txInfoValidRange info

policy :: PubKeyHash -> Slot -> Scripts.MonetaryPolicy
policy pkh deadline = mkMonetaryPolicyScripts $
                    $$(PlutusTx.compile[|| \pkh' deadline' -> Scripts.wrapMonetaryPolicy $ mkPolicy pkh' deadline' ||]) -- worried that they might want pkh'
                    `PlutusTx.applyCode`
                    PlutusTx.liftCode pkh
                    `Plutus.applyCode`
                    PlutusTx.liftCode deadline
					

curSymbol :: PubKeyHash -> Slot -> CurrencySymbol
curSymbol pkh deadline = scriptCurrencySymbol $ policy pkh deadline

data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpDeadline  :: !Slot
    , mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type SignedSchema =
    BlockchainActions
        .\/ Endpoint "mint" MintParams

mint :: MintParams -> Contract w SignedSchema Text ()
mint mp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    now <- Contract.currentSlot
    let deadline = mpDeadline mp
    if now > deadline
        then Contract.logError @String "deadline passed"
        else do
            let val     = Value.singleton (curSymbol pkh deadline) (mpTokenName mp) (mpAmount mp)
                lookups = Constraints.monetaryPolicy $ policy pkh deadline
                tx      = Constraints.mustForgeValue val <> Constraints.mustValidateIn (to deadline) --  Uses a slot interval to check within that interval
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () SignedSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = endpoint @"mint" >>= mint

mkSchemaDefinitions ''SignedSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn       = "ABC"
        deadline = 10
    h <- activateContractWallet (Wallet 1) endpoints
    callEndpoint @"mint" h $ MintParams
        { mpTokenName = tn
        , mpDeadline  = deadline
        , mpAmount    = 555
        }
    void $ Emulator.waitNSlots 15 -- Second should fail! Because it is way past the deadline.
    callEndpoint @"mint" h $ MintParams
        { mpTokenName = tn
        , mpDeadline  = deadline
        , mpAmount    = 555
        }
    void $ Emulator.waitNSlots 1