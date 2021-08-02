{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week06.Oracle.Funds
    ( ownFunds
    , ownFunds'
    ) where

import           Control.Monad    hiding (fmap)
import qualified Data.Map         as Map
import           Data.Monoid      (Last (..))
import           Data.Text        (Text)
import           Plutus.Contract  as Contract
import           PlutusTx.Prelude hiding ((<$>))
import           Prelude          (Show (..), String, (<$>))
import           Ledger           hiding (singleton)
import           Ledger.Value     as Value

ownFunds :: Contract w s Text Value -- Looks up own pubkey and all utxos at the address and then adds them togetther
ownFunds = do
    pk    <- ownPubKey
    utxos <- utxoAt $ pubKeyAddress pk
    let v = mconcat $ Map.elems $ txOutValue . txOutTxOut <$> utxos -- Combines all outputs into values, v is sum of all values of the UTXOs
    logInfo @String $ "own funds: " ++ show (Value.flattenValue v)
    return v

ownFunds' :: Contract (Last Value) Empty Text ()
ownFunds' = do
    handleError logError $ ownFunds >>= tell . Last . Just -- Runs forever and checks value at every slot
    void $ Contract.waitNSlots 1
    ownFunds'