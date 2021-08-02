{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}

module Week06.Oracle.PAB
    ( OracleContracts (..)
    ) where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import           GHC.Generics              (Generic)
import           Ledger

import qualified Week06.Oracle.Core        as Oracle

data OracleContracts = Init | Oracle CurrencySymbol | Swap Oracle.Oracle
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

-- Init is for testing and setting up testing environment
-- Oracle CurrencySym for defining the oracle
-- Swap

-- Boilerplate



instance Pretty OracleContracts where
    pretty = viaShow