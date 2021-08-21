module Test where

-- The Marlowe language
data Contract = Close
              | Pay Party Payee Value Contract
              | If Observation Contract Contract
              | When [Case Action Contract]
                    Timeout Contract
              | Let ValueId Value Contract
              | Assert Observation Contract

--Marlowe Suite
{-
marlow-finance.io
Run -- DApp end users obtain and run contracts distributed
Market Contracts up and down loaded with assurances
Play contracts to be sim'd interactively
Build contracts built in code visually and embedded
-}
