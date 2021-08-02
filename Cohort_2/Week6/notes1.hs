module Test where

{-

Writing an Oracle dApp to a mockchain

1 trusted data provider and data feed

ADA to USD.

Oracle is represented by the UTXO
    In its data field it carries the current value of the data provided by the oracle
    Remember validation only happens when we consume

Don't know the API for which the oracle is going to interact with

oracle is going to work with a swap address of ADA to USD

Oracle validator must check that the NFT is at the input

The output must have the same values but still contains the data and NFT, datum does not change.
    Must also be paid, the fee that is.

The value of the oracle or Datum must be able to change,NFT must be in the consumed and produced in the transaction

Provider of the oracle must sign the update

Provider must also be able to take out the fees of the oracle.

-}