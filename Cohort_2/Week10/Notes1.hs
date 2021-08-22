module Test where

-- Various operations
{-

TX which creates the factory, contains NFT which identifies the factory, datum contains all of the liq pools

Alice has liq of A,1000 and B,2000
    Chooses the create redeemer for the factory NFT UTXO
    2 inputs Liq 1000A and 2000B and the uniswap factory eutxo
    outputs Pool AB which holds the Liq and the AB NFT 1415 == ( 1000*2000 )^1/2
        Factory Uniswap NFT
        Alice's 1415 AB NFT tokens

Bob decides to swap 100 A with the AB pool
    2 inputs, 100A and the Pool with swap redeemer
    2 outputs, 181 B and the updated pool, 1100A and the 1819B, AB NFT and the 1415 Liq tokens

Charlie 400A and 800B adding liq to Pool AB
    Output: Pool AB 1500A, 2619B and AB NFT @ 1982
        Charlie with 567AB alice with 1415AB

Alice removes liq tokens, all of her 1415 tokens
    Pool AB = 430 ADA, 750B and ABNFT @ 567.
    Alice gets 1070A and 1869B
    Charlie still has 567 AB
    Formula for Alice's tokens is basically almost proportional
Closing the Factory UTXO
    Charlie receieves the remaining 430A and 750B and all AB tokens are burned.

For each pool an identical coin is created essentially and NFT though.

-}