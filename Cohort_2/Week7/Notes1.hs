module Test where

--State machine

{-

Alice
    sends a nonce
    hash(noncell0) 27a9ab
Bob 
    sends 0
nonce, 0
    hash(nonce || 0)?= 27a9ab

-}

--What does this look like?
{-
Alice 
    Opens the game by posting the hash of her nonce combined with the choice she makes to play -- Hash
Bob
    decides to play along and his own choice -> hash,cBob

Depening of if Alice wins she can reveal her spot  and Wins
If she lost, then she does nothing and doesn't reply Bob would claim his win

Alice can get her own money back if Bob never replies
-}
--Doesnt necessarily need to be a state machine to function
-- state of the state machine is the datum, transition is the consuming of the UTXO