module Test where

{-
Writing off-chain plutus code

foo :: Int
foo = ....

... foo ... foo ...
referential transparency, foo is going to be the same value in every single call -- makes refactoring very easy

-}

-- We need sideeffects
{-
foo :: IO Int
foo = ...

main :: IO ()
main = putStrLn "Hello, World!" -- hello world in haskell
-}
-- Functors
-- Only thing thats really important: fmap :: (a -> b) -> f a -> f b
{-
:t fmap (map toUpper) getLine
fmap (map toUpper) getLine :: IO [Char]

-}
{-
>>
chains two IO actions together
f a >> f b
a
b
sequence operator

-- Bind Operator
:t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b

:t return
no side effects simply returns immediately the given a
return :: Monad m => a -> m a
-}

--Writing relatively complex IO actions
{-

main :: IO ()
main = 

bar :: IO ()
bar = getLine >>= \s ->
      getLine >>= \t ->
      putStrLn (s ++ t)

-}
