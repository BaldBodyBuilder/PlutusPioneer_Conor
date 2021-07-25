module Monads where

-- (>>=)      :: IO a            -> (a -> IO b)            -> IO b
-- bindMaybe  :: Maybe a         -> (a -> Maybe b)         -> Maybe b
-- bindEither :: Either String a -> (a -> Either String b) -> Either String b
-- bindWriter :: Writer a        -> (a -> Writer b)        -> Writer b
--
-- return              :: a -> IO a
-- Just                :: a -> Maybe a
-- Right               :: a -> Either String a
-- (\a -> Writer a []) :: a -> Writer a

threeInts :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts mx my mz =
    mx >>= \k ->
    my >>= \l ->
    mz >>= \m ->
    let s = k + l + m in return s
-- DO notation alternative version
threeInts' :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts' mx my mz = do
    k <- mx
    l <- my
    m <- mz
    let s = k + l + m
    return s

{-
Return has the same type sig as pure, pure is just return
Haskell has an overloaded semicolon lmao


-}