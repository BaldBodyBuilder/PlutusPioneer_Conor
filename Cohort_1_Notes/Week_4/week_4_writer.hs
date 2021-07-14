data Writer a = Writer a [String]
     deriving Show

number :: Int -> Writer Int
number n = Writer n $ ["number: " ++ show n]

tell :: [String] -> Writer ()
tell = Writer () 


foo :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo (Writer k xs) (Writer l ys) (Writer m zs) =
  let
    s = k + l + m
    Writer _ us = tell ["sum: " ++ show s]
  in
    Writer s $ xs ++ ys ++ zs ++ us

-- Better way of writing the functions in a more abstract way "Concentrate on the business logic"

bindWriter :: Writer a -> (a -> Writer b) -> Writer b
bindWriter (Writer a xs) f =
    let
      Writer b ys = f a
    in
      Writer b $ xs ++ ys

foo' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo' x y z = x `bindWriter` \k ->
             y `bindWriter` \l ->
             z `bindWriter` \m ->
             let s = k + l + m
             in tell ["sum: " ++ show s] `bindWriter` \_ ->
                  Writer s []


threeInts :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts mx my mz =
     mx >>= \k ->
	 my >>= \l ->
	 mz >>= \m ->
	 let s = k + l + m in return s
--Versus the do notation

threeInts' :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts' mx my mz = do
         k <- mx
         l <- my
         m <- mz
         return $ k + l + m