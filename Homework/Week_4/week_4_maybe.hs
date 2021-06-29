import Text.Read (readMaybe)

readEither :: Read a => String -> Either String a
readEither s =case readMaybe s of
    Nothing -> Left $ "can't parse : " ++ s
    Just a -> Right a
{-
foo :: String -> String -> String -> Maybe Int
foo x y z = case readMaybe x of
    Nothing -> Nothing
    Just k -> case readMaybe y of
         Nothing -> Nothing
         Just l -> case readMaybe z of
              Nothing  -> Nothing
              Just m -> Just (k+l+m)
-- 3 Patterns possibly inefficient , we just want to check for any failures then add 3 strings together,
-- So how do we abstract?

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _ = Nothing
bindMaybe (Just x) f = f x

foo' :: String -> String -> String -> Maybe Int
foo' x y z = readMaybe x `bindMaybe` \k ->
             readMaybe y `bindMaybe` \l ->
             readMaybe z `bindMaybe` \m ->
             Just (l+k+m)
-}

foo :: String -> String -> String -> Either String Int
foo x y z = case readEither x of
    Left err -> Left err
    Right k -> case readEither y of
         Left err -> Left err
         Right l -> case readEither z of
              Left err -> Left err
              Right m -> Right (k+l+m)

bindEither :: Either String a -> (a -> Either String b) -> Either String b
bindEither (Left err) _ = Left err
bindEither (Right x) f = f x

foo' :: String -> String -> String -> Either String Int
foo' x y z = readEither x `bindEither` \k ->
             readEither y `bindEither` \l ->
             readEither z `bindEither` \m ->
             Right (l+k+m)

foo'' :: String -> String -> String -> Maybe Int
foo'' x y z = threeInts (readMaybe x) (readMaybe y) (readMaybe z)