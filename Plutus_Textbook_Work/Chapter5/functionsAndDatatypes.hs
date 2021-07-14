-- Functions and datatypes
{-
{-# INLINABLE plusOne #-} ②
plusOne :: Integer -> Integer
plusOne x = x `addInteger` 1 ①
{-# INLINABLE myProgram #-} ②
myProgram :: Integer
myProgram =
  let
  plusOneLocal :: Integer -> Integer
  plusOneLocal x = x `addInteger` 1 ①
  localPlus = plusOneLocal 1
  externalPlus = plusOne 1
  in localPlus `addInteger` externalPlus ①
functions :: CompiledCode Integer
functions = $$(compile [|| myProgram ||])

1) addInteger comes from Langauge.PlutusTx.Builtins and is which is mapped to the builtin integer addition in plutus core
2) Functions for reuse are marked with GHC's INLINABLE pragma. this is usually necessary for non-local functions to be usable in Plutus TX blocks, as it instructs GHC 
    to keep the information that the plutusTx compilers needs. While this is not always nececssary, it is a good idea to simply mark all such functions as INLINABLE


--We can use normal haskell datatypes and pattern matching freely
matchMaybe :: CompiledCode (Maybe Integer -> Integer)
matchMaybe = $$(compile [|| \(x:: Maybe Integer) -> case x of
  Just n -> n
  Nothing -> 0
  ||])


-}

-- Example of a datatype representing an open ended-end date
{-

-- | Either a specific end date, or "never".
data EndDate = Fixed Integer | Never

-- | Check whether a given time is past the end date.
pastEnd :: CompiledCode (EndDate -> Integer -> Bool)
pastEnd = $$(compile [|| \(end::EndDate) (current::Integer) -> case end of
  Fixed n -> n `lessThanEqInteger` current
  Never -> False
  ||])

-}