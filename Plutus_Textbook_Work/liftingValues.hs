-- 5.5 Lifting Values

{-

-- Simple addOne function

addOne :: compiledCode (Integer -> Integer)
addOne = $$(compile [|| \(x:: Integer) -> `addInteger` 1 ||])

-}

-- Making it such that we can lift to different numbers
{-

addOneToN :: Integer -> CompiledCode Integer
addOneToN n =
  addOne
  `applyCode` ①
  unsafeLiftCode n ②

1) applyCode applies one CompiledCode to another
2) unsafeLiftCode lifts the argument n into a CompiledCode Integer

-}

-- More complex lifted version
{-

makeLift ''EndDate

:
: >>> pastEndAt Never 5
:

pastEndAt :: EndDate -> Integer -> CompiledCode Bool
pastEndAt end current =
  pastEnd
  `applyCode`
  unsafeLiftCode end
  `applyCode`
  unsafeLiftCode current
--Output is false // Is 5 past the end date of Never? -- NO
-}