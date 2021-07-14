-- Writing basic PlutusTX programs
{-

-- Necessary language extensions for the Plutus Tx compiler to work.
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tutorial.PlutusTx where

import Language.PlutusTx            -- This is the main plutus tx module
import Language.PlutusTx.Lift       -- Additional support for lifting functions
import Language.PlutusTx.Builtins   -- Builtin Functions

import Language.PlutusCore
import Language.PlutusCore.Pretty
import Language.PlutusCore.Quote
import Language.PlutusCore.Evaluation.CkMachine
import Data.Text.Prettyprint.Doc    -- Used for examples

-}

--The simplest bit of code we can write: evaluates to the integer 1
{-
integerOne :: CompiledCode Integer
integerOne = $$(compile ③ ④
  [|| ②
  (1 :: Integer) ①
  ||])

1) We always use unbounded integers in Plutus Core, so we have to pin down this numeric literal to an integer rather than an INT
2) The quote has type TExpQ Integer
3) Compile turns the TExpQ Integer into a TExpQ (CompiledCode Integer)
4) The splice inserts the TExpQ (CompiledCode Integer into the program)
-}

--Slightly more complex program, the identify function on integers
{-
integerIdentity :: CompiledCode (Integer -> Integer)
integerIdentity = $$(compile [|| \(x:: Integer) -> x ||])
-- Basically compiling a lambda into a lambda
-}