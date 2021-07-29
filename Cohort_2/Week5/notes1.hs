module Test where

{-
Main thing we're going to be using for generating CNTs:

 Value
    getValue :: Map CurrencySymbol (Map TokenName Integer)
 TokenName 
    unTokenName :: ByteString

:t adaSymbol
    adaSymbol :: CurrencySymbol
    adaToken :: TokenName

Currency symbol of ada is empty
so lovelacevalueof123
    Value (Map [(,Map [("",123)])])


lovelacevalueof 123 <> lovelacevalueof 10
 -->133

 How to create values containing native tokens

:t singleton :: CurrencySymbol -> TokenName -> Integer -> Value

singleton :: CurrencySymbol -> TokenName -> Integer -> Valeu

singleton "a8ff" "ABC" 7
Value (Map [(a8ff,Map [("ABC",123)])])

singleton "a8ff" "ABC" 7 <> lovelaceValueOf 42 <> singleton "a8ff" "XYZ" 100

let v = singleton "a8ff" "ABC" 7 <> lovelaceValueOf 42 <> singleton "a8ff" "XYZ" 100

valueOf v "a8ff" "XYZ"
100

:t flattenValue
flattenValue :: Value -> [(CurrencySymbol, TokenName, Integer)]

flattenValue v
[(a8ff, "ABC",7),(a8ff,"XYZ",100),(,"",42)]

More fees are consumed if more memory thus more lines and more steps for something to happen -- a script

SCript purpose : Minting CurrencySymbol with some TxInfoForge
-}