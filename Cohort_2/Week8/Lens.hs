{-# LANGUAGE TemplateHaskell #-}

module Week08.Lens where

import Control.Lens

newtype Company = Company {_staff :: [Person]} deriving Show


data Person  = Person
    { _name    :: String
    , _address :: Address
    } deriving Show

newtype Address = Address {_city :: String} deriving Show

alejandro, lars :: Person
alejandro = Person
  {  _name    = "Alejandro"
  ,  _address = Address {_city = "Zacateca"}
  }
lars = Person
  {  _name    = "Lars"
  ,  _address = Address {_city = "Regensburg"}
  }

iohk :: Company
iohk = Company { _staff = [alejandro, lars] }

goTo :: String -> Company -> Company
goTo there c = c {_staff = map movePerson (_staff c)}
  where
    movePerson p = p {_address = (_address p) {_city = there}}

makeLenses ''Company
makeLenses ''Person
makeLenses ''Address

goTo' :: String -> Company -> Company
goTo' there c = c & staff . each . address . city .~ there


-- lars ^. name -> "Lars"
-- lars ^. address -> Address {_city = "regensburg"}
-- lars ^. address . city -> "Regensburg"
-- Lens allow you to zoom in on things within haskell
-- lars & name .~ "LARS" -> Person {_name = "LARS"}
-- Using a list of integers [1 :: Int, 3, 4] & each .~42 sets -> [42,42,42]
-- iohk & staff . each . address . city .~ "Athens" -> Changes everyones city into Athens

{-
Quick check automatic testing of haskell programs

-}