{-# LANGUAGE TemplateHaskell #-}
module Season where

import Test.QuickCheck

data Season = Winter | Spring | Summer | Fall
  deriving (Eq, Show)

next :: Season -> Season
next Winter  =  Spring
next Spring  =  Summer
next Summer  =  Fall
next Fall    =  Winter

eqSeason :: Season -> Season -> Bool
eqSeason Winter Winter  =  True
eqSeason Spring Spring  =  True
eqSeason Summer Summer  =  True
eqSeason Fall   Fall    =  True
eqSeason x      y       =  False

showSeason :: Season -> String
showSeason Winter  =  "Winter"
showSeason Spring  =  "Spring"
showSeason Summer  =  "Summer"
showSeason Fall    =  "Fall"

toInt :: Season -> Int
toInt Winter  =  0
toInt Spring  =  1
toInt Summer  =  2
toInt Fall    =  3

fromInt :: Int -> Season
fromInt 0  =  Winter
fromInt 1  =  Spring
fromInt 2  =  Summer
fromInt 3  =  Fall

next' :: Season -> Season
next' x  =  fromInt ((toInt x + 1) `mod` 4)

eqSeason' :: Season -> Season -> Bool
eqSeason' x y  =  (toInt x == toInt y)

-- QuickCheck tests
prop_eqSeason :: Season -> Season -> Bool
prop_eqSeason x y  =  eqSeason x y == (x == y)

prop_showSeason :: Season -> Bool
prop_showSeason x  =  showSeason x == show x

prop_next' :: Season -> Bool
prop_next' x  =  next x == next' x

prop_eqSeason' :: Season -> Season -> Bool
prop_eqSeason' x y  =  eqSeason x y == eqSeason' x y

-- tell QuickCheck how to generate random values of type Bool
instance Arbitrary Season where
  arbitrary = oneof [return Winter, return Spring, return Summer, return Fall]

-- ** Automatically run QuickCheck properties named 'prop_*' and any other tests.
return []

main :: IO ()
main = do
  qc <- $quickCheckAll
  putStrLn ("QuickCheck: " ++ if qc then "Pass" else "Fail")
