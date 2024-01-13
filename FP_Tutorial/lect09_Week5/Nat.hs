{-# LANGUAGE TemplateHaskell #-}
module Nat where

import Prelude hiding ((+), (*), (^))
import qualified Prelude
import Test.QuickCheck
import Control.Monad

data Nat = Zero
         | Succ Nat
         deriving (Eq, Show)

(+) :: Nat -> Nat -> Nat
Zero + n      =  n
(Succ m) + n  =  Succ (m + n)

(*) :: Nat -> Nat -> Nat
Zero * n      =  Zero
(Succ m) * n  =  (m * n) + n

(^) :: Nat -> Nat -> Nat
m ^ Zero      =  Succ Zero
m ^ (Succ n)  =  (m ^ n) * m

-- QuickCheck tests
int :: Nat -> Int
int Zero      =  0
int (Succ n)  =  int n Prelude.+ 1

prop_add :: Nat -> Nat -> Bool
prop_add m n  =  int (m + n) == (int m Prelude.+ int n)

prop_mul :: Nat -> Nat -> Bool
prop_mul m n  =  int (m * n) == (int m Prelude.* int n)

prop_exp :: Nat -> Nat -> Bool
prop_exp m n  =  int (m ^ n) == (int m Prelude.^ int n)

prop_eq :: Nat -> Nat -> Bool
prop_eq m n  =  (m == n) == (int m Prelude.== int n)

-- tell QuickCheck how to generate random values of type Nat
instance Arbitrary Nat where
  arbitrary = oneof [ return Zero, liftM Succ arbitrary ]

-- ** Automatically run QuickCheck properties named 'prop_*' and any other tests.
return []

main :: IO ()
main = do
  qc <- $quickCheckAll
  putStrLn ("QuickCheck: " ++ if qc then "Pass" else "Fail")
