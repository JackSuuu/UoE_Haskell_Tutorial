{-# LANGUAGE TemplateHaskell #-}
module Bool where

import Prelude hiding (Bool(True, False), (&&), (||), not)
import qualified Prelude
import Test.QuickCheck

data Bool = False | True
  deriving (Eq, Show)

not :: Bool -> Bool
not False  =  True
not True   =  False

(&&) :: Bool -> Bool -> Bool
False && q  =  False
True && q   =  q

(||) :: Bool -> Bool -> Bool
False || q  =  q
True || q   =  True

eqBool :: Bool -> Bool -> Bool
eqBool False False  =  True
eqBool False True   =  False
eqBool True  False  =  False
eqBool True  True   =  True

showBool :: Bool -> String
showBool False  =  "False"
showBool True   =  "True"

-- QuickCheck tests
bool :: Bool -> Prelude.Bool
bool True  = Prelude.True
bool False = Prelude.False

prop_not :: Bool -> Prelude.Bool
prop_not x  =  (bool (not x)) Prelude.== (Prelude.not (bool x))

prop_and :: Bool -> Bool -> Prelude.Bool
prop_and x y  =  (bool (x && y)) Prelude.== ((bool x) Prelude.&& (bool y))

prop_or :: Bool -> Bool -> Prelude.Bool
prop_or x y  =  (bool (x || y)) Prelude.== ((bool x) Prelude.|| (bool y))

prop_eq :: Bool -> Bool -> Prelude.Bool
prop_eq x y  =  (x == y) Prelude.== ((bool x) Prelude.== (bool y))

prop_eqBool :: Bool -> Bool -> Prelude.Bool
prop_eqBool x y  =  (x == y) Prelude.== bool (eqBool x y)

prop_show :: Bool -> Prelude.Bool
prop_show x  =  (show x) Prelude.== (Prelude.show (bool x))
                
prop_showBool :: Bool -> Prelude.Bool
prop_showBool x  =  (show x) Prelude.== showBool x

-- tell QuickCheck how to generate random values of type Bool
instance Arbitrary Bool where
  arbitrary = oneof [return True, return False]

-- ** Automatically run QuickCheck properties named 'prop_*' and any other tests.
return []

main :: IO ()
main = do
  qc <- $quickCheckAll
  putStrLn ("QuickCheck: " ++ if qc then "Pass" else "Fail")
