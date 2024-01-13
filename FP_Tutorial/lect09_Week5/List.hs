{-# LANGUAGE TemplateHaskell #-}
module List where

import Prelude hiding ((++), map, filter, foldr)
import qualified Prelude
import Test.QuickCheck

data  List a  =  Nil
              |  Cons a (List a)
  deriving (Eq, Show)

(++) :: List a -> List a -> List a
Nil ++ ys          =  ys
(Cons x xs) ++ ys  =  Cons x (xs ++ ys)

map :: (a -> b) -> List a -> List b
map f Nil          =  Nil
map f (Cons x xs)  =  Cons (f x) (map f xs)

filter :: (a -> Bool) -> List a -> List a
filter p Nil                      =  Nil
filter p (Cons x xs) | p x        =  Cons x (filter p xs)
                     | otherwise  =  filter p xs

foldr :: (a -> b -> b) -> b -> List a -> b
foldr f u Nil          =  u
foldr f u (Cons x xs)  =  x `f` (foldr f u xs)

-- QuickCheck tests
list :: [a] -> List a
list []      =  Nil
list (x:xs)  =  Cons x (list xs)

prop_append :: [Int] -> [Int] -> Bool
prop_append xs ys  =  (list xs ++ list ys) == list (xs Prelude.++ ys)

prop_map :: (Char -> Int) -> String -> Bool
prop_map f xs  =  (map f (list xs)) == list (Prelude.map f xs)

prop_filter :: (Char -> Bool) -> String -> Bool
prop_filter p xs  =  (filter p (list xs)) == list (Prelude.filter p xs)

prop_foldr :: (Char -> Int -> Int) -> Int -> String -> Bool
prop_foldr f u xs  =  (foldr f u (list xs)) == Prelude.foldr f u xs

-- Show instance for functions
instance Show (a -> b) where
   show f = "cannot show functions"

-- ** Automatically run QuickCheck properties named 'prop_*' and any other tests.
return []

main :: IO ()
main = do
  qc <- $quickCheckAll
  print qc

