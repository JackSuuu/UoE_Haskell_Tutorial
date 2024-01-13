-- Indexed data represented as a list
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module KeymapList ( Keymap,
                    invariant, keys,
                    size, get, set, del, select,
                    toList, fromList
                  )
where

import Test.QuickCheck 
import Data.List (nub,sort)
import Data.List.Extra (anySame)
import Control.Monad (liftM,liftM2)

data Keymap k a = K [(k,a)]
                deriving (Eq,Show)

-- Checks the invariant that all keys should be unique
invariant :: Eq k => Keymap k a -> Bool
invariant db  = not (anySame (keys db))

-- Returns all the keys in a Keymap
keys :: Keymap k a -> [k]
keys (K xs)   =  map fst xs

-- Gives the number of entries in a Keymap
size :: Keymap k a -> Int
size (K xs) = length xs

-- Given a key, look up the associated value in a Keymap
get :: Eq k => k -> Keymap k a -> Maybe a
get key (K xs) = lookup key xs

-- Given a key and a value, sets the value associated with the key to the given value in a Keymap.
set :: Eq k => k -> a -> Keymap k a -> Keymap k a
set key value (K xs) = K (ins  xs)
    where
      ins [] = [(key,value)]
      ins ((k,v):xs) | k == key  = (k,value) : xs
                     | otherwise = (k,v) : ins xs

-- Deletes an entry with the given key from a keymap.
del :: Eq k => k -> Keymap k a -> Keymap k a
del key (K xs) = K (filter ((/= key) . fst) xs)

-- Narrows a keymap to those values that satisfy a given predicate.
select :: Eq k => (a -> Bool) -> Keymap k a -> Keymap k a
select f (K xs) = K (filter (f . snd) xs)

-- Exports the keymap as a list
toList :: Keymap k a -> [(k,a)]
toList (K xs) = xs

-- Builds the keymap from a list
fromList :: [(k,a)] -> Keymap k a
fromList xs = K xs

-- for QuickCheck
instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
  arbitrary = liftM K (liftM2 zip (liftM nub (listOf arbitrary)) arbitrary)
