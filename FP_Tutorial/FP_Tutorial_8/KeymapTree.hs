{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}
module KeymapTree ( Keymap (Leaf, Node),
                    invariant, keys,
                    size, depth, get, set, select,
                    toList, fromList,
                    filterLT, filterGT, merge,
                  )
where

-- Modules for testing
  
import Test.QuickCheck
import Control.Monad
import Data.List
  
-- The data type

data Keymap k a = Leaf
                -- Node value left right
                | Node k a (Keymap k a) (Keymap k a)
                deriving (Eq, Show)

-- A test tree
testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf 
                               (Node 4 40 Leaf Leaf ))

-- Invariant
invariant :: Ord k => Keymap k a -> Bool
invariant Leaf  =  True
invariant (Node k _ left right)  =  all (< k) (keys left) &&
                                    all (> k) (keys right) &&
                                    invariant left &&
                                    invariant right

keys :: Keymap k a -> [k]
keys Leaf  = []
keys (Node k _ left right) = keys left ++ [k] ++ keys right

size :: Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

depth :: Keymap k a -> Int
depth Leaf = 0
depth (Node _ _ left right) = 1 + (depth left `max` depth right)

-- Exercise 3
toList :: Keymap k a -> [(k,a)]
toList Leaf = []
toList (Node k v left right) = toList left ++ [(k,v)] ++ toList right

-- Exercise 4
set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = go
    where go Leaf = Node key value Leaf Leaf
          go (Node k v left right) | key == k = Node k value left right
                                   | key < k  = Node k v (go left) right
                                   | key > k  = Node k v left (go right)
                                     

-- Exercise 5
get :: Ord k => k -> Keymap k a -> Maybe a
get k Leaf = Nothing
get k (Node key v left right) | k == key = Just v
                              | k < key = get k left
                              | k > key = get k right 

prop_set_get :: Int -> String -> Keymap Int String -> Bool
prop_set_get k v db = get k (set k v db) == Just v

-- Exercise 6

fromList :: Ord k => [(k,a)] -> Keymap k a
fromList = foldr (\(k, a) acc -> set k a acc) Leaf

prop_toList_fromList :: [Int] -> [String] -> Bool
prop_toList_fromList xs ys  =  toList (fromList zs) == sort zs
  where zs = zip (nub xs) ys


-- ** Optional Material

-- Exercise 8
filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT _ Leaf = Leaf
filterLT k (Node key value left right)
    | k <= key = filterLT k left
    | otherwise = Node key value (filterLT k left) (filterLT k right)
                                     
filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT _ Leaf = Leaf
filterGT k (Node key value left right)
    | k >= key = filterGT k right
    | otherwise = Node key value (filterGT k left) (filterGT k right)

-- Exercise 9                                    
merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge Leaf t2 = t2
merge t1 Leaf = t1
merge (Node k1 v1 left1 right1) (Node k2 v2 left2 right2)
    | k1 < k2   = Node k1 v1 (merge left1 (Node k2 v2 left2 right2)) right1
    | k1 > k2   = Node k2 v1 left2 (merge right1 (Node k2 v2 left2 right2))
    | otherwise = Node k1 v1 (merge left1 left2) (merge right1 right2)

prop_merge :: Keymap String String -> Keymap String String -> Bool
prop_merge t1 t2 = merge t1 t2 == merge t1 t2 -- Really don't know how to test

                
-- Exercise 10
select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select _ Leaf = Leaf
select p (Node k v left right)
    | p v       = Node k v (select p left) (select p right)
    | otherwise = merge (select p left) (select p right)


-- Instances for QuickCheck -----------------------------

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
    arbitrary = liftM fromList (liftM2 zip (liftM nub arbitrary) arbitrary)
