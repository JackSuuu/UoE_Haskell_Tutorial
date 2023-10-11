{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use foldr" #-}
module Tutorial4 where

import Data.Char
import Data.List
import Test.QuickCheck
import Data.Ratio
import GHC.Parser.Lexer (xset)


-- 1. doubles
-- a.
doublesComp :: [Int] -> [Int]
doublesComp xs = [x + x | x <- xs]

-- b.
doublesRec :: [Int] -> [Int]
doublesRec [] = []
doublesRec (x:xs) = (x + x) : doublesRec xs

-- c.
doublesHO :: [Int] -> [Int]
doublesHO lis = map (\x -> x + x) lis

-- d.
prop_doubles :: [Int] -> Bool
prop_doubles lis = (doublesComp lis == doublesRec lis) && (doublesRec lis == doublesHO lis) && (doublesComp lis == doublesHO lis)

-- 2. aboves
-- a.
abovesComp :: Int -> [Int] -> [Int]
abovesComp num lis = [x | x <- lis, x > num]

-- b.
abovesRec :: Int -> [Int] -> [Int]
abovesRec num [] = []
abovesRec num (x:xs)| x > num = x : abovesRec num xs
                    | otherwise = abovesRec num xs

-- c.
abovesHO :: Int -> [Int] -> [Int]
abovesHO num lis = filter (> num) lis

-- d.
prop_aboves :: Int -> [Int] -> Bool
prop_aboves num lis = (abovesComp num lis == abovesRec num lis) && (abovesComp num lis == abovesHO num lis) && (abovesRec num lis == abovesHO num lis)

-- 3. parity
-- a.
xor :: Bool -> Bool -> Bool
xor a b | a == b = False
        | otherwise = True

-- b.
parityRec :: [Bool] -> Bool
parityRec [] = True
parityRec (x:xs) = x `xor` parityRec xs

-- c.
parityHO :: [Bool] -> Bool
parityHO bool_list = foldr xor True bool_list

-- d.
prop_parity :: [Bool] -> Bool
prop_parity bool_list =  parityRec bool_list == parityHO bool_list

-- 4. allcaps
-- a.
allcapsComp :: String -> Bool
allcapsComp char_list = and [isUpper x | x <- char_list, isAlpha x]
                    
-- b.
allcapsRec :: String -> Bool
allcapsRec [] = True
allcapsRec (x:xs)| isAlpha x = isUpper x && allcapsRec xs
                 | otherwise = allcapsRec xs

-- c.
allcapsHO :: String -> Bool
allcapsHO =  undefined

-- d.
prop_allcaps :: String -> Bool
prop_allcaps =  undefined


-- ** Optional material
-- Matrix manipulation

type Matrix = [[Rational]]

-- 5
-- a.
uniform :: [Int] -> Bool
uniform = undefined

-- b.
valid :: Matrix -> Bool
valid = undefined


-- 6.
width :: Matrix -> Int
width m = undefined

height :: Matrix -> Int
height m = undefined

plusM :: Matrix -> Matrix -> Matrix
plusM = undefined

-- 7.
timesM :: Matrix -> Matrix -> Matrix
timesM = undefined


-- ** Challenge

-- 8.
-- Mapping functions
mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f = undefined

zipMatrix :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrix f = undefined

-- All ways of deleting a single element from a list
removes :: [a] -> [[a]]
removes = undefined

-- Produce a matrix of minors from a given matrix
minors :: Matrix -> [[Matrix]]
minors m = undefined

-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Int -> Int -> Matrix
signMatrix w h = undefined

determinant :: Matrix -> Rational
determinant = undefined

cofactors :: Matrix -> Matrix
cofactors m = undefined

scaleMatrix :: Rational -> Matrix -> Matrix
scaleMatrix k = undefined

inverse :: Matrix -> Matrix
inverse m = undefined

-- Tests
identity :: Int -> Matrix
identity n = undefined

prop_inverse1 :: Rational -> Property
prop_inverse1 a = undefined

prop_inverse2 :: Rational -> Rational -> Rational
                -> Rational -> Property
prop_inverse2 a b c d = undefined

type Triple a = (a,a,a)

prop_inverse3 :: Triple Rational ->
                 Triple Rational ->
                 Triple Rational ->
                 Property
prop_inverse3 r1 r2 r3 = undefined
