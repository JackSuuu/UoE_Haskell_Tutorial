-- Informatics 1 - Functional Programming 
-- Class Test 2023
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse foldr/map" #-}
{-# HLINT ignore "Use sum" #-}

module ClassExam where

import Data.Char
import Test.QuickCheck

-- Problem 1

-- a

f :: String -> Int
f str = sum [ord s | s <- str, isAlpha s]

-- b

g :: String -> Int
g "" = 0
g (x:xs) 
    | isAlpha x = ord x + g xs
    | otherwise = g xs

-- c
-- foldr map filter
h :: String -> Int
h str = foldr (+) 0 (map ord (filter isAlpha str))

-- d

prop_fgh :: String -> Bool
prop_fgh str = (f str == g str) && (f str == h str) && (g str == h str)

-- Problem 2

-- a

c :: String -> String -> Bool
c str_1 str_2 = and [s_1 == s_2 | (s_1, s_2) <- zip str_1 str_2, isAlpha s_1 && isAlpha s_2]

-- b

d :: String -> String -> Bool
d "" ys = True
d xs "" = True
d (x:xs) (y:ys)
    | isAlpha x && isAlpha y = (x == y) && d xs ys
    | otherwise = d xs ys 

-- c

prop_cd :: String -> String -> Bool
prop_cd str_1 str_2 = c str_1 str_2 == d str_1 str_2
