module Tutorial3 where

import Data.Char
import Data.List
import Test.QuickCheck
import Text.Read (Lexeme(String))


-- These are some helper functions for makeKey and makeKey itself.
-- Exercises continue below.

rotate :: Int -> [Char] -> [Char]
rotate k list | 0 <= k && k <= length list = drop k list ++ take k list
              | otherwise = error "Argument to rotate too large or too small"

--  prop_rotate rotates a list of lenght l first an arbitrary number m times,
--  and then rotates it l-m times; together (m + l - m = l) it rotates it all
--  the way round, back to the original list
--
--  to avoid errors with 'rotate', m should be between 0 and l; to get m
--  from a random number k we use k `mod` l (but then l can't be 0,
--  since you can't divide by 0

prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

alphabet = ['A'..'Z']

makeKey :: Int -> [(Char, Char)]
makeKey k = zip alphabet (rotate k alphabet)

-- ===============================================================================
-- ** Caesar Cipher Exercises

-- 1.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp char char_list = the (nub [y | (x, y) <- char_list, char == x])
      where
            the [] = char
            the [char] = char

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec char [] = char
lookUpRec char ((x,y) : xs)
      | char == x = y
      | otherwise = lookUpRec char xs


prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp x y = lookUp x y == lookUpRec x y

-- 2.
encipher :: Int -> Char -> Char
encipher num char = lookUp char (makeKey num)

-- 3.
normalise :: String -> String
normalise str = [toUpper s | s <- str, isAlpha s]

normaliseRec :: String -> String
normaliseRec [] = "" -- Base case: when string is empty
normaliseRec (s: str)
      | isAlpha s = toUpper s : normaliseRec str
      | otherwise = normaliseRec str

prop_normalise :: String -> Bool
prop_normalise x = normalise x == normaliseRec x

-- 4.
enciphers :: Int -> String -> String
enciphers num str = [encipher num s | s <- normalise str]

-- 5.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey char_list = [(y,x) | (x,y) <- char_list]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec ((x,y): xs) = (y,x) : reverseKeyRec xs

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey lis = reverseKey lis == reverseKeyRec lis

-- 6.
decipher :: Int -> Char -> Char
decipher num char = the [x | (x, y) <- makeKey num, char == y]
      where
            the [char] = char

decipherStr :: Int -> String -> String
decipherStr num str = [decipher num s | s <- str, isAlpha s, isUpper s]


-- ** Optional Material

-- 7.
candidates :: String -> [(Int, String)]
candidates str = [(count, s) | (count, s) <- [(count, decipherStr count str) | count <- [1..26]], isInfixOf "THE" s || isInfixOf "AND" s]

-- ===============================================================================
splitEachFive :: String -> [String]
splitEachFive xs | length xs > 5 = take 5 xs : splitEachFive (drop 5 xs)
                 | otherwise     = [ fillToFive xs ]

fillToFive :: String -> String
fillToFive xs = xs ++ replicate (5 - length xs) 'X'

-- An alternative solution demonstrating 'repeat'
fillToFive' :: String -> String
fillToFive' xs = take 5 (xs ++ repeat 'X')

-- The following example shows why 'transpose' is not
-- invertible in general. The transpose function
-- takes the 'columns' of a list of lists, and makes
-- them the 'rows' of a new list of lists. 
--
-- [[o n e],           [[o t t f f],       [[o n e e e],
--  [t w o],            [n w h o i],        [t w o r],  
--  [t h r e e],   -->  [e o r u v],   -->  [t h r e],  
--  [f o u r],          [e r e],            [f o u], 
--  [f i v e]   ]       [e],        ]       [f i v]     ]   

-- 8.
      -- 1. applying Caesar Cipher
      -- 2. splitting into pieces of length five
      -- 3. tranpose the whole string
      -- 4. putting pieces together
encrypt :: Int -> String -> String
encrypt num str = combine (transpose (splitEachFive (enciphers num str)))
      where 
            combine :: [String] -> String
            combine [] = []
            combine (x:xs) = x ++ combine xs


-- 9.
decrypt :: Int -> String -> String
decrypt num str = decipherStr num (combine (transpose (splitEach str)))
      where 
            combine :: [String] -> String
            combine [] = []
            combine (x:xs) = x ++ combine xs

            -- after tranpose, it always be five groups
            eachLength = length str `div` 5

            splitEach :: String -> [String]
            splitEach [] = []
            splitEach str = take eachLength str : splitEach (drop eachLength str)

