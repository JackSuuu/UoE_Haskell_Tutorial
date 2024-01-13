module Tutorial2 where

import Data.Char
import Data.List
import Test.QuickCheck
import GHC.Parser.Lexer (xset)


-- 1. inRange
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, x>=lo, x<=hi]


-- 2. multDigits
multDigits :: String -> Int
multDigits str = product [digitToInt s | s <- str, isDigit s]

countDigits :: String -> Int
countDigits str = length [s | s <- str, isDigit s]

prop_multDigits :: String -> Bool
prop_multDigits str = multDigits str <= (9 ^ countDigits str)


-- 3. capitalise and title
capitalise :: String -> String
capitalise [] = []
capitalise (x:xs) = toUpper x : [toLower c | c <- xs]

-- Helper function only capitalise only length > 4
capitaliseLong :: String -> String
capitaliseLong xs | length xs >= 4 = capitalise xs
                  | otherwise = [toLower c | c <- xs]


title :: [String] -> [String]
title words = [capitalise w | w <- words]

-- 4. score and totalScore
vowelScore :: Char -> Int
vowelScore char
  | char `elem` "aeiouAEIOU" = 1
  | otherwise = 0

upperScore :: Char -> Int
upperScore char
  | isUpper char = 1
  | otherwise = 0

score :: Char -> Int
score char
  | isAlpha char = 1 + vowelScore char + upperScore char
  | otherwise = 0


totalScore :: String -> Int
totalScore string = product [score s | s <- string, isAlpha s]

prop_totalScore_pos :: String -> Bool
prop_totalScore_pos xs = totalScore xs >= 1


-- ** Optional Material

-- 5. crosswordFind
-- * letter should be inclueded in the word in pos
-- * length should fit len
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words = [word | word <- words, length word == len, word !! pos == letter]


-- 6. search
search :: String -> Char -> [Int]
search str goal = [index | (s, index) <- zip str [0..length str], s == goal]
    -- [s | s <- str, s == goal]

-- Depending on the property you want to test, you might want to change the type signature
prop_search :: String -> Char -> Bool
prop_search str goal = undefined



