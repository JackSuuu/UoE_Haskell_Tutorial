{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use intercalate" #-}
{-# HLINT ignore "Move guards forward" #-}
module Tutorial9 where

-- Sudoku solver
-- Based on Bird, "Thinking Functionally with Haskell"

import Data.List (sort,nub,(\\),transpose,genericLength)
import Data.String (lines,unlines)
import Test.QuickCheck
import Turtle (x)

-- Representing Sudoku puzzles

type Row a    = [a]
type Matrix a = [Row a]
type Digit    = Char

-- Examples from websudoku.com

easy :: Matrix Digit
easy = ["    345  ",
        "  89   3 ",
        "3    2789",
        "2 4  6815",
        "    4    ",
        "8765  4 2",
        "7523    6",
        " 1   79  ",
        "  942    "]

medium :: Matrix Digit
medium = ["   4 6 9 ",
          "     3  5",
          "45     86",
          "6 2 74  1",
          "    9    ",
          "9  56 7 8",
          "71     64",
          "3  6     ",
          " 6 9 2   "]

hard :: Matrix Digit
hard = ["9 3  42  ",
        "4 65     ",
        "  28     ",
        "     5  4",
        " 67 4 92 ",
        "1  9     ",
        "     87  ",
        "     94 3",
        "  83  6 1"]

evil :: Matrix Digit
evil = ["  9      ",
        "384   5  ",
        "    4 3  ",
        "   1  27 ",
        "2  3 4  5",
        " 48  6   ",
        "  6 1    ",
        "  7   629",
        "     5   "]

-- Another example, from Bird's book

book :: Matrix Digit
book = ["  4  57  ",
        "     94  ",
        "36      8",
        "72  6    ",
        "   4 2   ",
        "    8  93",
        "4      56",
        "  53     ",
        "  61  9  "]

-- Printing Sudoku puzzles

group :: [a] -> [[a]]
group = groupBy 3

groupBy :: Int -> [a] -> [[a]]
groupBy n [] = []
groupBy n xs = take n xs : groupBy n (drop n xs)

intersperse :: a -> [a] -> [a]
intersperse sep []     = [sep]
intersperse sep (y:ys) = sep : y : intersperse sep ys

showRow :: String -> String
showRow = concat . intersperse "|" . group

showGrid :: Matrix Digit -> [String]
showGrid = showCol . map showRow
  where
    showCol = concat . intersperse [bar] . group
    bar     = replicate 13 '-'

put :: Matrix Digit -> IO ()
put = putStrLn . unlines . showGrid

-- 1.
choice :: Digit -> [Digit]
choice dig
        | dig == ' ' = ['1'..'9']
        | otherwise = [dig]

choices :: Matrix Digit -> Matrix [Digit]
choices = map (map choice)
-- choices = map (\ r -> [choice x | x <- r])

-- 2.
splits :: [a] -> [(a, [a])]
splits xs  =
  [ (xs!!k, take k xs ++ drop (k+1) xs) | k <- [0..n-1] ]
  where
  n = length xs

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = remove (splits row)
        where
                remove [] = []
                remove (y:ys)  = rem y : remove ys
                        where
                                rem (x,xs) = [l | l <- x, not (any (elem l) [b | b <- xs, length b == 1])]


-- this code builds on pruneRow to also prune columns and boxes

pruneBy :: (Matrix [Digit] -> Matrix [Digit]) -> Matrix [Digit] -> Matrix [Digit]
pruneBy f = f . map pruneRow . f

rows, cols, boxs :: Matrix a -> Matrix a
rows = id
cols = transpose
boxs = map ungroup . ungroup . map cols . group . map group
  where
    ungroup :: Matrix a -> [a]
    ungroup = concat

prune :: Matrix [Digit] -> Matrix [Digit]
prune = pruneBy boxs . pruneBy cols . pruneBy rows

-- 3.
dec :: Int -> Int
dec a = 20 - a

close :: Eq a => (a -> a) -> a -> a
close g x = clo g (g x) x
        where
                clo g curr prev
                        | curr == prev = curr
                        | otherwise = clo g (g curr) curr


-- 4.
extract :: Matrix [Digit] -> Matrix Digit
extract matrix
        | and [and [length c == 1 | c <- x] | x <- matrix] = [concat x | x <- matrix]
        | otherwise = undefined

-- 5.
solve :: Matrix Digit -> Matrix Digit
solve m = extract (close prune (choices m))


-- ** Optional Material

-- 6.
failed :: Matrix [Digit] -> Bool
failed m = or [check row | row <- m]
        where
                check row = or [r == "" | r <- row]

-- 7.
solved :: Matrix [Digit] -> Bool
solved m = and [one row | row <- m]
        where
                one row = and [length r == 1 | r <- row]

--------------------------------------------------------------------------
-- 8.
shortest :: Matrix [Digit] -> Int
shortest m = minimum [min row | row <- m]
        where
                min row = minimum [length r | r <- row, length r > 1]

-- 9.
expand :: Matrix [Digit] -> [Matrix [Digit]]
expand mat
    | not (solved mat) = [ preMat ++ [preRow ++ [[d']] ++ postRow] ++ postMat | d' <- ds ]
    | otherwise = [[[]]]
        where
            (preMat, row:postMat) =  break (any p) mat
            (preRow, ds:postRow)  =  break p row
            p = (== sm) . length
            sm = shortest mat

-- 10.
search :: Matrix Digit -> [Matrix Digit]
search mat = search' (close prune (choices mat))
        where 
                search' :: Matrix [Digit] -> [Matrix Digit]
                search' mat
                        | solved mat = [extract mat]
                        | failed mat = [[]]
                        | otherwise = concat [ search' (close prune e) | e <- expand mat ]


-- display puzzle and then solution(s) found by search

puzzle :: Matrix Digit -> IO ()
puzzle g = put g >> puts (search g) >> putStrLn "***"
     where puts = sequence_ . map put

main :: IO ()
main = puzzle easy >>
       puzzle medium >>
       puzzle hard >>
       puzzle evil

{-
-------------
|   | 34|5  |
|  8|9  | 3 |
|3  |  2|789|
-------------
|2 4|  6|815|
|   | 4 |   |
|876|5  |4 2|
-------------
|752|3  |  6|
| 1 |  7|9  |
|  9|42 |   |
-------------

-------------
|927|834|561|
|168|975|234|
|345|162|789|
-------------
|234|796|815|
|591|248|673|
|876|513|492|
-------------
|752|389|146|
|413|657|928|
|689|421|357|
-------------

***
-------------
|   |4 6| 9 |
|   |  3|  5|
|45 |   | 86|
-------------
|6 2| 74|  1|
|   | 9 |   |
|9  |56 |7 8|
-------------
|71 |   | 64|
|3  |6  |   |
| 6 |9 2|   |
-------------

-------------
|837|456|192|
|296|183|475|
|451|729|386|
-------------
|682|374|951|
|175|298|643|
|943|561|728|
-------------
|719|835|264|
|328|647|519|
|564|912|837|
-------------

***
-------------
|9 3|  4|2  |
|4 6|5  |   |
|  2|8  |   |
-------------
|   |  5|  4|
| 67| 4 |92 |
|1  |9  |   |
-------------
|   |  8|7  |
|   |  9|4 3|
|  8|3  |6 1|
-------------

-------------

-------------
|983|674|215|
|416|532|879|
|752|891|346|
-------------
|839|725|164|
|567|143|928|
|124|986|537|
-------------
|391|468|752|
|675|219|483|
|248|357|691|
-------------

-------------

-------------

-------------

***
-------------
|  9|   |   |
|384|   |5  |
|   | 4 |3  |
-------------
|   |1  |27 |
|2  |3 4|  5|
| 48|  6|   |
-------------
|  6| 1 |   |
|  7|   |629|
|   |  5|   |
-------------

Tutorial9.hs: Prelude.minimum: empty list
-}