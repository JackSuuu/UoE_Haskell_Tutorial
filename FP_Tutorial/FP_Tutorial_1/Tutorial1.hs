module Tutorial1 where

import PicturesSVG -- needed for the optional chess part
import Test.QuickCheck
-- 

-- 2.
double :: Int -> Int
double x = x + x

square :: Int -> Int
square x = x * x

-- 3.
isTriple :: Int -> Int -> Int -> Bool
isTriple a b c = square a + square b == square c

-- 4.
leg1 :: Int -> Int -> Int
leg1 x y = square x - square y

leg2 :: Int -> Int -> Int
leg2 x y = 2 * x * y

hyp :: Int -> Int -> Int
hyp x y = square x + square y

-- 5.
prop_triple :: Int -> Int -> Bool
prop_triple x y = isTriple (leg1 x y) (leg2 x y) (hyp x y)
-- When x is bigger than y, the output is True, and vice versa
-- if x is smaller than y, the output will be False

-- 8.
pic1 :: Picture
pic1 = above (beside knight (invert knight)) (beside (invert knight) knight)

pic2 :: Picture
pic2 = above (beside knight (invert knight)) (FlipV (beside (invert knight) knight))

-- ** Functions

twoBeside :: Picture -> Picture
twoBeside x = beside x (invert x)

-- 9.
twoAbove :: Picture -> Picture
twoAbove x = above x (invert x)

fourPictures :: Picture -> Picture
fourPictures x = twoBeside (twoAbove x)

-- 10.
-- a)
besideRow :: Picture
besideRow = beside whiteSquare blackSquare

emptyRow :: Picture
emptyRow = repeatH 4 besideRow

-- b)
otherEmptyRow :: Picture
otherEmptyRow = flipV emptyRow

-- c)
twoRow :: Picture
twoRow = above emptyRow otherEmptyRow

middleBoard :: Picture
middleBoard = repeatV 2 twoRow

-- d)
whiteRow :: Picture
whiteRow = beside rook (beside knight (beside bishop (beside queen (beside king (beside bishop (beside knight rook))))))

blackRow :: Picture
blackRow = invert whiteRow

-- e)
whitePawns :: Picture
whitePawns = repeatH 8 pawn

blackPawns :: Picture
blackPawns = invert whitePawns

whiteSum :: Picture
whiteSum = above twoRow (above whiteRow whitePawns)

blackSum :: Picture
blackSum = above blackRow blackPawns

populatedBoard :: Picture
populatedBoard = above (over blackSum middleBoard) (over whiteSum middleBoard)
