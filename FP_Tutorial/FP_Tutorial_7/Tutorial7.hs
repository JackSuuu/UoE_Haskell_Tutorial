{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Tutorial7 where

import LSystem
import Test.QuickCheck
import Turtle (x)

pathExample = Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#: Go 30
pathTest = (Go 50.0 :#: Turn 72.0) :#: (Go 50.0 :#: Turn 72.0)

-- 1a. copy
copy :: Int -> Command -> Command
copy 0 c = Sit
copy int commands = commands :#: copy (int - 1) commands

-- 1b. polygon
polygon :: Distance -> Int -> Command
polygon dis int = copy int (Go dis :#: Turn 72)

-- 2. snowflake
snowflake :: Int -> Command
snowflake x = f x :#: p :#: p :#: f x :#: p :#: p :#: f x :#: p :#: p
    where
        f 0 = Go 10
        f x = f (x - 1) :#: n :#: f (x - 1) :#: p :#: p :#: f (x - 1) :#: n :#: f (x - 1)
        p = Turn 60
        n = Turn (-60)

-- 3. sierpinski
sierpinski :: Int -> Command
sierpinski x = f x
    where
        f 0 = Go 10
        f x = g (x - 1) :#: n :#: f (x - 1) :#: n :#: g (x - 1)
        g 0 = GrabPen blue :#: Go 10
        g x = f (x - 1) :#: p :#: g (x - 1) :#: p :#: f (x - 1)
        p = Turn 60
        n = Turn (-60)

-- 4. hilbert
hilbert :: Int -> Command
hilbert x = l x
    where
        f 0 = Go 10
        f _ = Sit
        l 0 = Sit
        l x = pos :#: r (x-1) :#: f (x-1) :#: neg :#: l (x-1) :#: f (x-1) :#: l (x-1) :#: neg :#: f (x-1) :#: r (x-1) :#: pos
        r 0 = Sit
        r x = neg :#: l (x-1) :#: f (x-1) :#: pos :#: r (x-1) :#: f (x-1) :#: r (x-1) :#: pos :#: f (x-1) :#: l (x-1) :#: neg
        neg = Turn 90
        pos = Turn (-90)

-- 5. dragon
dragon :: Int -> Command
dragon x =  l x
    where
        f 0 = Go 10
        f _ = Sit
        l 0 = Sit
        l x = l (x-1) :#: pos :#: r (x-1) :#: f (x-1) :#: pos
        r 0 = Sit
        r x = neg :#: f (x-1) :#: l (x-1) :#: neg :#: r (x-1)
        neg = Turn 90
        pos = Turn (-90)

-- ** Optional Material

-- 6a. split
split :: Command -> [Command]
split (command1 :#: command2) = split command1 ++ split command2
split Sit = []
split command = [command]

-- 6b. join
join :: [Command] -> Command
join commands = foldr (:#:) Sit commands

-- 6c. equivalent
equivalent :: Command -> Command -> Bool
equivalent com_1 com_2 = split com_1 == split com_2

-- 6d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join c = equivalent (join (split c)) c

notConnect :: [Command] -> Bool
notConnect [] = True
notConnect (x:xs) = case x of
    _ :#: _ -> False
    _ -> notConnect xs

prop_split :: Command -> Bool
prop_split c = (Sit `notElem` split c) && notConnect (split c)

-- 7. optimise
optimise :: Command -> Command
optimise c = getRidSit (join (filterTurnGoSit (addAdjacent (filterTurnGoSit (split c)))))
            where 
                getRidSit (cs :#: Sit) = cs

-- Helper function for optimise
isTurnZero :: Command -> Bool
isTurnZero (Turn 0) = False
isTurnZero _ = True

isGoZero :: Command -> Bool
isGoZero (Go 0) = False
isGoZero _ = True

isSit :: Command -> Bool
isSit Sit = False
isSit _ = True

filterTurnGoSit :: [Command] -> [Command]
filterTurnGoSit com = [c | c <- com, isTurnZero c && isGoZero c && isSit c]

addAdjacent :: [Command] -> [Command]
addAdjacent [] = []
addAdjacent [c] = [c]
addAdjacent (Go d1 : Go d2 : rest) = addAdjacent (Go (d1 + d2) : rest)
addAdjacent (Turn d1 : Turn d2 : rest) = addAdjacent (Turn (d1 + d2) : rest)
addAdjacent (Go d : Turn d1 : Turn d2 : rest) = addAdjacent (Go d : Turn (d1 + d2) : rest)
addAdjacent (Turn d : Go d1 : Go d2 : rest) = addAdjacent (Turn d : Go (d1 + d2) : rest)
addAdjacent (c : rest) = c : addAdjacent rest

