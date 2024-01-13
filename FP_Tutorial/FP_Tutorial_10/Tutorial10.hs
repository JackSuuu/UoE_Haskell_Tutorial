module Tutorial10 where

import Test.QuickCheck
import Control.Monad
import Data.Char

-- Question 1

-- 1a

ok :: String -> Bool
ok s = length s < 6 && and [isLower x | x <- s]

-- 1b

f :: [String] -> String
f str = head ([s | s <- str, ok s] ++ ["zzzzz"])

-- 1c

g :: [String] -> String
g str = head (rg str ++ ["zzzzz"])
  where
    rg [] = []
    rg (x:xs) 
          | ok x = x : rg xs
          | otherwise = rg xs

-- 1d

h :: [String] -> String
h str = head (filter ok str ++ ["zzzzz"])

-- Question 2

-- 2a

i :: [a] -> [a] -> [a]
i a b = tail a ++ [head b]

-- 2b

j :: [[a]] -> [[a]]
j str = [i x y | (x,y) <- zip str (rev str)]
        where
          rev :: [a] -> [a]
          rev str = i str str 
-- 2c

k :: [[a]] -> [[a]]
k str = k' str (rev str)
  where
    k' :: [[a]] -> [[a]] -> [[a]]
    k' [] _ = []
    k' (x:xs) (y:ys) = i x y : k' xs ys

    rev :: [a] -> [a]
    rev [] = []
    rev (x:xs) = rev xs ++ [x]


-- Question 3

data Prop = X
          | Y
          | T
          | F
          | Not Prop
          | Prop :&&: Prop
          | Prop :||: Prop
          | Prop :->: Prop
  deriving (Eq, Show)

instance Arbitrary Prop where
  arbitrary = sized gen
    where
    gen 0 =
      oneof [ return X,
              return Y,
              return T,
              return F ]
    gen n | n>0 =
      oneof [ return X,
              return Y,
              return T,
              return F,
              liftM Not prop,
              liftM2 (:&&:) prop prop,
              liftM2 (:||:) prop prop,
              liftM2 (:->:) prop prop]
      where
      prop = gen (n `div` 2)

-- 3a

eval :: Bool -> Bool -> Prop -> Bool
eval x y X = x
eval x y Y = y
eval x y (p :&&: q) = eval x y p && eval x y q
eval x y (p :||: q) = eval x y p || eval x y q
eval x y (p :->: q) = not (eval x y p) || eval x y q
eval x y (Not X) = not x
eval x y (Not Y) = not y

-- 3b

simple :: Prop -> Bool
simple p | p == T = True
         | p == F = True
         | p == X || p == Y = True
simple (T :||: q) = False
simple (p :||: T) = False
simple (F :||: q) = False
simple (p :||: F) = False
simple (T :&&: q) = False
simple (p :&&: T) = False
simple (F :&&: q) = False
simple (p :&&: F) = False
simple (p :->: q) = simple p && simple q
simple (p :||: q) = simple p && simple q
simple (p :&&: q) = simple p && simple q


-- 3c

simplify :: Prop -> Prop
simplify = simplifyTillNoChange
  where
    simplify' T              = T
    simplify' F              = F
    simplify' X              = X
    simplify' Y              = Y
    simplify' (Not T)        = F
    simplify' (Not F)        = T
    simplify' (F :&&: p)     = F
    simplify' (p :&&: F)     = F
    simplify' (T :&&: p)     = simplify' p
    simplify' (p :&&: T)     = simplify' p
    simplify' (F :||: p)     = simplify' p
    simplify' (p :||: F)     = simplify' p
    simplify' (T :||: p)     = T
    simplify' (p :||: T)     = T
    simplify' (F :->: p)     = T
    simplify' (p :->: T)     = T
    simplify' (T :->: p)     = simplify' p
    simplify' (p :->: F)     = Not (simplify' p)
    simplify' (p :->: q)     = simplify' p :->: simplify' q

    simplifyTillNoChange x = if x == simplify' x then x else simplifyTillNoChange (simplify' x)
