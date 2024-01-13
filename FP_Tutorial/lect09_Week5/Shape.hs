{-# LANGUAGE TemplateHaskell #-}
module Shape where

import Test.QuickCheck
import Control.Monad

type  Radius  =  Float
type  Width   =  Float
type  Height  =  Float

data  Shape  =  Circle Radius
             |  Rect Width Height
  deriving (Eq, Show)

area :: Shape -> Float
area (Circle r)  =  pi * r^2
area (Rect w h)  =  w * h

square :: Width -> Shape
square w  =  Rect w w

eqShape :: Shape -> Shape -> Bool
eqShape (Circle r) (Circle r')   =  (r == r')
eqShape (Rect w h) (Rect w' h')  =  (w == w') && (h == h')
eqShape x          y             =  False

showShape :: Shape -> String
showShape (Circle r)  =  "Circle " ++ showF r
showShape (Rect w h)  =  "Rect " ++ showF w ++ " " ++ showF h

showF :: Float -> String
showF x | x >= 0     =  show x
        | otherwise  =  "(" ++ show x ++ ")"

isCircle :: Shape -> Bool
isCircle (Circle r)  =  True
isCircle (Rect w h)  =  False

isRect :: Shape -> Bool
isRect (Circle r)  =  False
isRect (Rect w h)  =  True

radius :: Shape -> Float
radius (Circle r)  =  r

width :: Shape -> Float
width (Rect w h)  =  w

height :: Shape -> Float
height (Rect w h)  =  h

area' :: Shape -> Float
area' s  =
  if isCircle s then
     let
        r = radius s
     in
        pi * r^2
  else if isRect s then
     let
        w = width s
        h = height s
     in
        w * h
  else error "impossible"

-- QuickCheck tests
prop_eq :: Shape -> Shape -> Bool
prop_eq x y  =  eqShape x y == (x == y)

prop_show :: Shape -> Bool
prop_show x  =  showShape x == show x

prop_area' :: Shape -> Bool
prop_area' x  =  area x == area' x

-- tell QuickCheck how to generate random values of type Shape
instance Arbitrary Shape where
  arbitrary = oneof [ liftM Circle arbitrary, liftM2 Rect arbitrary arbitrary]

-- ** Automatically run QuickCheck properties named 'prop_*' and any other tests.
return []

main :: IO ()
main = do
  qc <- $quickCheckAll
  putStrLn ("QuickCheck: " ++ if qc then "Pass" else "Fail")

