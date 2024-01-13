{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use $>" #-}
module Prop where

import Control.Monad
import MyParser(Parser,apply,parse,char,spot,
  token,match,star,plus,parseInt)
import Test.QuickCheck
import Control.Applicative ((<|>))

data Prop   = X
            | Y
            | T
            | F
            | Not Prop
            | Prop :&&: Prop
            | Prop :||: Prop
            | Prop :->: Prop
  deriving (Eq)

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
              fmap Not prop,
              liftM2 (:&&:) prop prop,
              liftM2 (:||:) prop prop,
              liftM2 (:->:) prop prop]
      where
      prop = gen (n `div` 2)

par :: String -> String
par s  =  "(" ++ s ++ ")"

instance Show Prop where
  show X            =  "X"
  show Y            =  "Y"
  show F            =  "0"
  show T            =  "1"
  show (Not p)      =  par ("not " ++ show p)
  show (p :||: q)   =  par (show p ++ " || " ++ show q)
  show (p :&&: q)   =  par (show p ++ " && " ++ show q)
  show (p :->: q)   =  par (show p ++ " -> " ++ show q)

-- ** Optional Exercises **

-- Exercise 1

parseProp :: Parser Prop
parseProp = X <$ token 'X'
        <|> Y <$ token 'Y'
        <|> T <$ token '1'
        <|> F <$ token '0'
        <|> Not <$> (token 'n' *> token 'o' *> token 't' *> parseProp)
        <|> do
              p1 <- parseProp
              op <- token '&' *> pure (:&&:) <|> token '|' *> pure (:||:) <|> token '-' *> token '>' *> pure (:->:)
              p2 <- parseProp
              return (p1 `op` p2)
        <|> parProp
  where
    parProp = token '(' *> parseProp <* token ')'


-- Exercise 2

prop_roundtrip :: Prop -> Bool
prop_roundtrip prop = prop == parse parseProp (show prop)
