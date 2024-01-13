import GHC.ByteOrder (ByteOrder)


-- Exercise 1
-- Things B is the old one, because it doesn't have the black thick border like others

-- Exercise 2
data Thing = A | B | C | D | E deriving (Eq, Show)
things :: [Thing]
things = [A, B, C, D, E]

data Colour = Orange | Blue deriving (Eq, Show)
data Shape = Disc | Square deriving (Eq, Show)
data Border = Thick | Thin deriving (Eq, Show)
data Size = Big | Small deriving (Eq, Show)

colour :: Thing -> Colour -- Pattern Matching
colour A = Orange
colour B = Orange
colour C = Orange
colour D = Blue
colour E = Orange

shape :: Thing -> Shape
shape A = Square 
shape B = Square
shape C = Disc
shape D = Square
shape E = Square

border :: Thing -> Border
border A = Thick
border B = Thin
border C = Thick
border D = Thick
border E = Thick

size :: Thing -> Size
size A = Big
size B = Big
size C = Big
size D = Big
size E = Small

-- Exercise 3
type Predicate u = u -> Bool

isOrange :: Predicate Thing
isOrange x = x `elem` [A, B, C, E]
-- isOrange x = colour x == Orange

isBlue :: Predicate Thing
isBlue x = not (isOrange x)

isSquare :: Predicate Thing
isSquare x = x `elem` [A, B, D, E]

isDisc :: Predicate Thing
isDisc x = not (isSquare x)

isThickBorder :: Predicate Thing
isThickBorder x = x `elem` [A, C, D, E]

isThinBorder :: Predicate Thing
isThinBorder x = not (isThickBorder x)

isBig :: Predicate Thing
isBig x = x `elem` [A, B, C, D]

isSmall :: Predicate Thing
isSmall x = not (isBig x)

-- Exercise 4
-- 1. [ isThinBorder x | x <- things, isBlue x && isSquare x ]
-- 2. [ (x, isSmall x) | x <- things, isOrange x && isDisc x ]

-- Exercise 5: No square is blue
-- It is not the case that some X is Y
-- not (or [ isBlue x | x <- things, isSquare x])
-- Every X is not Y
-- and [ not (isSquare x) | x<- things, isBlue x]

-- Exercise 6
thingsOtherThan :: Thing -> [Thing]
thingsOtherThan thing = [x | x <- things, x /= thing]

properties :: [Predicate Thing]
properties = [isOrange, isBlue, isSquare, isDisc, isThickBorder, isThinBorder, isBig, isSmall] 

propertiesOf :: Thing -> [Predicate Thing] 
propertiesOf thing = [property | property <- properties, property thing]

-- First, get all the different things than x
isPropertyOfAnotherThing :: Predicate Thing -> Thing -> Bool
isPropertyOfAnotherThing p x = and [p thing | thing <- thingsOtherThan x]

propertiesOnlyOf :: Thing -> [Predicate Thing]
propertiesOnlyOf x = [p | p <- propertiesOf x, not (isPropertyOfAnotherThing p x)]

rank :: Thing -> Int
rank x = length (propertiesOnlyOf x)
