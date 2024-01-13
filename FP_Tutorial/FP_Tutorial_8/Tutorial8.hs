module Tutorial8 where  

import System.Random
import Test.QuickCheck

-- Importing the keymap module

-- import KeymapList (Keymap,
--                     invariant, keys,
--                     size, get, set, del, select,
--                     toList, fromList)
import GHC.Base (undefined)
import Data.Maybe (Maybe(Nothing), catMaybes)
import KeymapTree (Keymap (Leaf, Node),
                    invariant, keys,
                    size, depth, get, set, select,
                    toList, fromList,
                    filterLT, filterGT, merge,
                  )

-- Type declarations
type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item

-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]

-- Exercise 1
getItems :: [Barcode] -> Catalogue -> [Item]
getItems code catalogue = catMaybes [get c catalogue | c <- code]

-- Exercise 2

{-
*Tutorial8> db <- readDB
Done
(1.88 secs, 2,271,784,856 bytes)
*Tutorial8> size db
104651
(0.05 secs, 75,664 bytes)
*Tutorial8> ks <- samples 1000 db
(0.16 secs, 9,106,048 bytes)
*Tutorial8> force (getItems ks db)
()
(4.23 secs, 384,000 bytes)

If the database was two times bigger,
how would you expect the time to change?
ANS: It will be two times bigger than the pervious, 
since it traverse the database linearly by searching every element
-}

-- for Exercises 3--6 check KeymapTree.hs 

-- Exercise 7

{-
*Tutorial8> db <- readDB
Done
(4.07 secs, 3,357,643,720 bytes)
*Tutorial8> size db
104651
(0.08 secs, 27,703,512 bytes)
*Tutorial8> depth db
40
(0.06 secs, 26,862,368 bytes)
*Tutorial8> ks <- loadKeys
(1.26 secs, 100,100,072 bytes)
*Tutorial8> force (getItems ks db)
()
(0.03 secs, 5,161,032 bytes)

If the database was two times bigger,
how would you expect the time to change?
ANS: tree will take a logarithmic increase, 
since it recursive search for its left node / right node,
every new node increases the average search time twice
-}

-- for Exercises 8--10 check KeymapTree.hs 

-- ** Input-output

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine (lines dbl))
            putStrLn (force (show db) `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

samples :: Int -> Catalogue -> IO [Barcode]
samples n db =
  do g <- newStdGen
     let allKeys = [ key | (key,item) <- toList db ]
     let indices = randomRs (0, length allKeys - 1) g
     let keys = take n [ allKeys !! i | i <- indices ]
     saveKeys keys
     return (force keys `seq` keys)

saveKeys :: [Barcode] -> IO ()
saveKeys = writeFile "keys.cache" . show

loadKeys :: IO [Barcode]
loadKeys = do
  keys <- read <$> readFile "keys.cache"
  return (force keys `seq` keys)

force :: [a] -> ()
force = foldr seq ()
