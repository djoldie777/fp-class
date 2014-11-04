module Set (Set, empty, add, contains, isEmpty) where

import AbstractSet
import Data.List

newtype Set = SetImpl [Int]

instance AbstractSet Set where
  empty = SetImpl []

  isEmpty (SetImpl xs) = null xs

  add (SetImpl xs) x
    | not (x `elem` xs) = SetImpl  (x : xs)
    | otherwise = SetImpl  xs

  contains (SetImpl xs) x = x `elem` xs
