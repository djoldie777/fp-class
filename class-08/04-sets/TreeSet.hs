module TreeSet (Set, empty, add, contains, isEmpty) where

import AbstractSet
import BST

newtype Set = SetImpl (BST Int)

instance AbstractSet Set where
  empty = SetImpl Nil

  isEmpty (SetImpl xs) = emptyBST xs

  add (SetImpl xs) x = SetImpl (insertBST xs x)

  contains (SetImpl xs) x = containsBST xs x
