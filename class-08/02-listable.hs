{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
{-
   Определите класс типов Listable с двумя функциями:
   toList :: a -> [a]
   fromList :: [a] -> a
-}

class Listable a where
   toList :: a -> [a]
   fromList :: [a] -> a

{-
  Объявите экземпляры класса типов Listable для следующих типов:
  1) String - строка разбивается по пробелам на список слов.
-}

instance Listable String where
   toList s = words s
   fromList s = unwords s

--2) Integer - любое целое число разбивается на список цифр.

instance Listable Integer where
   toList n
     | n == 0 = []
     | otherwise = (toList $ div n 10) ++ [mod n 10]
   fromList = foldl (\res x -> res * 10 + x) 0
