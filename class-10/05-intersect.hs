{- Пользуясь списком как монадой, вычислите пересечение  заданных списков -}
intersect :: Eq a => [[a]] -> [a]
intersect [] = []
intersect (x : xs) = foldr (\acc x -> return x >>= filter (`elem` acc)) x xs

tests = all test [1,2]
  where
    test 1 = intersect [[7,1,2], [1,7,3], [1,4,7]] == [7,1]
    test 2 = intersect [[5,6], [4,5], [8,4,5]] == [5]
