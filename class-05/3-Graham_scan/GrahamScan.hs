{-# LANGUAGE EmptyDataDecls #-}

module GrahamScan where

-- 1. Определить тип Point для хранения информации о точке на вещественной плоскости.

data Point = Point Double Double
	deriving (Show)


{-
  2. Если заданы три точки a, b, c, можно рассматривать направление поворота от отрезка прямой,
  заключённого между точками a и b, к отрезку прямой, заключённому между точками b и c. Поворот
  может осуществляться влево, вправо или отрезки могут лежать на одной прямой — для представления
  этих трёх возможностей определить специальный тип Direction.
-}

data Direction = LD | SD | RD
	deriving (Show)


{-
  3. Определить функцию, которая принимает список точек и вычисляет список направлений поворотов
  для каждых трёх последовательных точек. Например, для списка точек [a, b, c, d, e] она возвращает
  список поворотов для точек [a, b, c], [b, c, d] и [c, d, e]. При написании этой функции рекомендуется
  определить несколько вспомогательных функций.
-}

f2b :: (Num a, Ord a) => a -> [a1] -> [a1]
f2b n = fst . foldl (\(acc, c) x -> if c < n then (acc ++ [x], c + 1) else (acc, c + 1)) ([], 0)

f2c :: (Num a, Ord a) => a -> [b] -> [b]
f2c n = foldl (flip (:)) [] . f2b n . foldl (flip (:)) []

f2h :: (Num a, Ord a) => a -> a -> [a1] -> [[a1]]
f2h n k = (\(acc, x, tmp) -> acc ++ [x]) . foldl (\(acc, xs, tmp) x -> if (tmp /= 0) then (acc, xs ++ [x], tmp - 1) else (acc ++ [xs], (f2c k xs) ++ [x], n - k - 1)) ([], [], n)

find_dir :: [Point] -> Direction
find_dir ((Point x1 y1) : (Point x2 y2) : (Point x3 y3) :xs) 
	| (x2 - x1) * (y3 - y2) - (x3 - x2) * (y2 - y1) > 0 = LD
	| (x2 - x1) * (y3 - y2) - (x3 - x2) * (y2 - y1) < 0 = RD
	| otherwise = SD

directions :: [Point] -> [Direction]
directions (x:xs) = f $ f2h 3 2 (x:xs)
	where 
		f = foldl(\acc x -> acc ++ [find_dir x]) []


{-
  4. Пользуясь решениями предыдущих упражнений, реализовать алгоритм Грэхема нахождения выпуклой
  оболочки множества точек на вещественной плоскости. Описание алгоритма можно взять в английском
  (Graham scan) или русском разделах Википедии. Там же можно разобраться с тем, что именно называют
  выпуклой оболочкой (convex hull). Визуализация порядка работы алгоритма имеется на Youtube:
  http://www.youtube.com/watch?v=BTgjXwhoMuI
-}

graham_scan :: [Point] -> [Point]
graham_scan = undefined

{-
  5. Приведите несколько примеров работы функции graham_scan.
-}
