import System.Environment
import System.Random

{-
  Напишите функцию reduce, принимающую один целочисленный аргумент a и возвращающую 0,
  если аргумент делится на 3, a^2, если он на 3 не делится и является при этом нечётным,
  a^3 в остальных случаях.
-}

reduce :: Integral a => a -> a
reduce a
  | (a `mod` 3 == 0) = 0
  | (a `mod` 2 /= 0) = a ^ 2
  | otherwise = a ^ 3


{-
  Напишите функцию, применяющую функцию reduce заданное количество раз к значению в контексте,
  являющемся функтором:
-}

reduceNF :: (Integral a, Functor f) => Int -> f a -> f a
reduceNF n f = last $ take n $ iterate (fmap reduce) (fmap reduce f)


{-
  Реализуйте следующие функции-преобразователи произвольным, но, желательно, осмысленным и
  нетривиальным способом.
-}

toList :: Integral a => [(a, a)]  -> [a]
toList = foldl (\acc (x,y) -> if (x == y) then acc ++ [x] else acc ++ [0]) []


toMaybe :: Integral a => [(a, a)]  -> Maybe a
toMaybe xs
  | (xs == []) = Nothing
  | otherwise = Just (sum $ map (snd) xs)


toEither :: Integral a => [(a, a)]  -> Either String a
toEither xs
  | (xs == []) = Left "Empty list"
  | otherwise = Right (sum $ map (snd) xs)


-- воспользуйтесь в этой функции случайными числами
toIO :: (Random a, Integral a) => [(a, a)]  -> IO a
toIO xs
  | (xs == []) = return 0
  | otherwise = do
	gen <- newStdGen
	let tmp = fst $ randomR (1,10) gen
    	return $ tmp * (sum $ zipWith (+) (map (fst) xs) (map (snd) xs))


{-
  В параметрах командной строки задано имя текстового файла, в каждой строке
  которого записана пара целых чисел, разделённых пробелами. Загрузите
  данные из файла в список пар целых чисел, преобразуйте этот список к
  значениям в контекстах [], Maybe, Either String и IO и примените к каждому
  из контекстов функцию reduceNF (значение N также должно браться из 
  параметров командной строки).
-}

parseArgs :: [String] -> (FilePath, Int)
parseArgs args = (head args, read $ last args)


readData :: FilePath -> IO [(Int, Int)]
readData fname = do
  contents <- readFile fname
  return $ fmap (\xs -> (read $ head xs, read $ last xs)) $ fmap words $ lines contents
  --return $ foldl (\acc x -> acc ++ [(read $ head x, read $ last x)]) [] (fmap words $ lines contents)


main = do
  (fname, n) <- parseArgs `fmap` getArgs
  ps <- readData fname
  print ps
  print $ reduceNF n (toList ps)
  print $ reduceNF n (toMaybe ps)
  print $ reduceNF n (toEither ps)
  reduceNF n (toIO ps) >>= print

{-
  Подготовьте несколько тестовых файлов, демонстрирующих особенности различных контекстов.
  Скопируйте сюда результаты вызова программы на этих файлах.

*Main> :main 02-1.txt 1
[(2,2),(3,4),(5,5),(7,8),(9,9)]
[8,0,25,0,0]
Just 21952
Right 21952
0

*Main> :main 02-2.txt 1
[(2,6),(1,7),(5,2),(1,8),(7,7)]
[0,0,0,0,49]
Just 0
Right 0
97336000

*Main> :main 02-3.txt 1
[]
[]
Nothing
Left "Empty list"
0

-}
