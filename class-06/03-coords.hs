import System.Random
import System.Environment

{-
  Написать программу, которая в зависимости от параметров командной строки
  а) генерирует случайный текстовый файл содержащий декартовы координаты точек на плоскости
     (по одной точке в каждой строке);
-}

genFile :: [FilePath] -> IO ()
genFile (fname : []) = do
	gen1 <- newStdGen
	gen2 <- newStdGen
	gen3 <- newStdGen 
	let countLine = head $ (randomRs (5,10) gen1 :: [Int])
	writeFile fname $ unlines $ map show $ take countLine $ zip (randomRs (-100,100) gen2 :: [Int]) (randomRs (-100,100) gen3 :: [Int])


{-
  б) определяет по заданному файлу в указанном ранее формате количество точек в каждой
     из четвертей;
-}

getQuarter :: (Num a1, Num a2, Num a, Ord a1, Ord a2) => (a1, a2) -> a
getQuarter (x, y)
	| x > 0 && y > 0 = 1
 	| x < 0 && y > 0 = 2
 	| x < 0 && y < 0 = 3
 	| otherwise = 4


getCount :: [FilePath] -> IO ()
getCount (fname : []) = do
	contents <- readFile fname
	let xs = foldl (\acc x -> acc ++ [getQuarter $ read x]) [] (lines contents)
	mapM_ print $ foldl (\[q1, q2, q3, q4] x -> if x == 1 then [q1 + 1, q2, q3, q4] 
		else if x == 2 then [q1, q2 + 1, q3, q4]
            	else if x == 3 then [q1, q2, q3 + 1, q4]
            	else [q1, q2, q3, q4 + 1]) [0,0,0,0] xs


--в) отыскивает наиболее удалённую от начала координат точку.

getDistance :: Floating a => (a, a) -> a
getDistance (x, y) = sqrt $ x^2 + y^2

getTheMostRemoted :: [FilePath] -> IO ()
getTheMostRemoted (fname : []) = do
	contents <- readFile fname
	print $ fst $ foldl (\(p, max) x -> if ((getDistance $ read x) > max) then (read x, getDistance $ read x) else (p, max)) ((0,0), 0) (lines contents)


main = do
	(op : args) <- getArgs
 	if (op == "1") then
		genFile args
	else if (op == "2") then
  		getCount args
 	else if (op == "3") then
  		getTheMostRemoted  args
 	else
  		putStrLn "Unknown operation"