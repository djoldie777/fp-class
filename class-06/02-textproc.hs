import System.IO
import Data.Char
import System.Environment
import System.Directory
import System.Random

{-
  Разработайте утилиту со следующими возможностями:
  1) подсчёт количества строк в заданном текстовом файле;
-}

processFile1 :: [FilePath] -> IO ()
processFile1 (fname : []) = do
	contents <- readFile fname
	print $ length $ lines contents


--2) добавление заданной строки в начало (конец) заданного файла;

processFile21 :: [FilePath] -> IO ()
processFile21 (s : fname : []) = do
	contents <- readFile fname
	removeFile fname
	writeFile fname (s ++ "\n" ++ contents)

processFile22 :: [FilePath] -> IO ()
processFile22 (s : fname : []) = appendFile fname ("\n" ++ s)


{-
  3) преобразование всех буквенных символов заданного файла к верхнему
     регистру (результат выводится на консоль);
-}

processFile3 :: [FilePath] -> IO ()
processFile3 (fname : []) = do
	contents <- readFile fname
	removeFile fname
	writeFile fname (map toUpper contents)
	putStrLn $ map toUpper contents


{-
  4) построчное слияние двух заданных файлов (каждая строка первого файла
     соединяется с соответствующей строкой второго файла);
-}

processFile4 :: [FilePath] -> IO ()
processFile4 (fname1 : fname2 : []) = do
	contents1 <- readFile fname1
	contents2 <- readFile fname2
	writeFile "res.txt" (concat $ map (++ "\n") $ zipWith (++) (lines contents1) (lines contents2))


{-
  5) генерация случайного текстового файла (случайность должна ограничиваться
     максимальным количеством строк в файле и символов в строке).

  Все входные данные программы должны передаваться с помощью параметров
  командной строки.
-}

processFile5 :: [[Char]] -> IO ()
processFile5 (r1 : r2 : []) = do
	gen1 <- newStdGen
	gen2 <- newStdGen
	gen3 <- newStdGen
	let countLine = head $ (randomRs (1, read r1) gen1 :: [Int])
	let countChars = head $ (randomRs (1, read r2) gen2 :: [Int])
	let countAll = countLine * countChars
	writeFile "random.txt" $ unlines $ fst $ fst $ foldl (\((acc, c), (str, cc)) x -> if (c < cc) then ((acc, c + 1), (str ++ [x], cc)) else ((acc ++ [str], 0), ("", (head $ (randomRs (5, read r2) gen2 :: [Int]))))) (([], 0), ("", countChars)) (take countAll $ (randomRs ('a','z') gen3 :: [Char]))


main = do
	(op : args) <- getArgs
 	if (op == "1") then
		processFile1 args
	else if (op == "2.1") then
  		processFile21 args
	else if (op == "2.2") then
		processFile22 args
 	else if (op == "3") then
  		processFile3  args
 	else if (op == "4") then
  		processFile4 args
	else if (op == "5") then
		processFile5 args
 	else
  		putStrLn "Unknown operation"