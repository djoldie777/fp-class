import System.IO
import Data.Char
import System.Environment
import System.Directory

{-
  Разработайте утилиту со следующими возможностями:
  1) подсчёт количества строк в заданном текстовом файле;
-}

processFile1 handle = do
	contents <- hGetContents handle
	putStrLn $ show $ length $ lines contents


--2) добавление заданной строки в начало (конец) заданного файла;

processFile21 s fname = do
	contents <- readFile fname
	removeFile fname
	writeFile fname (s ++ "\n" ++ contents)

processFile22 s fname = appendFile fname s


{-
  3) преобразование всех буквенных символов заданного файла к верхнему
     регистру (результат выводится на консоль);
-}

processFile3 fname = do
	contents <- readFile fname
	removeFile fname
	writeFile fname (map toUpper contents)
	putStrLn $ map toUpper contents


{-
  4) построчное слияние двух заданных файлов (каждая строка первого файла
     соединяется с соответствующей строкой второго файла);
-}

processFile4 fname1 fname2 = do
	contents1 <- readFile fname1
	contents2 <- readFile fname2
	writeFile "res.txt" (concat $ map (++ "\n") $ zipWith (++) (lines contents1) (lines contents2))


{-
  5) генерация случайного текстового файла (случайность должна ограничиваться
     максимальным количеством строк в файле и символов в строке).

  Все входные данные программы должны передаваться с помощью параметров
  командной строки.
-}


main = do
{-
	[fname] <- getArgs
	putStrLn "Количество строк в заданном файле:"
	withFile fname ReadMode processFile1
-}
{-
	[fname] <- getArgs
	putStrLn "Преобразование к верхнему регистру:"
	processFile3 fname
-}
{-
	[s, fname] <- getArgs
	processFile21 s fname
	processFile22 s fname
-}

	[fname1, fname2] <- getArgs
	processFile4 fname1 fname2








