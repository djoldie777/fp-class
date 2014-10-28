import System.Directory
import System.Random
import System.Environment

{-
  Создать текстовый файл, содержащий случайные целые числа, разделённые пробелами
  и символами перевода строки. В командной строке должны задаваться следующие параметры:
  1) имя создаваемого файла;
  2) диапазон генерируемых случайных чисел: от и до;
  3) количество чисел в строке;
  4) количество строк в файле.
-}

processFile fname r1 r2 dcount scount = do
	gen <- newStdGen
	let countAll = (read dcount) * (read scount)
	writeFile fname $ unlines $ fst $ fst $ foldl (\((acc, c), str) x -> if (c < (read dcount - 1)) then ((acc, c + 1), str ++ (show x) ++ " ") else ((acc ++ [str ++ (show x)], 0), "")) (([], 0), "") (take countAll $ (randomRs (read r1, read r2) gen :: [Int]))

main = do
	[fname, r1, r2, dcount, scount] <- getArgs
	processFile fname r1 r2 dcount scount