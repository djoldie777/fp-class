{-
  В параметрах командной строки указаны имена текстовых файлов, содержащих целые числа, разделённые
  пробелами и символами перевода строк. Определить количество и сумму различных чисел, встречающихся
  в заданных текстовых файлах.
-}

import System.Environment
import qualified Data.IntSet as Set

readNumFile :: FilePath -> IO [Int]
readNumFile fname = do
	content <- readFile fname
	return $ map read $ concatMap words $ lines content

solve :: [[Int]] -> (Int, Int)
solve xs = do
	let set = Set.unions $ map Set.fromList xs
	let sum = Set.foldl (+) 0 set
	(Set.size set, sum)

main = getArgs >>= mapM readNumFile >>= print.solve
