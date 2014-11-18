{-
   Дан текстовый файл, содержащий данные о нескольких студентах в следующем формате: три последовательные
   строки соответствуют имени, возрасту и номеру группы (например, 4.8). Определить соответствующий тип
   данных (data) и организовать чтение файла в список значений этого типа.

   Для двух данных файлов объединить загруженные списки в один список, упорядоченный по имени и сохранить
   результат в новый файл того же формата. Указание: всюду следует использовать монадический синтаксис
   (операции >>= и >>, функции ap, liftM и другие функции монадической обработки данных, использование
   блока do не допускается).
-}

import Control.Monad
import Data.List

data Student = Student { name :: String, age :: Int, group :: String }
   deriving (Show, Ord, Eq)


groupByStudent :: [a] -> [[a]]
groupByStudent [] = []
groupByStudent xs = [take 3 xs] ++ (groupByStudent (drop 3 xs))


listToStudent :: [String] -> Student
listToStudent [n, a, c] = Student n (read a) c


readFromFile :: FilePath -> IO [Student]
readFromFile fname = readFile fname >>= (return . (foldl (\acc x -> acc ++ [listToStudent x]) []) . groupByStudent . lines)    


writeToFile :: FilePath -> [Student] -> IO ()
writeToFile fname xs = writeFile fname $ unlines $ map show xs


main = (++) `liftM` readFromFile "task1_1.txt" `ap` readFromFile "task1_2.txt" >>= writeToFile "task1_res.txt" . sort
