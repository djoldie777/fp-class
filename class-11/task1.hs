{-
1. Написать программу, работа которой управляется конфигурационным файлом, содержащим строки следующего формата:
имя поля=значение
Возможными именами полей являются summand (слагаемое), multiplier (множитель), divisor (делитель). Все значения
являются целыми числами. В качестве параметров командной строки программе подаются имя конфигурационного файла
и имя текстового файла с целочисленными данными. Над каждым целым числом из второго файла выполняются операции,
указанные в конфигурационном файле, то есть число складывается, умножается и делится соответственно.
Если какое-либо поле отсутствует, то действие не выполняется. Результаты вычислений выводятся на консоль.
Организовать доступ к параметрам конфигурационного файла средствами монады Reader.
-}

import Control.Monad.Reader
import System.Environment


parse :: String -> (String, Int)
parse s = (fst $ span (/= '=') s, read $ drop 1 $ snd $ span (/= '=') s)


operation :: (String, Int) -> Int -> Int
operation (s, v) x
  | (s == "divisor" && v == 0) = error "It's forbidden to divide by zero!"
  | s == "summand" = x + v
  | s == "multiplier" = x * v
  | s == "divisor" = div x v
  | otherwise = v


readF :: FilePath -> IO [(String, Int)]
readF fname = do
  contents <- readFile fname
  return $ map (parse) $ lines contents


applyOp :: [Int] -> Reader [(String, Int)] [Int]
applyOp numbers = do
  config <- ask
  return $ fmap (\x -> (foldl (flip operation) x config)) numbers


main = do
  (config : numbers : []) <- getArgs
  c <- readF config
  n <- readFile numbers >>= return . map (read) . words
  print $ runReader (applyOp n) c