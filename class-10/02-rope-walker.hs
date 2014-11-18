import Control.Monad
{-
  Модифицируйте имеющуюся реализацию задачи о канатоходце (лекция 9) следующим образом:
  1) реализуйте загрузку входных данных из файла следующего вида:
       R 2
       L 3
       R -1
       B
       L 1
     и вычисление соответствующего им результата (в решении может пригодиться 
     функция foldr (<=<) return — проверьте её тип для получения подсказки);
  2) замените монаду Maybe на Either String так, чтобы в случае падения канатоходца
     можно было получить информацию о его причинах (нарушение баланса и в какую
     сторону или банан на канате);
  3) реализуйте операцию landBoth, поддерживающую одновременное (атомарное) приземление
     птиц на оба конца шеста, и внесите соответствующие изменения в другие функции;
  5) реализуйте операцию unlandAll (одновременный вылет всех птиц с шеста) и внесите
     соответствующие изменения в другие функции;
  4) организуйте масштабное тестирование.
-}

type Birds = Int

type Pole = (Birds, Birds)

balance = 3

updatePole :: Pole -> Either String Pole
updatePole p = if unbalancedLeft p then Left "Unbalanced because of birds on the left side" else if unbalancedRight p then Left "Unbalanced because of birds on the right side" else Right p
  where
    unbalancedLeft (l, r) = l - r > balance
    unbalancedRight (l, r) = r - l > balance

landLeft :: Birds -> Pole -> Either String Pole
landLeft n (left, right) = updatePole (left + n, right)

landRight :: Birds -> Pole -> Either String Pole
landRight n (left, right) = updatePole (left, right + n)

landBoth :: Birds -> Pole -> Either String Pole
landBoth n (left, right) = updatePole (left + n, right + n)

unlandAll :: Pole -> Either String Pole
unlandAll = const (Right (0, 0))

banana :: Pole -> Either String Pole
banana = const (Left "It's banana on the pole")

action :: (Char, Birds) -> Pole -> Either String Pole
action (c, n)
  | c == 'B' = banana
  | c == 'L' = landLeft n
  | c == 'R' = landRight n
  | c == 'E' = landBoth n
  | c == 'U' = unlandAll
  | otherwise = error ("There is no action for " ++ show c ++ " letter!")

process :: String -> Either String Pole
process s = foldr (<=<) return (reverse $ fmap action $ fmap (\x -> (head x, read (drop 2 x))) $ lines s) (0, 0)

fileRes :: IO (Either String Pole)
fileRes = (readFile "task2.txt") >>= return . process

tests = all test [1..7]
  where
    test 1 = (return (0, 0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)) == Left "Unbalanced because of birds on the right side"
    test 2 = (return (0, 0) >>= landRight 2 >>= landLeft 2 >>= landRight 2) == Right (2, 4)
    test 3 = (return (0, 0) >>= landLeft 1 >>= banana >>= landRight 1) == Left "It's banana on the pole"
    test 4 = (return (0, 0) >>= landLeft 1 >>= banana >>= landRight 1) == Left "It's banana on the pole"
    test 5 = (return (0, 0) >>= landLeft 1 >>= landRight 2 >>= landBoth 3 >>= landRight 1) == Right (4, 6)
    test 6 = (return (0, 0) >>= landLeft 2 >>= landRight 3 >>= unlandAll >>= landRight 1) == Right (0, 1)
    test 7 = (return (0, 0) >>= landBoth 3 >>= unlandAll >>= banana) == Left "It's banana on the pole"