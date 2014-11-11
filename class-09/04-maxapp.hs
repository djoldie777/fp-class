import Control.Applicative
import System.Random

{-
  Пользуясь возможностями аппликативных функторов, определите функцию, 
  вычисляющую наибольший из результатов двух вычислений (значений в некотором
  контексте), принимаемых в качестве параметров (для результатов имеется
  экземпляр класса типов Ord).
-}

maxApp2 :: (Ord a, Applicative f) => f a -> f a -> f a
maxApp2 a b = max <$> a <*> b

{- Реализуйте аналогичную функцию в случае трёх заданных значений в контексте. -}

maxApp3 :: (Ord a, Applicative f) => f a -> f a -> f a -> f a
maxApp3 a b c = max <$> a <*> (maxApp2 b c)

{- Реализуйте аналогичную функцию в случае списка значений в контексте. -}

maxApp :: (Ord a, Applicative f) => [f a] -> f a
maxApp = foldl1 maxApp2

{-
  Продемонстрируйте использование написанных функций для аппликативных функторов Maybe,
  список (для каждого из двух экземпляров), Either String и IO.
-}

main = do
  print $ maxApp2 (Just 7) (Just 21)
  print $ maxApp [Just 7, Just 21, Just 27, Nothing] 
  print $ maxApp3 [7] [21] [27]
  print $ maxApp [[7], [21], [27], [25]]
  print $ maxApp2 (Right 7 :: Either String Int) (Right 21 :: Either String Int)
  print $ maxApp [Right 7, Right 21, Right 27, Left "error"]
  maxApp3 (randomIO :: IO Int) (randomIO :: IO Int) (randomIO :: IO Int) >>= print

{- (необязательно)
  Ясно ли вам, что вы реализовали нечто, похожее на моноид на аппликативных функторах?
  Можете ли вы выразить это в коде? Необходимо ли добавлять какие-нибудь ограничения?
-}
