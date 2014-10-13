{-# LANGUAGE EmptyDataDecls #-}

module Drunkard where

{-
  1. Определить типы данных, необходимые для представления игральной карты в игре «Пьяница»,
  учитывая, что всего в колоде 52 карты.
-}

data Suit = Spades | Clubs | Diamonds | Hearts
	deriving (Show, Eq, Ord)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
	deriving (Show, Eq, Ord)

data Card = Card Value Suit
	deriving (Show, Eq)


-- 2. Определить функцию, проверяющую, что две переданные ей карты одной масти.

sameSuit :: Card -> Card -> Bool
sameSuit (Card _ s1) (Card _ s2) = s1 == s2


{-
  3. Определить функцию, проверяющую, что переданная ей первой карта старше второй
  (масть в игре «Пьяница» игнорируется). Возвращённое значение EQ означает, что обе
  карты одинакового старшинства.
-}

beats :: Card -> Card -> Ordering
(Card v1 _) `beats` (Card v2 _) = compare v1 v2


{-
  4. Определить функцию, которая по паре списков карт возвращает новую пару списков карт
  с учетом правил игры «Пьяница» (один раунд игры): 
    * из вершин списков берутся две карты и добавляются в конец того списка, карта из
      которого старше оставшейся;
    * если первые взятые карты совпадают по достоинству, то из списков берутся и
      сравниваются следующие две карты (и так до тех пор, пока не будет определён победитель
      раунда).
-}

game_round :: ([Card], [Card]) -> ([Card], [Card])
game_round ((x:xs), (y:ys))
	| x `beats` y == LT = (xs, ys ++ [x, y])
	| x `beats` y == GT = (xs ++ [x, y], ys)
	| otherwise = game_round (xs ++ [x], ys ++ [y])


{-
  5. Определить функцию, которая по паре списков возвращает количество раундов, необходимых
  для завершения игры (одна из колод оказывается пустой), и номер победителя.
-}

data Winner = First | Second
	deriving (Show, Eq)

game :: ([Card], [Card]) -> (Winner, Int)
game (xs, ys) = game1 (xs, ys) 0
	where
		game1 (_, []) n = (First, n)
		game1 ([], _) n = (Second, n)
		game1 (xs1, ys1) n = game1 (game_round (xs1, ys1)) (n + 1)


{-
  6. Приведите здесь результаты как минимум пяти запусков функции game (в каждом списке
  изначально должно быть не менее 10 карт).
-}

game_test1 = game ([(Card Two Spades), (Card Three Clubs), (Card Four Clubs), (Card Five Hearts), (Card Six Spades), (Card Seven Clubs), (Card Eight Diamonds), (Card Nine Hearts), (Card Ten Spades), (Card Jack Clubs)], [(Card Four Hearts), (Card Five Diamonds), (Card Six Clubs), (Card Seven Clubs), (Card Eight Spades), (Card Nine Hearts), (Card Ten Diamonds), (Card Jack Clubs), (Card Queen Clubs), (Card King Spades)]) == (Second, 10)

game_test2 = game ([(Card King Hearts), (Card Ace Diamonds), (Card Queen Clubs), (Card Ten Clubs), (Card Jack Spades), (Card Nine Hearts), (Card Eight Diamonds), (Card Six Clubs), (Card Seven Clubs), (Card Five Spades)], [(Card Queen Spades), (Card King Clubs), (Card Jack Clubs), (Card Nine Hearts), (Card Ten Spades), (Card Eight Clubs), (Card Seven Diamonds), (Card Five Hearts), (Card Two Spades), (Card Six Diamonds)]) == (First, 12)

game_test3 = game ([(Card Ace Spades), (Card King Clubs), (Card Queen Clubs), (Card Jack Hearts), (Card Ten Spades), (Card Nine Clubs), (Card Eight Diamonds), (Card Seven Hearts), (Card Six Spades), (Card Five Clubs)], [(Card King Hearts), (Card Queen Diamonds), (Card Jack Clubs), (Card Ten Clubs), (Card Nine Spades), (Card Eight Hearts), (Card Seven Diamonds), (Card Six Clubs), (Card Five Clubs), (Card Four Spades)]) == (First, 10)

game_test4 = game ([(Card Four Hearts), (Card Three Diamonds), (Card Five Clubs), (Card Four Clubs), (Card Seven Spades), (Card Six Hearts), (Card Eight Diamonds), (Card Nine Clubs), (Card Jack Clubs), (Card Ten Spades)], [(Card Jack Spades), (Card Four Diamonds), (Card Six Clubs), (Card Five Hearts), (Card Eight Spades), (Card Seven Clubs), (Card Nine Diamonds), (Card Ten Hearts), (Card King Spades), (Card Ace Diamonds)]) == (Second, 14)

game_test5 = game ([(Card Four Hearts), (Card Four Spades), (Card Five Clubs), (Card Four Clubs), (Card Seven Spades), (Card Six Hearts), (Card Eight Diamonds), (Card Nine Clubs), (Card Jack Clubs), (Card Ten Spades)], [(Card Three Spades), (Card Four Diamonds), (Card Six Clubs), (Card Five Hearts), (Card Eight Spades), (Card Seven Clubs), (Card Nine Diamonds), (Card Ten Hearts), (Card King Spades), (Card Ace Diamonds)]) == (Second, 12)


{-
  7 (необязательное упражнение). Реализуйте версию функции game, которая помимо результатов
  игры возвращает запись всех ходов (карты, выкладываемые по ходу игры для сравнения).
-}

game2 :: ([Card], [Card]) -> (Winner, [(Card,Card)], Int)
game2 (xs, ys) = game1 (xs, ys) [] 0
	where
		game1 (_, []) zs n = (First, zs, n)
		game1 ([], _) zs n = (Second, zs, n)
		game1 ((x:xs1), (y:ys1)) zs n = game1 (game_round ((x:xs1), (y:ys1))) (zs ++ [(x, y)]) (n + 1)


{-
  8 (необязательное упражнение). При выполнении функций из упражнений 4 и 5 возможно
  зацикливание. Чтобы его избежать, можно предусмотреть максимальное количество повторений
  (для раундов и ходов в рамках одного раунда). Подумайте, как обнаружить факт зацикливания
  в функции 4? Можно ли применить такой же подход в функции 5? Что нужно возвращать в случае
  обнаружения факта зацикливания? Измените соответствующим образом типовые аннотации и
  напишите безопасные по отношению к зацикливанию версии функций game_round и game.
-}
