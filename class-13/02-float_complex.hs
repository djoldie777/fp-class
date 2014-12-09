import Parser
import SimpleParsers
import ParseNumbers
import Control.Applicative hiding (many, optional)

{- Напишите парсер для вещественных чисел. -}
float :: Parser Float
float = num <|> fromIntegral <$> integer
  where
    num = do
	n <- integer
	char '.'
	m <- natural
	return $ read (show n ++ "." ++ show m)


{-
  Напишите парсер для представления комплексных чисел,
  записываемых в виде вещественной и мнимой части через запятую
  в круглых скобках, например, "(2.3, 1)".
-}
complex :: Parser (Float, Float)
complex = bracket "(" ")" $ do
  x <- token float
  char ','
  y <- token float
  return $ (x, y)


{-
  Напишите парсер для списка комплексных чисел (разделитель — точка с запятой),
  заключённого в квадратные скобки.
-}
complexList :: Parser [(Float, Float)]
complexList = bracket "[" "]" $ sepBy (token complex) (symbol ";")


{-
  Модифицируйте предыдущий парсер таким образом, чтобы в исходной строке
  могли встречаться как комплексные числа, так и вещественные (мнимая часть
  при этом должна считаться равной нулю).
-}
floatToComplex = do
  x <- token float
  return $ (x, 0)

complexList2 :: Parser [(Float, Float)]
complexList2 = bracket "[" "]" $ sepBy (token complex <|> floatToComplex) (symbol ";")


{-
   Модифицируйте предыдущий парсер таким образом, чтобы компоненты списка
   разделялись запятой, а не точкой запятой. Постарайтесь реализовать
   требуемое с помощью вспомогательных парсеров, допускающих повторное применение.
-}
complexList3 :: Parser [(Float, Float)]
complexList3 = bracket "[" "]" $ sepBy (token complex <|> floatToComplex) (symbol ",")
