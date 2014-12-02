{-
   Модифицируйте представленное на лекции решение задачи о запросе пароля,
   удовлетворяющего требованиям по стойкости, следующим образом:
   - в командной строке задаются ограничения (минимальная длина, наличие букв,
     наличие цифр, наличие знаков пунктуации);
   - к стеку монад добавляется монада Reader, и с её помощью организуется
     доступ к ограничениям в функции isValid;
   - все попытки ввода пароля протоколируются средствами монады Writer.
-}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.Writer
import System.Environment

import Data.Char

isValid :: String -> [String] -> Bool
isValid s (l : symb : d : p : []) = (length s >= read l) && 
                (if (symb == "T" || symb == "t") then any isAlpha s else True) && 
                (if (d == "T" || d == "t") then any isNumber s else True) && 
                (if (p == "T" || p == "t") then any isPunctuation s else True)

getValidPassword :: MaybeT (ReaderT [String] (WriterT [String] IO)) String
getValidPassword = do
  restrictions <- ask
  liftIO $ putStrLn "Введите новый пароль:"
  s <- liftIO getLine
  tell [s]
  guard (isValid s restrictions)
  return s
 
askPassword :: MaybeT (ReaderT [String] (WriterT [String] IO)) ()
askPassword = do
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Сохранение в базе данных..."

main = do
	args <- getArgs
	pair <- runWriterT (runReaderT (runMaybeT askPassword) args)
	print $ snd pair
