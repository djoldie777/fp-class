import Parser
import SimpleParsers
import Control.Applicative hiding (many, optional)
import Control.Monad

{-
   Определите тип данных, представляющий адрес URL следующего вида:

     <схема>://<логин>:<пароль>@<хост>:<порт>/<URL‐путь>?<параметры>#<якорь>

   Реализуйте соответствующий парсер, считая, что обязательными компонентами
   URL являются только схема с символами "://" и имя хоста, все остальные
   компоненты могут отсутствовать.
-}


data Scheme = FTP | HTTP | HTTPS | Unk String
              deriving Show
type Server = String
type Path = String
type Login = String
type Password = String
type Port = String
type Parameters = String
type Anchor = String
data URL = URL Scheme Login Password Server Path Parameters Anchor
           deriving Show

scheme = (string "https" >> return HTTPS) <|>
         (string "http" >> return HTTP) <|>
         (string "ftp" >> return FTP) <|>
         Unk `liftM` lowers


login :: Parser Login
login = do
	l <- many1 (sat (/= ':'))
	char ':'
	return l


password :: Parser Password
password = do
	p <- many1 (sat (/= '@'))
	char '@'
	return p


url = URL <$>
      scheme <*>
      (string "://" >> many1 (sat (/='/'))) <*>
      many (sat $ const True)
