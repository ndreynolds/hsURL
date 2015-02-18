module Http.Uri
  ( UriParts (..)
  , Login (..)
  , uriParts )
  where

import Text.ParserCombinators.Parsec
import Data.List


-- A URI parser 
--
-- Based on http://www.w3.org/Addressing/URL/5_URI_BNF.html

data UriParts = UriParts 
    { scheme :: String
    , host :: String
    , port :: Int
    , login :: Maybe Login
    , path :: String
    } deriving (Show)

data Login = Login { user :: String
                   , password :: String
                   } deriving (Show)


uriDefaults :: UriParts
uriDefaults = UriParts { scheme = "http"
                       , host = ""
                       , port = 80
                       , login = Nothing
                       , path = "/"
                       }

uriParts :: String -> UriParts
uriParts uri = case parse parseURI "URL" uri of
    Left err -> error $ "Parse error at " ++ show err
    Right res -> res


reserved   = oneOf ";/?:@&="
safe       = oneOf "$-_.+"
extra      = oneOf "!*'(),"
unreserved = alphaNum <|> safe <|> extra
escape     = do char '%'; count 2 hexDigit
loginChars = many1 (unreserved <|> oneOf ";?&=") <|> escape


parseURI :: Parser UriParts
parseURI = do scheme <- parseScheme
              login <- option (login uriDefaults) (try parseLogin)
              host <- parseHost
              port <- option (port uriDefaults) parsePort
              path <- option (path uriDefaults) parsePath
              return uriDefaults { scheme = scheme
                                 , login = login
                                 , path = path
                                 , host = host
                                 , port = port }

parseScheme :: Parser String
parseScheme = do init <- letter
                 rest <- many $ letter <|> digit <|> oneOf "+-."
                 string "://"
                 return $ init : rest

parsePassword :: Parser [String]
parsePassword = do char ':'
                   many loginChars

parseLogin :: Parser (Maybe Login)
parseLogin = do user <- many loginChars
                pass <- option [] parsePassword
                char '@'
                return $ Just Login { user = concat user
                                    , password = concat pass }

parseHost :: Parser String
parseHost = parseHostname <|> parseHostnumber

parseHostnumber :: Parser String
parseHostnumber = do p1 <- many1 digit
                     p2 <- char '.' >> many1 digit
                     p3 <- char '.' >> many1 digit
                     p4 <- char '.' >> many1 digit
                     return $ intercalate "." [p1,p2,p3,p4]

parseHostname :: Parser String
parseHostname = many1 (letter <|> oneOf ".-")

parsePort :: Parser Int
parsePort = do char ':'
               port <- many1 digit
               return (read port :: Int)
               
parsePath :: Parser String
parsePath = do char '/'
               path <- many $ many1 (unreserved <|> reserved) <|> escape
               eof
               return $ '/' : concat path

