module Http.Response
  ( Response (..) 
  , response 
  ) where

import Text.ParserCombinators.Parsec hiding (token)


data Response = Response 
    { statusLine :: StatusLine
    , headers :: [(String,String)]
    , body :: String
    } deriving (Show)

data StatusLine = StatusLine 
    { code :: Int
    , reasonPhrase :: String
    , version :: String
    } deriving (Show)


statusLineDefaults :: StatusLine
statusLineDefaults = StatusLine 
                     { code=500
                     , reasonPhrase="Unable to parse response"
                     , version="HTTP/1.1"
                     } 
    
responseDefaults :: Response
responseDefaults = Response 
                   { statusLine=statusLineDefaults
                   , headers=[]
                   , body=""
                   }

-- TODO: better name (contrast with request)
response :: String -> Response
response text = case parse parseResponse "Response" text of
    Left err -> error $ "Parse error at " ++ show err
    Right res -> res


crlf = string "\r\n"
ctls = "\r\n"

-- Any character (0-127), except for CTLs and separators
token = many (alphaNum <|> oneOf "!#$%&'*+-.^_`|~")

parseResponse :: Parser Response
parseResponse = do sl <- parseStatusLine
                   headers <- many parseHeader
                   crlf
                   body <- many anyChar
                   eof
                   return Response { statusLine = sl,
                                     headers = headers,
                                     body = body }

parseHeader :: Parser (String,String)
parseHeader = do name <- token
                 char ':'
                 spaces
                 val <- many1 $ noneOf ctls
                 crlf
                 return (name,val)

parseVersion :: Parser String
parseVersion = do string vPrefix
                  major <- many1 digit
                  char '.'
                  minor <- many1 digit
                  return $ vPrefix ++ major ++ "." ++ minor
               where vPrefix = "HTTP/"

parseStatusLine :: Parser StatusLine
parseStatusLine = do version <- parseVersion
                     space
                     code <- count 3 digit
                     space
                     reason <- many $ noneOf "\r\n"
                     crlf
                     return StatusLine { code = read code :: Int, 
                                         reasonPhrase = reason, 
                                         version = version }
