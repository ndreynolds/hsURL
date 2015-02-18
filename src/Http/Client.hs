module Http.Client
    ( ClientConfig )
    where

import Control.Monad.State

data ClientConfig = ClientConfig
    { followRedirects :: Bool
    , userAgent :: String
    } deriving Show

type Client = State ClientConfig

defaultConfig = ClientConfig { followRedirects=True
                             , userAgent="hsURL"
                             }

readConfig :: Client String
readConfig = do
    config <- get
    return $ userAgent config

main = print $ evalState readConfig defaultConfig
