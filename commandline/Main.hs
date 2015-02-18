module Main where

import System.Console.GetOpt
import System.IO
import System.Environment

import Data.Version (showVersion)
import Paths_hsURL  (version)

import Http.Request  (makeRequest, get)
import Http.Response (body)

data Flag = Verbose 
          | Version
          | Help
          deriving (Show,Eq)

options :: [OptDescr Flag]
options =
    [ Option ['v']  ["verbose"] (NoArg Verbose) "verbose output on stdout"
    , Option ['h']  ["help"]    (NoArg Help)    "show usage info"
    , Option []     ["version"] (NoArg Version) "print version"
    ]

header = "Usage: hsurl [OPTIONS...] <url>"

usage :: String
usage = usageInfo header options

err :: String -> IO ()
err msg = ioError $ userError (msg ++ " " ++ usage)

doRequest opts url = do
    response <- makeRequest $ get url
    putStr $ body response

main :: IO ()
main = do
    args <- getArgs
    case getOpt Permute options args of
        ([Help],_,[])    -> putStrLn usage
        ([Version],_,[]) -> putStrLn $ "hsURL " ++ showVersion version
        (_,[],[])        -> err "No URL was provided!"
        (opts,[url],[])  -> doRequest opts url
        (_,_,[])         -> err "Too many arguments!"
        (_,_,errs)       -> err $ concat errs
