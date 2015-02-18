module Main where

import qualified Http.Tests.Request as Request
import           Test.Framework     (defaultMain, testGroup)


------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests
  where
    tests = [ testGroup "Tests.Request" Request.tests
            ]
