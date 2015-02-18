module Http.Tests.Request (tests) where

------------------------------------------------------------------------------
import qualified Http.Request                   as Request
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)


------------------------------------------------------------------------------
tests :: [Test]
tests = [ testTheTests ]

------------------------------------------------------------------------------
testAddHeader :: Test
testAddHeader = testCase "request/test" $ assertEqual "2 is" (1 + 1) 3 
