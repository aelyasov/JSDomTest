module Analysis.CFG.Instrument.Tests (tests) where

import Test.Tasty (TestTree, testGroup, defaultMain, withResource)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, (@?=), runTestTT, Test(..), assertFailure)
import Analysis.CFG.Instrument
import Language.ECMAScript3.Syntax
import Language.ECMAScript3.PrettyPrint
import Language.ECMAScript3.Parser
import Analysis.CFG.Data (SLab)
import Data.Default (def)
import Control.Monad.State (runState)
import Analysis.CFG.Label (assignUniqueIdsSt)
import Data.Either.Combinators
import Data.Either.Combinators (fromRight')


tests :: TestTree
tests = testGroup "Analysis.CFG.Instrument"
        [ testCase "instrStatement01" instrStatement01
        , testCase "instrStatement02" instrStatement02
        , testCase "instrStatement03" instrStatement03
        , testCase "instrStatement04" instrStatement04  
        ]


instrStatement01 :: Assertion
instrStatement01 =
  let stmt = "{}"
  in instrStatementAsString stmt @?= "trace.push(1);\n{\n}"


instrStatement02 :: Assertion
instrStatement02 =
  let stmt = ";"
  in instrStatementAsString stmt @?= "trace.push(1);\n;"     


instrStatement03 :: Assertion
instrStatement03 =
  let stmt = "true"
  in instrStatementAsString stmt @?= "trace.push(1);\ntrue;"


instrStatement04 :: Assertion
instrStatement04 =
  let stmt = "if (a) {} else {}"
  in instrStatementAsString stmt @?= "trace.push(1);\nif (a) {\n   trace.push(2);\n   branchDistance.push({label: 1\n                       ,distance: instrument.absNegZero(a)});\n} else {\n   trace.push(3);\n   branchDistance.push({label: 1\n                       ,distance: instrument.absZero(a)});\n}"     


-- prettyPrint $  instrStatement $ head $ unJavaScript $ fst $ flip runState 0 $ assignUniqueIdsSt $ (\(Right x) -> x) $ parseFromString "{}"


instrStatementAsString :: String -> String
instrStatementAsString = show
                         . prettyPrint
                         . instrStatement
                         . head
                         . unJavaScript
                         . fst
                         . flip runState 0
                         . assignUniqueIdsSt
                         . fromRight'
                         . parseFromString


instrStatementAsString1 = prettyPrint
                         . instrStatement
                         . head
                         . unJavaScript
                         . fst
                         . flip runState 0
                         . assignUniqueIdsSt
                         . fromRight'
                         . parseFromString
