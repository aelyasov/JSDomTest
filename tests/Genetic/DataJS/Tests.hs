module Genetic.DataJS.Tests (tests) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, (@?=), runTestTT, Test(..))
import Genetic.DataJS
import Data.Aeson (encode, decode)
import qualified Data.Map as M


-- | To run an individual test case: runTestTT $ TestCase testBranchDistanceToJSON
tests :: TestTree
tests = testGroup "Genetic.DataJS"
        [ testCase "testBranchDistanceToJSON"   testBranchDistanceToJSON
        , testCase "testBranchDistanceFromJSON" testBranchDistanceFromJSON
        , testCase "testJSEnviromentToJSON"     testJSEnviromentToJSON
        , testCase "testJSExecutionToJSON"      testJSExecutionToJSON
        , testCase "testJSExecutionFromJSON"    testJSExecutionFromJSON
        ]
        

testBranchDistanceToJSON :: Assertion
testBranchDistanceToJSON = encode (BranchDist 1 2) @?= "{\"label\":1,\"distance\":2}"

testBranchDistanceFromJSON :: Assertion
testBranchDistanceFromJSON = decode "{\"label\":1,\"distance\":2}" @?= Just (BranchDist 1 2)  
  
testJSEnviromentToJSON :: Assertion
testJSEnviromentToJSON =
  encode (JSEnviroment ["tag"] ["name"] ["id"] ["class"] ["selector"])
  @?=
  "{\"tags\":[\"tag\"],\"names\":[\"name\"],\"ids\":[\"id\"],\"classes\":[\"class\"],\"selectors\":[\"selector\"]}"

testJSEnviromentFromJSON :: Assertion
testJSEnviromentFromJSON =
  decode "{\"tags\":[\"tag\"],\"names\":[\"name\"],\"ids\":[\"id\"],\"classes\":[\"class\"],\"selectors\":[\"selector\"]}"
  @?=
  Just (JSEnviroment ["tag"] ["name"] ["id"] ["class"] ["selector"])
    
testJSExecutionToJSON :: Assertion
testJSExecutionToJSON =
  encode (JSExecution [] [] (M.fromList[("loop",1)]) (JSEnviroment [] [] [] [] []))
  @?=
  "{\"trace\":[],\"branchDistance\":[],\"loopMap\":{\"loop\":1},\"environment\":{\"tags\":[],\"names\":[],\"ids\":[],\"classes\":[],\"selectors\":[]}}"

testJSExecutionFromJSON :: Assertion
testJSExecutionFromJSON =
  decode "{\"trace\":[],\"branchDistance\":[],\"loopMap\":{\"loop\":1},\"environment\":{\"tags\":[],\"names\":[],\"ids\":[],\"classes\":[],\"selectors\":[]}}"
  @?=
  Just (JSExecution [] [] (M.fromList[("loop",1)]) (JSEnviroment [] [] [] [] []))
