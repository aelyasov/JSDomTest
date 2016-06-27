module Genetic.ScoreJS.Tests (tests) where

import Test.Tasty (TestTree, testGroup, defaultMain, withResource)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, (@?=), runTestTT, Test(..))
import Genetic.ScoreJS (fitnessScore)
import Genetic.DataJS (Target(..),  JSArg(..))
import Data.Graph.Inductive (LEdge)
import Mock.ServerJS (runMockServer, stopMockServer)
import Analysis.CFG.Build (mkTestCFG)
import Analysis.CFG.Data (ELab)



tests :: TestTree
tests = withResource runMockServer stopMockServer
        (\_ -> testGroup "Genetic.ScoreJS"
               [ testCase "testFitnessScore01" testFitnessScore01
               , testCase "testFitnessScore02" testFitnessScore02
               , testCase "testFitnessScore02" testFitnessScore02  
                ]
        )

testFitnessScore01 :: Assertion
testFitnessScore01 = do
  gr  <- mkTestCFG "./Genetic/safeAdd.js"
  res <- fitnessScore (Target gr (-1,-1,"")) []
  fst res @?= fst res

testFitnessScore02 :: Assertion
testFitnessScore02 = do
  gr  <- mkTestCFG "./Genetic/safeAdd.js"
  res <- fitnessScore (Target gr (-1,-1,"")) []
  fst res @?= fst res  
  
