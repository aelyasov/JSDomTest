module Genetic.ScoreJS.Tests (tests) where

import Test.Tasty (TestTree, testGroup, defaultMain, withResource)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, (@?=), runTestTT, Test(..))
import Genetic.ScoreJS 
import Genetic.DataJS 
import Data.Graph.Inductive (LEdge)
import Mock.ServerJS (runMockServer, stopMockServer)
import Analysis.CFG.Build (mkTestCFG)
import Analysis.CFG.Data (ELab)
import qualified Data.IntMap as IntMap


tests :: TestTree
tests = withResource runMockServer stopMockServer
        (\_ -> testGroup "Genetic.ScoreJS"
               [ testCase "testFitnessScore01"   testFitnessScore01
               , testCase "testFitnessScore02"   testFitnessScore02
               , testCase "testComputeFitnessSequence01" testComputeFitnessSequence01
               , testCase "testComputeFitnessSequence02" testComputeFitnessSequence02
               , testCase "testComputeFitnessSequence03" testComputeFitnessSequence03
               , testCase "testComputeFitnessSequence04" testComputeFitnessSequence04
               , testCase "testComputeFitnessBranch01"   testComputeFitnessBranch01
               , testCase "testComputeFitnessBranch02"   testComputeFitnessBranch02
               , testCase "testComputeFitnessBranch03"   testComputeFitnessBranch03
               , testCase "testComputeFitnessBranch04"   testComputeFitnessBranch04
               , testCase "testComputeFitnessBranch05"   testComputeFitnessBranch05
               , testCase "testComputeFitnessBranch06"   testComputeFitnessBranch06
               , testCase "testComputeFitnessBranch07"   testComputeFitnessBranch07
               , testCase "testComputeFitnessBranch08"   testComputeFitnessBranch08
               , testCase "testComputeFitnessBranch09"   testComputeFitnessBranch09
               , testCase "testComputeFitnessLoop01"     testComputeFitnessLoop01  
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


testComputeFitnessSequence01 :: Assertion
testComputeFitnessSequence01 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test1.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,2,3]
      target      = 3
      branchDists = []
  computeFitness1 graph loopIterMap target path branchDists @?= 0

  
testComputeFitnessSequence02 :: Assertion
testComputeFitnessSequence02 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test1.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,2,3,-100]
      target      = 3
      branchDists = []
  computeFitness1 graph loopIterMap target path branchDists @?= 1 

    
testComputeFitnessSequence03 :: Assertion
testComputeFitnessSequence03 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test1.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,2,-100]
      target      = 3
      branchDists = []
  computeFitness1 graph loopIterMap target path branchDists @?= 2    


testComputeFitnessSequence04 :: Assertion
testComputeFitnessSequence04 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test1.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,-100]
      target      = 3
      branchDists = []
  computeFitness1 graph loopIterMap target path branchDists @?= 3   


testComputeFitnessBranch01 :: Assertion
testComputeFitnessBranch01 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test2.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,2,3,-100]
      target      = 5
      branchDists = [BranchDist 1 1]
  computeFitness1 graph loopIterMap target path branchDists @?= 2.25   


testComputeFitnessBranch02 :: Assertion
testComputeFitnessBranch02 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test2.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,2,3]
      target      = 5
      branchDists = [BranchDist 1 1]
  computeFitness1 graph loopIterMap target path branchDists @?= 2.25   


testComputeFitnessBranch03 :: Assertion
testComputeFitnessBranch03 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test2.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,2]
      target      = 5
      branchDists = [BranchDist 1 1]
  computeFitness1 graph loopIterMap target path branchDists @?= 2.25


testComputeFitnessBranch04 :: Assertion
testComputeFitnessBranch04 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test2.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,2,3]
      target      = 5
      branchDists = [BranchDist 1 4]
  computeFitness1 graph loopIterMap target path branchDists @?= 2.4


testComputeFitnessBranch05 :: Assertion
testComputeFitnessBranch05 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test2.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,2,3]
      target      = 7
      branchDists = [BranchDist 1 4]
  computeFitness1 graph loopIterMap target path branchDists @?= 4.4


testComputeFitnessBranch06 :: Assertion
testComputeFitnessBranch06 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test2.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,4,5,6,7]
      target      = 9
      branchDists = [BranchDist 1 1, BranchDist 5 1]
  computeFitness1 graph loopIterMap target path branchDists @?= 2.25 


testComputeFitnessBranch07 :: Assertion
testComputeFitnessBranch07 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test2.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,4,5,6,7]
      target      = 9
      branchDists = [BranchDist 1 1, BranchDist 5 4]
  computeFitness1 graph loopIterMap target path branchDists @?= 2.4 


testComputeFitnessBranch08 :: Assertion
testComputeFitnessBranch08 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test2.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,4,5,8]
      target      = 9
      branchDists = [BranchDist 1 1, BranchDist 5 4]
  computeFitness1 graph loopIterMap target path branchDists @?= 1


testComputeFitnessBranch09 :: Assertion
testComputeFitnessBranch09 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test2.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,4,5,8,-100]
      target      = 9
      branchDists = [BranchDist 1 1, BranchDist 5 4]
  computeFitness1 graph loopIterMap target path branchDists @?= 2


testComputeFitnessLoop01 :: Assertion
testComputeFitnessLoop01 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test3.js"
  let loopIterMap = IntMap.fromList [(1,1)]
      path        = [0,1,2,3,-100]
      target      = -1
      branchDists = []
  computeFitness1 graph loopIterMap target path branchDists @?= 2
