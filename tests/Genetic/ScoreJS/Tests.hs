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
               [ testCase "testFitnessScore01"             testFitnessScore01
               , testCase "testFitnessScore02"             testFitnessScore02
               , testCase "testComputeFitnessSequence01"   testComputeFitnessSequence01
               , testCase "testComputeFitnessSequence02"   testComputeFitnessSequence02
               , testCase "testComputeFitnessSequence03"   testComputeFitnessSequence03
               , testCase "testComputeFitnessSequence04"   testComputeFitnessSequence04
               , testCase "testComputeFitnessBranch01"     testComputeFitnessBranch01
               , testCase "testComputeFitnessBranch02"     testComputeFitnessBranch02
               , testCase "testComputeFitnessBranch03"     testComputeFitnessBranch03
               , testCase "testComputeFitnessBranch04"     testComputeFitnessBranch04
               , testCase "testComputeFitnessBranch05"     testComputeFitnessBranch05
               , testCase "testComputeFitnessBranch06"     testComputeFitnessBranch06
               , testCase "testComputeFitnessBranch07"     testComputeFitnessBranch07
               , testCase "testComputeFitnessBranch08"     testComputeFitnessBranch08
               , testCase "testComputeFitnessBranch09"     testComputeFitnessBranch09
               , testCase "testComputeFitnessSingleLoop01" testComputeFitnessSingleLoop01
               , testCase "testComputeFitnessSingleLoop02" testComputeFitnessSingleLoop02
               , testCase "testComputeFitnessSingleLoop03" testComputeFitnessSingleLoop03
               , testCase "testComputeFitnessNestedLoop01" testComputeFitnessNestedLoop01
               , testCase "testComputeFitnessNestedLoop02" testComputeFitnessNestedLoop02
               , testCase "testComputeFitnessNestedLoop03" testComputeFitnessNestedLoop03
               , testCase "testComputeFitnessNestedLoop04" testComputeFitnessNestedLoop04
               , testCase "testComputeFitnessNestedLoop05" testComputeFitnessNestedLoop05
               , testCase "testComputeFitnessNestedLoop06" testComputeFitnessNestedLoop06
               , testCase "testComputeFitnessNestedLoop07" testComputeFitnessNestedLoop07
               , testCase "testComputeFitnessNestedLoop08" testComputeFitnessNestedLoop08
               , testCase "testComputeFitnessNestedLoop09" testComputeFitnessNestedLoop09
               , testCase "testComputeFitnessNestedLoop10" testComputeFitnessNestedLoop10
               , testCase "testComputeFitnessNestedLoop11" testComputeFitnessNestedLoop11
               , testCase "testComputeFitnessNestedLoop12" testComputeFitnessNestedLoop12
               , testCase "testComputeFitnessNestedLoop13" testComputeFitnessNestedLoop13
               , testCase "testComputeFitnessNestedLoop14" testComputeFitnessNestedLoop14
               , testCase "testComputeFitnessNestedLoop15" testComputeFitnessNestedLoop15
               , testCase "testComputeFitnessNestedLoop16" testComputeFitnessNestedLoop16
               , testCase "testComputeFitnessNestedLoop17" testComputeFitnessNestedLoop17
               , testCase "testComputeFitnessNestedLoop18" testComputeFitnessNestedLoop18
               , testCase "testComputeFitnessNestedLoop19" testComputeFitnessNestedLoop19
               , testCase "testComputeFitnessNestedLoop20" testComputeFitnessNestedLoop20
               , testCase "testComputeFitnessNestedLoop21" testComputeFitnessNestedLoop21  
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
  computeFitness graph loopIterMap target path branchDists @?= 0

  
testComputeFitnessSequence02 :: Assertion
testComputeFitnessSequence02 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test1.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,2,3,-100]
      target      = 3
      branchDists = []
  computeFitness graph loopIterMap target path branchDists @?= 1 

    
testComputeFitnessSequence03 :: Assertion
testComputeFitnessSequence03 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test1.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,2,-100]
      target      = 3
      branchDists = []
  computeFitness graph loopIterMap target path branchDists @?= 2    


testComputeFitnessSequence04 :: Assertion
testComputeFitnessSequence04 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test1.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,-100]
      target      = 3
      branchDists = []
  computeFitness graph loopIterMap target path branchDists @?= 3   


testComputeFitnessBranch01 :: Assertion
testComputeFitnessBranch01 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test2.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,2,3,-100]
      target      = 5
      branchDists = [BranchDist 1 1]
  computeFitness graph loopIterMap target path branchDists @?= 2.25   


testComputeFitnessBranch02 :: Assertion
testComputeFitnessBranch02 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test2.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,2,3]
      target      = 5
      branchDists = [BranchDist 1 1]
  computeFitness graph loopIterMap target path branchDists @?= 2.25   


testComputeFitnessBranch03 :: Assertion
testComputeFitnessBranch03 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test2.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,2]
      target      = 5
      branchDists = [BranchDist 1 1]
  computeFitness graph loopIterMap target path branchDists @?= 2.25


testComputeFitnessBranch04 :: Assertion
testComputeFitnessBranch04 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test2.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,2,3]
      target      = 5
      branchDists = [BranchDist 1 4]
  computeFitness graph loopIterMap target path branchDists @?= 2.4


testComputeFitnessBranch05 :: Assertion
testComputeFitnessBranch05 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test2.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,2,3]
      target      = 7
      branchDists = [BranchDist 1 4]
  computeFitness graph loopIterMap target path branchDists @?= 4.4


testComputeFitnessBranch06 :: Assertion
testComputeFitnessBranch06 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test2.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,4,5,6,7]
      target      = 9
      branchDists = [BranchDist 1 1, BranchDist 5 1]
  computeFitness graph loopIterMap target path branchDists @?= 2.25 


testComputeFitnessBranch07 :: Assertion
testComputeFitnessBranch07 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test2.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,4,5,6,7]
      target      = 9
      branchDists = [BranchDist 1 1, BranchDist 5 4]
  computeFitness graph loopIterMap target path branchDists @?= 2.4 


testComputeFitnessBranch08 :: Assertion
testComputeFitnessBranch08 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test2.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,4,5,8]
      target      = 9
      branchDists = [BranchDist 1 1, BranchDist 5 4]
  computeFitness graph loopIterMap target path branchDists @?= 1


testComputeFitnessBranch09 :: Assertion
testComputeFitnessBranch09 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test2.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,4,5,8,-100]
      target      = 9
      branchDists = [BranchDist 1 1, BranchDist 5 4]
  computeFitness graph loopIterMap target path branchDists @?= 2


testComputeFitnessSingleLoop01 :: Assertion
testComputeFitnessSingleLoop01 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test3.js"
  let loopIterMap = IntMap.fromList [(1,1)]
      path        = [0,1,2,3,-100]
      target      = -1
      branchDists = []
  computeFitness graph loopIterMap target path branchDists @?= 3


testComputeFitnessSingleLoop02 :: Assertion
testComputeFitnessSingleLoop02 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test3.js"
  let loopIterMap = IntMap.fromList [(1,1)]
      path        = [0,1,2,3,1,-100]
      target      = -1
      branchDists = []
  computeFitness graph loopIterMap target path branchDists @?= 2


testComputeFitnessSingleLoop03 :: Assertion
testComputeFitnessSingleLoop03 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test3.js"
  let loopIterMap = IntMap.fromList [(1,1)]
      path        = [0,1,2,3,1,-1]
      target      = -1
      branchDists = []
  computeFitness graph loopIterMap target path branchDists @?= 0


testComputeFitnessNestedLoop01 :: Assertion
testComputeFitnessNestedLoop01 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test4.js"
  let loopIterMap = IntMap.fromList [(1,2),(3,2)]
      path        = [0,-100]
      target      = -1
      branchDists = []
  computeFitness graph loopIterMap target path branchDists @?= 21


testComputeFitnessNestedLoop02 :: Assertion
testComputeFitnessNestedLoop02 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test4.js"
  let loopIterMap = IntMap.fromList [(1,2),(3,2)]
      path        = [0,1,-100]
      target      = -1
      branchDists = []
  computeFitness graph loopIterMap target path branchDists @?= 20


testComputeFitnessNestedLoop03 :: Assertion
testComputeFitnessNestedLoop03 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test4.js"
  let loopIterMap = IntMap.fromList [(1,2),(3,2)]
      path        = [0,1,2,-100]
      target      = -1
      branchDists = []
  computeFitness graph loopIterMap target path branchDists @?= 19


testComputeFitnessNestedLoop04 :: Assertion
testComputeFitnessNestedLoop04 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test4.js"
  let loopIterMap = IntMap.fromList [(1,2),(3,2)]
      path        = [0,1,2,3,-100]
      target      = -1
      branchDists = []
  computeFitness graph loopIterMap target path branchDists @?= 18


testComputeFitnessNestedLoop05 :: Assertion
testComputeFitnessNestedLoop05 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test4.js"
  let loopIterMap = IntMap.fromList [(1,2),(3,2)]
      path        = [0,1,2,3,4,-100]
      target      = -1
      branchDists = []
  computeFitness graph loopIterMap target path branchDists @?= 17  


testComputeFitnessNestedLoop06 :: Assertion
testComputeFitnessNestedLoop06 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test4.js"
  let loopIterMap = IntMap.fromList [(1,2),(3,2)]
      path        = [0,1,2,3,4,5,-100]
      target      = -1
      branchDists = []
  computeFitness graph loopIterMap target path branchDists @?= 16    


testComputeFitnessNestedLoop07 :: Assertion
testComputeFitnessNestedLoop07 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test4.js"
  let loopIterMap = IntMap.fromList [(1,2),(3,2)]
      path        = [0,1,2,3,4,5,3,-100]
      target      = -1
      branchDists = []
  computeFitness graph loopIterMap target path branchDists @?= 15


testComputeFitnessNestedLoop08 :: Assertion
testComputeFitnessNestedLoop08 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test4.js"
  let loopIterMap = IntMap.fromList [(1,2),(3,2)]
      path        = [0,1,2,3,4,5,3,4,-100]
      target      = -1
      branchDists = []
  computeFitness graph loopIterMap target path branchDists @?= 14      


testComputeFitnessNestedLoop09 :: Assertion
testComputeFitnessNestedLoop09 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test4.js"
  let loopIterMap = IntMap.fromList [(1,2),(3,2)]
      path        = [0,1,2,3,4,5,3,4,5,-100]
      target      = -1
      branchDists = []
  computeFitness graph loopIterMap target path branchDists @?= 13


testComputeFitnessNestedLoop10 :: Assertion
testComputeFitnessNestedLoop10 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test4.js"
  let loopIterMap = IntMap.fromList [(1,2),(3,2)]
      path        = [0,1,2,3,4,5,3,4,5,3,-100]
      target      = -1
      branchDists = []
  computeFitness graph loopIterMap target path branchDists @?= 12  


testComputeFitnessNestedLoop11 :: Assertion
testComputeFitnessNestedLoop11 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test4.js"
  let loopIterMap = IntMap.fromList [(1,2),(3,2)]
      path        = [0,1,2,3,4,5,3,4,5,3,1,-100]
      target      = -1
      branchDists = []
  computeFitness graph loopIterMap target path branchDists @?= 11


testComputeFitnessNestedLoop12 :: Assertion
testComputeFitnessNestedLoop12 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test4.js"
  let loopIterMap = IntMap.fromList [(1,2),(3,2)]
      path        = [0,1,2,3,4,5,3,4,5,3,1,2,-100]
      target      = -1
      branchDists = []
  computeFitness graph loopIterMap target path branchDists @?= 10


testComputeFitnessNestedLoop13 :: Assertion
testComputeFitnessNestedLoop13 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test4.js"
  let loopIterMap = IntMap.fromList [(1,2),(3,2)]
      path        = [0,1,2,3,4,5,3,4,5,3,1,2,3,-100]
      target      = -1
      branchDists = []
  computeFitness graph loopIterMap target path branchDists @?= 9


testComputeFitnessNestedLoop14 :: Assertion
testComputeFitnessNestedLoop14 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test4.js"
  let loopIterMap = IntMap.fromList [(1,2),(3,2)]
      path        = [0,1,2,3,4,5,3,4,5,3,1,2,3,4,-100]
      target      = -1
      branchDists = []
  computeFitness graph loopIterMap target path branchDists @?= 8


testComputeFitnessNestedLoop15 :: Assertion
testComputeFitnessNestedLoop15 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test4.js"
  let loopIterMap = IntMap.fromList [(1,2),(3,2)]
      path        = [0,1,2,3,4,5,3,4,5,3,1,2,3,4,5,-100]
      target      = -1
      branchDists = []
  computeFitness graph loopIterMap target path branchDists @?= 7


testComputeFitnessNestedLoop16 :: Assertion
testComputeFitnessNestedLoop16 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test4.js"
  let loopIterMap = IntMap.fromList [(1,2),(3,2)]
      path        = [0,1,2,3,4,5,3,4,5,3,1,2,3,4,5,3,-100]
      target      = -1
      branchDists = []
  computeFitness graph loopIterMap target path branchDists @?= 6


testComputeFitnessNestedLoop17 :: Assertion
testComputeFitnessNestedLoop17 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test4.js"
  let loopIterMap = IntMap.fromList [(1,2),(3,2)]
      path        = [0,1,2,3,4,5,3,4,5,3,1,2,3,4,5,3,4,-100]
      target      = -1
      branchDists = []
  computeFitness graph loopIterMap target path branchDists @?= 5


testComputeFitnessNestedLoop18 :: Assertion
testComputeFitnessNestedLoop18 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test4.js"
  let loopIterMap = IntMap.fromList [(1,2),(3,2)]
      path        = [0,1,2,3,4,5,3,4,5,3,1,2,3,4,5,3,4,5,-100]
      target      = -1
      branchDists = []
  computeFitness graph loopIterMap target path branchDists @?= 4


testComputeFitnessNestedLoop19 :: Assertion
testComputeFitnessNestedLoop19 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test4.js"
  let loopIterMap = IntMap.fromList [(1,2),(3,2)]
      path        = [0,1,2,3,4,5,3,4,5,3,1,2,3,4,5,3,4,5,3,-100]
      target      = -1
      branchDists = []
  computeFitness graph loopIterMap target path branchDists @?= 3               


testComputeFitnessNestedLoop20 :: Assertion
testComputeFitnessNestedLoop20 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test4.js"
  let loopIterMap = IntMap.fromList [(1,2),(3,2)]
      path        = [0,1,2,3,4,5,3,4,5,3,1,2,3,4,5,3,4,5,3,1,-100]
      target      = -1
      branchDists = []
  computeFitness graph loopIterMap target path branchDists @?= 2               


testComputeFitnessNestedLoop21 :: Assertion
testComputeFitnessNestedLoop21 = do
  graph <- mkTestCFG "tests/Genetic/ScoreJS/resources/test4.js"
  let loopIterMap = IntMap.fromList [(1,2),(3,2)]
      path        = [0,1,2,3,4,5,3,4,5,3,1,2,3,4,5,3,4,5,3,1,-1]
      target      = -1
      branchDists = []
  computeFitness graph loopIterMap target path branchDists @?= 0               
