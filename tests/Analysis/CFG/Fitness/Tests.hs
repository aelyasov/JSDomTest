module Analysis.CFG.Fitness.Tests (tests) where

import Test.Tasty (TestTree, testGroup, defaultMain, withResource)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, (@?=), runTestTT, Test(..), assertFailure)
import Analysis.CFG.Fitness
import Analysis.CFG.Data
import Analysis.CFG.Build
import qualified Data.IntMap as IntMap


-- | Importr required for interanl module testing
import Control.Monad
import Data.Graph.Analysis.Algorithms.Commons (pathTree, cyclesIn')
import Data.Graph.Inductive.Basic (grev)
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph (LNode, LEdge, match, outdeg, mkGraph)
import Data.List (nub, groupBy, find, findIndex)
import Data.Function (on)
import Analysis.CFG.Util 


tests :: TestTree
tests = testGroup "Analysis.CFG.Fitness"
        [-- testCase "allCompletePaths2Target01"     allCompletePaths2Target01
        -- , testCase "allCompletePaths2Target02"     allCompletePaths2Target02
        -- , testCase "allCompletePaths2Target03"     allCompletePaths2Target03
          testCase "findAllPathsBetweenTwoNodes01" findAllPathsBetweenTwoNodes01
        , testCase "findAllPathsBetweenTwoNodes02" findAllPathsBetweenTwoNodes02
        , testCase "findAllPathsBetweenTwoNodes03" findAllPathsBetweenTwoNodes03
        , testCase "findAllPathsBetweenTwoNodes04" findAllPathsBetweenTwoNodes04
        , testCase "findAllPathToTarget01"         findAllPathToTarget01
        , testCase "estimateAllPath01"             estimateAllPath01
        , testCase "estimateAllPath02"             estimateAllPath02
        , testCase "estimateAllPath03"             estimateAllPath03
        , testCase "estimateAllPath04"             estimateAllPath04
        , testCase "estimateAllPath05"             estimateAllPath05
        , testCase "getShortestPathsToTarget01"    getShortestPathsToTarget01
        , testCase "computeRealCfgLevel01"         computeRealCfgLevel01
        , testCase "estimateAllPathSingleLoop01"   estimateAllPathSingleLoop01
        , testCase "estimateAllPathSingleLoop02"   estimateAllPathSingleLoop02  
        ]


allCompletePaths2Target01 :: Assertion
allCompletePaths2Target01 = do
    graph <- mkTestCFG "./tests/Analysis/CFG/Fitness/resources/test1.js"
    allCompletePaths2Target graph (-1) @?= [[-1,2,1,0],[-1,3,1,0]]

allCompletePaths2Target02 :: Assertion
allCompletePaths2Target02 = do
    graph <- mkTestCFG "./tests/Analysis/CFG/Fitness/resources/test1.js"
    allCompletePaths2Target graph 1 @?= [[1,0]]

allCompletePaths2Target03 :: Assertion
allCompletePaths2Target03 = do
    graph <- mkTestCFG "./tests/Analysis/CFG/Fitness/resources/test2.js"
    allCompletePaths2Target graph (2) @?= []    


findAllPathsBetweenTwoNodes01 :: Assertion
findAllPathsBetweenTwoNodes01 = do
  graph <- mkTestCFG "tests/Analysis/CFG/Fitness/resources/test2.js"
  let startNode  = 1
      targetNode = -1
  findAllPathsBetweenTwoNodes graph targetNode startNode @?= [[1,10,-1]]

findAllPathsBetweenTwoNodes02 :: Assertion
findAllPathsBetweenTwoNodes02 = do
  graph <- mkTestCFG "tests/Analysis/CFG/Fitness/resources/test2.js"
  let startNode  = 1
      targetNode = 7
  findAllPathsBetweenTwoNodes graph targetNode startNode @?= [[1,2,3,4,5,6,7]]


findAllPathsBetweenTwoNodes03 :: Assertion
findAllPathsBetweenTwoNodes03 = do
  graph <- mkTestCFG "tests/Analysis/CFG/Fitness/resources/test2.js"
  let startNode  = 7
      targetNode = -1
  findAllPathsBetweenTwoNodes graph targetNode startNode @?= [[7,5,1,10,-1]]


findAllPathsBetweenTwoNodes04 :: Assertion
findAllPathsBetweenTwoNodes04 = do
  graph <- mkTestCFG "tests/Analysis/CFG/Fitness/resources/test2.js"
  let startNode  = 7
      targetNode = 9
  findAllPathsBetweenTwoNodes graph targetNode startNode @?= [[7,5,1,2,3,8,9]]


findAllPathToTarget01 :: Assertion
findAllPathToTarget01 = do
  graph <- mkTestCFG "tests/Analysis/CFG/Fitness/resources/test2.js"
  let path       = [0,1,2]
      targetNode = -1 
  findAllPathToTarget graph path targetNode @?=
    [([0,1,10,-1],[0]),([1,10,-1],[0,1]),([2,3,4,5,1,10,-1],[0,1,2]),([2,3,8,9,1,10,-1],[0,1,2])]


estimateAllPath01 :: Assertion 
estimateAllPath01 = do
  graph <- mkTestCFG "tests/Analysis/CFG/Fitness/resources/test5.js"
  let loopIterMap = IntMap.fromList [(1,2)]
      path        = [0,1,2,3,1,2,3,1,-1]
      target      = -1
  estimateAllPath graph loopIterMap path target @?= [([0,1,-1],9),([1,-1],8),([2,3,1,-1],7),([3,1,-1],6),([1,-1],5),([2,3,1,-1],4),([3,1,-1],3),([1,-1],2),([-1],1)]
    

estimateAllPath02 :: Assertion 
estimateAllPath02 = do
  graph <- mkTestCFG "tests/Analysis/CFG/Fitness/resources/test3.js"
  let loopIterMap = IntMap.fromList [(1,1),(3,1)]
      path        = [0,1,2,3,4,5,3,1,-1]
      target      = -1
  estimateAllPath graph loopIterMap path target @?= [([0,1,-1],9),([1,-1],8),([2,3,1,-1],7),([3,1,-1],6),([4,5,3,1,-1],5),([5,3,1,-1],4),([3,1,-1],3),([1,-1],2),([-1],1)]
    

estimateAllPath03 :: Assertion 
estimateAllPath03 = do
  graph <- mkTestCFG "tests/Analysis/CFG/Fitness/resources/test4.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,2,3]
      target      = 9
  estimateAllPath graph loopIterMap path target @?= [([0,1,4,5,8,9],6),([1,4,5,8,9],5)]    


estimateAllPath04 :: Assertion 
estimateAllPath04 = do
  graph <- mkTestCFG "tests/Analysis/CFG/Fitness/resources/test4.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,4,5,6,7]
      target      = 9
  estimateAllPath graph loopIterMap path target @?= [([0,1,4,5,8,9],6),([1,4,5,8,9],5),([4,5,8,9],4),([5,8,9],3)]


estimateAllPath05 :: Assertion 
estimateAllPath05 = do
  graph <- mkTestCFG "tests/Analysis/CFG/Fitness/resources/test3.js"
  let loopIterMap = IntMap.fromList [(1,1),(3,1)]
      path        = [0,1,2,3,4,5,3,1,-1]
      target      = -1
  estimateAllPath graph loopIterMap path target @?= [([0,1,-1],9),([1,-1],8),([2,3,1,-1],7),([3,1,-1],6),([4,5,3,1,-1],5),([5,3,1,-1],4),([3,1,-1],3),([1,-1],2),([-1],1)]
  

getShortestPathsToTarget01 :: Assertion
getShortestPathsToTarget01 = do
  graph <- mkTestCFG "tests/Analysis/CFG/Fitness/resources/test4.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,4,5,6,7]
      target      = 9
  getShortestsPathsToTarget graph loopIterMap path target @?= [([5,8,9],3)]


computeRealCfgLevel01 :: Assertion
computeRealCfgLevel01 = do
  graph <- mkTestCFG "tests/Analysis/CFG/Fitness/resources/test4.js"
  let loopIterMap = IntMap.fromList []
      path        = [0,1,4,5,6,7]
      target      = 9
  computeRealCfgLevel graph loopIterMap path target @?= [(3, 5)]
  

estimateAllPathSingleLoop01 :: Assertion 
estimateAllPathSingleLoop01 = do
  graph <- mkTestCFG "tests/Analysis/CFG/Fitness/resources/test5.js"
  let loopIterMap = IntMap.fromList [(1,1)]
      path        = [0,1,2,3]
      target      = -1
  estimateAllPath graph loopIterMap path target @?= [([0,1,-1],6),([1,-1],5),([2,3,1,-1],4),([3,1,-1],3)]


estimateAllPathSingleLoop02 :: Assertion 
estimateAllPathSingleLoop02 = do
  graph <- mkTestCFG "tests/Analysis/CFG/Fitness/resources/test5.js"
  let loopIterMap = IntMap.fromList [(1,1)]
      path        = [0,1,2,3,1]
      target      = -1
  estimateAllPath graph loopIterMap path target @?= [([0,1,-1],6),([1,-1],5),([2,3,1,-1],4),([3,1,-1],3),([1,-1],2)]
