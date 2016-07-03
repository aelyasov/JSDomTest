module Analysis.CFG.Fitness.Tests (tests) where

import Test.Tasty (TestTree, testGroup, defaultMain, withResource)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, (@?=), runTestTT, Test(..), assertFailure)
import Analysis.CFG.Fitness
import Analysis.CFG.Data
import Analysis.CFG.Build


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
        [ testCase "allCompletePaths2Target01" allCompletePaths2Target01
        , testCase "allCompletePaths2Target02" allCompletePaths2Target02
        , testCase "allCompletePaths2Target03" allCompletePaths2Target03
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
