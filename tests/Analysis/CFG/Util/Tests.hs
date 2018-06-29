module Analysis.CFG.Util.Tests (tests) where

import Test.Tasty (TestTree, testGroup, defaultMain, withResource)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, (@?=), runTestTT, Test(..), assertFailure)
import Analysis.CFG.Util
import Analysis.CFG.Data
import qualified Data.IntMap as IntMap
import Analysis.CFG.Build
import Data.Graph.Inductive

-- | Test import
import Data.Graph.Analysis.Algorithms.Commons 
import Control.Monad (liftM)
import Data.Graph.Inductive.Query
import Data.Tree (drawTree, drawForest)

tests :: TestTree
tests = testGroup "Analysis.CFG.Util"
        [ testCase "injectLoopsInPathSuccess"     injectLoopsInPathSuccess
        , testCase "injectLoopsInPathFailure"     injectLoopsInPathFailure
        , testCase "injectLoopsInGroupSuccess"    injectLoopsInGroupSucces
        , testCase "injectLoopsInGroupFailure"    injectLoopsInGroupFailure
        , testCase "injectLoopsInGroupsSuccess"   injectLoopsInGroupsSuccess
        , testCase "injectLoopsInGroupsFailure"   injectLoopsInGroupsFailure
        , testCase "mkLoopTransitiveClosure01"    mkLoopTransitiveClosure01
        , testCase "evalLoopTreeLeaf"             evalLoopTreeLeaf
        , testCase "evalLoopTreeNode"             evalLoopTreeNode
        , testCase "list2LoopTreeTest"            list2LoopTreeTest
        , testCase "insertOneLoopTreeInAnother01" insertOneLoopTreeInAnother01
        , testCase "insertOneLoopTreeInAnother02" insertOneLoopTreeInAnother02
        , testCase "insertOneLoopInLoops01"       insertOneLoopInLoops01
        , testCase "insertOneLoopInLoops02"       insertOneLoopInLoops02
        , testCase "insertLoopsInLoops01"         insertLoopsInLoops01
        , testCase "insertLoopsInAllLoops01"      insertLoopsInAllLoops01
        , testCase "insertAllLoopsInAllLoops01"   insertAllLoopsInAllLoops01
        , testCase "findAllLoopTreesInGraph01"    findAllLoopTreesInGraph01
        , testCase "findAllLoopTreesInGraph02"    findAllLoopTreesInGraph02
        , testCase "findAllLoopTreesInGraph03"    findAllLoopTreesInGraph03
        , testCase "findAllLoopTreesInGraph04"    findAllLoopTreesInGraph04
        , testCase "findAllLoopTreesInGraph05"    findAllLoopTreesInGraph05
        , testCase "buildLoopMaxSizeMap01"        buildLoopMaxSizeMap01
        , testCase "buildLoopMaxSizeMap02"        buildLoopMaxSizeMap02
        , testCase "buildLoopMaxSizeMap03"        buildLoopMaxSizeMap03
        , testCase "buildLoopMaxSizeMap04"        buildLoopMaxSizeMap04
        , testCase "buildLoopMaxSizeMap05"        buildLoopMaxSizeMap05
        , testCase "estimatePath01"               estimatePath01
        , testCase "estimatePath02"               estimatePath02
        , testCase "updateLoopIterMap01"          updateLoopIterMap01
        , testCase "updateLoopIterMap02"          updateLoopIterMap02
        , testCase "updateLoopIterMap03"          updateLoopIterMap03
        , testCase "updateLoopIterMap04"          updateLoopIterMap04
        , testCase "updateLoopIterMap05"          updateLoopIterMap05
        , testCase "updateLoopIterMap06"          updateLoopIterMap06
        , testCase "updateLoopIterMap07"          updateLoopIterMap07
        , testCase "updateLoopIterMap08"          updateLoopIterMap08
        , testCase "updateLoopIterMap09"          updateLoopIterMap09
        , testCase "updateLoopIterMap10"          updateLoopIterMap10
        , testCase "updateLoopIterMap11"          updateLoopIterMap11
        , testCase "updateLoopIterMap12"          updateLoopIterMap12
        , testCase "countLoopIterations01"        countLoopIterations01
        , testCase "countLoopIterations02"        countLoopIterations02
        ]


injectLoopsInPathSuccess :: Assertion
injectLoopsInPathSuccess =
  injectLoopsInPath groupOfLoops pathToInjectLoops @?= (injectedLoops, isInjected)
  where
    groupOfLoops      = [[1,2],[1,3]]
    pathToInjectLoops = [0,1,4]
    injectedLoops     = [[0,1,2,4],[0,1,3,4]]
    isInjected        = True


injectLoopsInPathFailure :: Assertion
injectLoopsInPathFailure =
  injectLoopsInPath groupOfLoops pathToInjectLoops @?= (injectedLoops, isInjected)
  where
    groupOfLoops      = [[5,2],[5,3]]
    pathToInjectLoops = [0,1,4]
    injectedLoops     = [[0,1,4]]
    isInjected        = False


injectLoopsInGroupSucces :: Assertion
injectLoopsInGroupSucces =
  injectLoopsInGroup groupOfLoops pathsToInjectLoops @?= (injectedLoops, isInjected)
  where
    groupOfLoops       = [[1,2],[1,3]]
    pathsToInjectLoops = [[0,1,4],[0,1,5],[6,7]]
    injectedLoops      = [[0,1,2,4],[0,1,3,4],[0,1,2,5],[0,1,3,5],[6,7]]
    isInjected         = True


injectLoopsInGroupFailure :: Assertion
injectLoopsInGroupFailure =
  injectLoopsInGroup groupOfLoops pathsToInjectLoops @?= (injectedLoops, isInjected)
  where
    groupOfLoops       = [[1,2],[1,3]]
    pathsToInjectLoops = [[6,7],[6,8]]
    injectedLoops      = [[6,7],[6,8]]
    isInjected         = False    


injectLoopsInGroupsSuccess :: Assertion
injectLoopsInGroupsSuccess = injectLoopsInGroups groupOfLoops groupOfPathes @?= injectedLoops
  where
    groupOfLoops  = [[1,2],[1,3]]
    groupOfPathes = [[[6,7],[6,8]],[[0,1,4],[0,1,5]]]
    injectedLoops = [[[6,7],[6,8]],[[0,1,2,4],[0,1,3,4],[0,1,2,5],[0,1,3,5]]]


injectLoopsInGroupsFailure :: Assertion
injectLoopsInGroupsFailure = injectLoopsInGroups groupOfLoops groupOfPathes @?= injectedLoops
  where
    groupOfLoops  = [[1,2],[1,3]]
    groupOfPathes = [[[6,7],[6,8]],[[0,4],[0,5]]]
    injectedLoops = [[[6,7],[6,8]],[[0,4],[0,5]]]   


mkLoopTransitiveClosure01 :: Assertion
mkLoopTransitiveClosure01 = mkLoopTransitiveClosure loops @?= result
  where
    loops  = [[[1,2],[1,3]],[[4,1,5],[4,1,6]],[[7,4,8]]]
    result = [[[7,4,1,2,5,8],[7,4,1,3,5,8],[7,4,1,2,6,8],[7,4,1,3,6,8]]]


evalLoopTreeLeaf :: Assertion
evalLoopTreeLeaf = evalLoopTree loopTree @?= result
  where
    loopTree = Leaf 10
    result   = 1 

evalLoopTreeNode :: Assertion
evalLoopTreeNode = evalLoopTree loopTree @?= result
  where
    loopTree = Node 2 5 [Leaf 10, Node 3 6 [Leaf 100]]    
    result   = 19 

testLoopIterationMap :: LoopIterationMap
testLoopIterationMap = IntMap.fromList [(1,11), (2, 12), (3, 13)]


list2LoopTreeTest :: Assertion
list2LoopTreeTest = list2LoopTree testLoopIterationMap testLoop @?= loopTree
  where
    testLoop = [1,4,5]
    loopTree = Node 11 1 [Leaf 4, Leaf 5]


insertOneLoopTreeInAnother01 :: Assertion
insertOneLoopTreeInAnother01 = insertOneLoopTreeInAnother oneTree anotherTree @?= (resultTree, isInserted)
  where
    oneTree     = Node 10 1 [Leaf 2, Leaf 3]
    anotherTree = Node 11 2 [Leaf 1, Leaf 5]
    resultTree  = Node 11 2 [oneTree, Leaf 5]
    isInserted  = True 


insertOneLoopTreeInAnother02 :: Assertion
insertOneLoopTreeInAnother02 = insertOneLoopTreeInAnother oneTree anotherTree @?= (resultTree, isInserted)
  where
    oneTree     = Node 10 1 [Leaf 2, Leaf 3]
    anotherTree = Node 11 2 [Leaf 4, Leaf 5]
    resultTree  = anotherTree
    isInserted  = False


insertOneLoopInLoops01 :: Assertion
insertOneLoopInLoops01 = insertOneLoopInLoops loopTree loopTrees @?= (resultLoops, isInserted) 
  where
    loopTree    = Node 10 1 [Leaf 2, Leaf 3] 
    loopTrees   = [Node 11 2 [Leaf 1, Leaf 5], Node 12 2 [Leaf 5, Node 13 6 [Leaf 1, Leaf 7]]]
    resultLoops = [Node 11 2 [loopTree, Leaf 5], Node 12 2 [Leaf 5, Node 13 6 [loopTree, Leaf 7]]]
    isInserted  = True


insertOneLoopInLoops02 :: Assertion
insertOneLoopInLoops02 = insertOneLoopInLoops loopTree loopTrees @?= (resultLoops, isInserted) 
  where
    loopTree    = Node 10 1 [Leaf 2, Leaf 3] 
    loopTrees   = [Node 11 2 [Leaf 4, Leaf 5], Node 12 2 [Leaf 5, Node 13 6 [Leaf 4, Leaf 7]]]
    resultLoops = loopTrees
    isInserted  = False    


insertLoopsInLoops01 :: Assertion
insertLoopsInLoops01 = insertLoopsInLoops loopsToInsert loopsWhereInsert @?= (resultLoops, isInserted)
  where
    loopsToInsert    = [Node 10 1 [Leaf 2, Leaf 3], Node 11 1 [Leaf 4, Leaf 5]]
    loopsWhereInsert = [Node 11 2 [Leaf 1, Leaf 5], Node 12 2 [Leaf 5, Node 13 6 [Leaf 1, Leaf 7]]]
    resultLoops      = [ Node 11 2 [loopsToInsert !! 0, Leaf 5]
                       , Node 12 2 [Leaf 5, Node 13 6 [loopsToInsert !! 0, Leaf 7]]
                       , Node 11 2 [loopsToInsert !! 1, Leaf 5]
                       , Node 12 2 [Leaf 5, Node 13 6 [loopsToInsert !! 1, Leaf 7]]]
    isInserted       = True


insertLoopsInAllLoops01 :: Assertion
insertLoopsInAllLoops01 = insertLoopsInAllLoops loopsToInsert allLoops @?= resultLoops
  where
    loopsToInsert    = [Node 10 1 [Leaf 2, Leaf 3], Node 11 1 [Leaf 4, Leaf 5]]
    allLoops         = [[Node 11 2 [Leaf 0, Leaf 5]], [Node 12 2 [Leaf 5, Node 13 6 [Leaf 1, Leaf 7]]]]
    resultLoops      = [ [Node 11 2 [Leaf 0, Leaf 5]]
                       , [ Node 12 2 [Leaf 5, Node 13 6 [loopsToInsert !! 0, Leaf 7]]
                         , Node 12 2 [Leaf 5, Node 13 6 [loopsToInsert !! 1, Leaf 7]]
                         ]
                       ]
    

insertAllLoopsInAllLoops01 :: Assertion
insertAllLoopsInAllLoops01 = insertAllLoopsInAllLoops allLoops @?= resultLoops
  where
    allLoops    = [[Node 11 1 [Leaf 0, Leaf 5]], [Node 12 2 [Leaf 5, Node 13 6 [Leaf 1, Leaf 7]]]]
    resultLoops = [[Node 11 1 [Leaf 0, Leaf 5]], [Node 12 2 [Leaf 5, Node 13 6 [Node 11 1 [Leaf 0, Leaf 5], Leaf 7]]]]
    

findAllLoopTreesInGraph01 :: Assertion
findAllLoopTreesInGraph01 = do
  graph <- mkTestCFG "./tests/Analysis/CFG/Util/resources/test1.js"
  let loopIterationMap = IntMap.fromList []
  findAllLoopTreesInGraph graph loopIterationMap @?= []


findAllLoopTreesInGraph02 :: Assertion
findAllLoopTreesInGraph02 = do
  graph <- mkTestCFG "./tests/Analysis/CFG/Util/resources/test2.js"
  let loopIterationMap = IntMap.fromList [(3, 10)]
  findAllLoopTreesInGraph graph loopIterationMap @?= [[Node 10 3 [Leaf 4]]]


findAllLoopTreesInGraph03 :: Assertion
findAllLoopTreesInGraph03 = do
  graph <- mkTestCFG "./tests/Analysis/CFG/Util/resources/test3.js"
  let loopIterationMap = IntMap.fromList [(1, 10), (3, 11)]
  findAllLoopTreesInGraph graph loopIterationMap @?= [[Node 11 3 [Leaf 4]],[Node 10 1 [Leaf 2,Node 11 3 [Leaf 4]]]]


findAllLoopTreesInGraph04 :: Assertion
findAllLoopTreesInGraph04 = do
  graph <- mkTestCFG "./tests/Analysis/CFG/Util/resources/test4.js"
  let loopIterationMap = IntMap.fromList [(1, 10), (5, 11), (8, 12)]
  findAllLoopTreesInGraph graph loopIterationMap @?=
    [ [ Node 12 8 [Leaf 9] ]
    , [ Node 11 5 [Leaf 6] ]
    , [Node 10 1 [Leaf 2,Leaf 3,Leaf 7,Node 12 8 [Leaf 9]],Node 10 1 [Leaf 2,Leaf 3,Leaf 4,Node 11 5 [Leaf 6]]]]


findAllLoopTreesInGraph05 :: Assertion
findAllLoopTreesInGraph05 = do
  graph <- mkTestCFG "./tests/Analysis/CFG/Util/resources/test5.js"
  let loopIterationMap = IntMap.fromList [(1, 10), (5, 11), (7, 12), (10, 13), (14, 14), (17, 15)]
  findAllLoopTreesInGraph graph loopIterationMap @?=
    [[ Node 15 17 [Leaf 18] ],
     [ Node 14 14 [Leaf 15] ],
     [ Node 13 10 [Leaf 11,Leaf 12,Leaf 16,Node 15 17 [Leaf 18] ]
     , Node 13 10 [Leaf 11,Leaf 12,Leaf 13,Node 14 14 [Leaf 15]] ],
     [ Node 12 7 [Leaf 8] ],
     [ Node 11 5 [Leaf 6,Node 12 7 [Leaf 8]] ],
     [ Node 10 1 [Leaf 2,Leaf 3,Leaf 9,Node 13 10 [Leaf 11,Leaf 12,Leaf 16,Node 15 17 [Leaf 18]]]
     , Node 10 1 [Leaf 2,Leaf 3,Leaf 4,Node 11 5 [Leaf 6,Node 12 7 [Leaf 8]]]
     , Node 10 1 [Leaf 2,Leaf 3,Leaf 9,Node 13 10 [Leaf 11,Leaf 12,Leaf 13,Node 14 14 [Leaf 15]]]]
    ]


buildLoopMaxSizeMap01 :: Assertion
buildLoopMaxSizeMap01 = do
  graph <- mkTestCFG "./tests/Analysis/CFG/Util/resources/test1.js"
  let loopIterationMap = IntMap.fromList []
  buildLoopMaxSizeMap graph loopIterationMap @?= IntMap.fromList []


buildLoopMaxSizeMap02 :: Assertion
buildLoopMaxSizeMap02 = do
  graph <- mkTestCFG "./tests/Analysis/CFG/Util/resources/test2.js"
  let loopIterationMap = IntMap.fromList [(3, 10)]
  buildLoopMaxSizeMap graph loopIterationMap @?= IntMap.fromList [(3,21)]


buildLoopMaxSizeMap03 :: Assertion
buildLoopMaxSizeMap03 = do
  graph <- mkTestCFG "./tests/Analysis/CFG/Util/resources/test3.js"
  let loopIterationMap = IntMap.fromList [(1, 10), (3, 11)]
  buildLoopMaxSizeMap graph loopIterationMap @?= IntMap.fromList [(1,251),(3,23)]


buildLoopMaxSizeMap04 :: Assertion
buildLoopMaxSizeMap04 = do
  graph <- mkTestCFG "./tests/Analysis/CFG/Util/resources/test4.js"
  let loopIterationMap = IntMap.fromList [(1, 10), (5, 11), (8, 12)]
  buildLoopMaxSizeMap graph loopIterationMap @?= IntMap.fromList [(1,291),(5,23),(8,25)]


buildLoopMaxSizeMap05 :: Assertion
buildLoopMaxSizeMap05 = do
  graph <- mkTestCFG "./tests/Analysis/CFG/Util/resources/test5.js"
  let loopIterationMap = IntMap.fromList [(1, 10), (5, 11), (7, 12), (10, 13), (14, 14), (17, 15)]
  buildLoopMaxSizeMap graph loopIterationMap @?= IntMap.fromList [(1,4601),(5,298),(7,25),(10,456),(14,29),(17,31)] 

estimatePath01 :: Assertion
estimatePath01 =
  let path    = [0,1,2]
      loopMap = IntMap.fromList [(1,5)]
  in  estimatePath path loopMap @?= 7


estimatePath02 :: Assertion
estimatePath02 =
  let path    = [0,1,2,3,4]
      loopMap = IntMap.fromList [(1,5), (3, 2)]
  in  estimatePath path loopMap @?= 10


updateLoopIterMap01 :: Assertion
updateLoopIterMap01 =
  let iterMap      = IntMap.fromList [(1,5),(2,10)]
      iterCountMap = IntMap.fromList [(1,6),(2,11)]
      path         = [0,1,3,1,3]
      newIterMap   = IntMap.fromList [(1,3),(2,10)]
  in  updateLoopIterMap iterMap iterCountMap path @?= newIterMap


updateLoopIterMap02 :: Assertion
updateLoopIterMap02 =
  let iterMap    = IntMap.fromList [(1,2),(3,2)]
      iterCountMap = IntMap.fromList [(1,3),(3,6)]
      path       = [0,1,3,4,3,4,3,1,3,4,3,4,3,1,5,6]
      newIterMap = IntMap.fromList [(1,0),(3,0)]
  in  updateLoopIterMap iterMap iterCountMap path @?= newIterMap      


updateLoopIterMap03 :: Assertion
updateLoopIterMap03 =
  let iterMap    = IntMap.fromList [(1,2),(3,2),(7,2)]
      iterCountMap = IntMap.fromList [(1,3),(3,6)]
      path       = [0]
      newIterMap = IntMap.fromList [(1,2),(3,2),(7,2)]
  in  updateLoopIterMap iterMap iterCountMap path @?= newIterMap


updateLoopIterMap04 :: Assertion
updateLoopIterMap04 =
  let iterMap    = IntMap.fromList [(1,2),(3,2),(7,2)]
      iterCountMap = IntMap.fromList [(1,3),(3,6)]
      path       = [0,1]
      newIterMap = IntMap.fromList [(1,1),(3,2),(7,2)]
  in  updateLoopIterMap iterMap iterCountMap path @?= newIterMap


updateLoopIterMap05 :: Assertion
updateLoopIterMap05 =
  let iterMap    = IntMap.fromList [(1,2),(3,2),(7,2)]
      iterCountMap = IntMap.fromList [(1,3),(3,6)]
      path       = [0,1,3]
      newIterMap = IntMap.fromList [(1,1),(3,1),(7,2)]
  in  updateLoopIterMap iterMap iterCountMap path @?= newIterMap


updateLoopIterMap06 :: Assertion
updateLoopIterMap06 =
  let iterMap    = IntMap.fromList [(1,2),(3,2),(7,2)]
      iterCountMap = IntMap.fromList [(1,3),(3,6)]
      path       = [0,1,3,3]
      newIterMap = IntMap.fromList [(1,1),(3,0),(7,2)]
  in  updateLoopIterMap iterMap iterCountMap path @?= newIterMap


updateLoopIterMap07 :: Assertion
updateLoopIterMap07 =
  let iterMap    = IntMap.fromList [(1,2),(3,2),(7,2)]
      iterCountMap = IntMap.fromList [(1,3),(3,6)]
      path       = [0,1,3,3,3]
      newIterMap = IntMap.fromList [(1,1),(3,2),(7,2)]
  in  updateLoopIterMap iterMap iterCountMap path @?= newIterMap


updateLoopIterMap08 :: Assertion
updateLoopIterMap08 =
  let iterMap    = IntMap.fromList [(1,2),(3,2),(7,2)]
      iterCountMap = IntMap.fromList [(1,3),(3,6)]
      path       = [0,1,3,3,3,1]
      newIterMap = IntMap.fromList [(1,0),(3,2),(7,2)]
  in  updateLoopIterMap iterMap iterCountMap path @?= newIterMap


updateLoopIterMap09 :: Assertion
updateLoopIterMap09 =
  let iterMap    = IntMap.fromList [(1,2),(3,2),(7,2)]
      iterCountMap = IntMap.fromList [(1,3),(3,6)]
      path       = [0,1,3,3,3,1,3]
      newIterMap = IntMap.fromList [(1,0),(3,1),(7,2)]
  in  updateLoopIterMap iterMap iterCountMap path @?= newIterMap


updateLoopIterMap10 :: Assertion
updateLoopIterMap10 =
  let iterMap    = IntMap.fromList [(1,2),(3,2),(7,2)]
      iterCountMap = IntMap.fromList [(1,3),(3,6)]
      path       = [0,1,3,3,3,1,3,3]
      newIterMap = IntMap.fromList [(1,0),(3,0),(7,2)]
  in  updateLoopIterMap iterMap iterCountMap path @?= newIterMap


updateLoopIterMap11 :: Assertion
updateLoopIterMap11 =
  let iterMap    = IntMap.fromList [(1,2),(3,2),(7,2)]
      iterCountMap = IntMap.fromList [(1,3),(3,6)]
      path       = [0,1,3,3,3,1,3,3,3]
      newIterMap = IntMap.fromList [(1,0),(3,0),(7,2)]
  in  updateLoopIterMap iterMap iterCountMap path @?= newIterMap


updateLoopIterMap12 :: Assertion
updateLoopIterMap12 =
  let iterMap    = IntMap.fromList [(1,2),(3,2),(7,2)]
      iterCountMap = IntMap.fromList [(1,3),(3,6)]
      path       = [0,1,3,3,3,1,3,3,3,1]
      newIterMap = IntMap.fromList [(1,0),(3,0),(7,2)]
  in  updateLoopIterMap iterMap iterCountMap path @?= newIterMap


countLoopIterations01 :: Assertion
countLoopIterations01 = do
  graph <- mkTestCFG "./tests/Analysis/CFG/Util/resources/test2.js"
  let iterMap      = IntMap.fromList [(3, 5)]
      iterCountMap = IntMap.fromList [(3, 6)]
  countLoopIterations graph iterMap @?= iterCountMap


countLoopIterations02 :: Assertion
countLoopIterations02 = do
  graph <- mkTestCFG "./tests/Analysis/CFG/Util/resources/test3.js"
  let iterMap      = IntMap.fromList [(1, 4), (3, 5)]
      iterCountMap = IntMap.fromList [(1, 5), (3, 24)]
  countLoopIterations graph iterMap @?= iterCountMap  

