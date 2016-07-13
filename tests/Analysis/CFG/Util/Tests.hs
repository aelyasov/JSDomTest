module Analysis.CFG.Util.Tests (tests) where

import Test.Tasty (TestTree, testGroup, defaultMain, withResource)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, (@?=), runTestTT, Test(..), assertFailure)
import Analysis.CFG.Util
import Analysis.CFG.Data
import qualified Data.IntMap as IntMap
import Analysis.CFG.Build

-- | Test import
import Data.Graph.Analysis.Algorithms.Commons (cyclesIn')
import Control.Monad (liftM)


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
        , testCase "updateLoopIterMap01"          updateLoopIterMap01
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
    result   = 16 

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
  buildLoopMaxSizeMap graph loopIterationMap @?= IntMap.fromList [(3,20)]


buildLoopMaxSizeMap03 :: Assertion
buildLoopMaxSizeMap03 = do
  graph <- mkTestCFG "./tests/Analysis/CFG/Util/resources/test3.js"
  let loopIterationMap = IntMap.fromList [(1, 10), (3, 11)]
  buildLoopMaxSizeMap graph loopIterationMap @?= IntMap.fromList [(1,240),(3,22)]


buildLoopMaxSizeMap04 :: Assertion
buildLoopMaxSizeMap04 = do
  graph <- mkTestCFG "./tests/Analysis/CFG/Util/resources/test4.js"
  let loopIterationMap = IntMap.fromList [(1, 10), (5, 11), (8, 12)]
  buildLoopMaxSizeMap graph loopIterationMap @?= IntMap.fromList [(1,280),(5,22),(8,24)]


buildLoopMaxSizeMap05 :: Assertion
buildLoopMaxSizeMap05 = do
  graph <- mkTestCFG "./tests/Analysis/CFG/Util/resources/test5.js"
  let loopIterationMap = IntMap.fromList [(1, 10), (5, 11), (7, 12), (10, 13), (14, 14), (17, 15)]
  buildLoopMaxSizeMap graph loopIterationMap @?= IntMap.fromList [(1,4460),(5,286),(7,24),(10,442),(14,28),(17,30)]


estimatePath01 :: Assertion
estimatePath01 = estimatePath [0,1,2] (IntMap.fromList [(1,5)]) @?= 7


updateLoopIterMap01 :: Assertion
updateLoopIterMap01 = updateLoopIterMap (IntMap.fromList [(1,5),(2,10)]) [0,1,3,1,3] @?= (IntMap.fromList [(1,5),(2,10)])
