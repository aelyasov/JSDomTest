module Analysis.CFG.Util.Tests (tests) where

import Test.Tasty (TestTree, testGroup, defaultMain, withResource)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, (@?=), runTestTT, Test(..), assertFailure)
import Analysis.CFG.Util
import Analysis.CFG.Data


tests :: TestTree
tests = testGroup "Analysis.CFG.Util"
        [ testCase "injectLoopsInPathSuccess"   injectLoopsInPathSuccess
        , testCase "injectLoopsInPathFailure"   injectLoopsInPathFailure
        , testCase "injectLoopsInGroupSuccess"  injectLoopsInGroupSucces
        , testCase "injectLoopsInGroupFailure"  injectLoopsInGroupFailure
        , testCase "injectLoopsInGroupsSuccess" injectLoopsInGroupsSuccess
        , testCase "injectLoopsInGroupsFailure" injectLoopsInGroupsFailure
        , testCase "mkLoopTransitiveClosure01"  mkLoopTransitiveClosure01
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
