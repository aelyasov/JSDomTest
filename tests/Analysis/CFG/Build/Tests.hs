module Analysis.CFG.Build.Tests
       ( tests
       ) where


import Analysis.CFG.Build
import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, (@?=), runTestTT, Test(..))
import Data.Graph.Inductive 


-- | Run whole test suite: defaultMain tests
tests :: TestTree
tests = testGroup "Analysis.CFG.Build"
        [ testCase "testFindSourceBranch01" testFindSourceBranch01
        , testCase "testApproachLevel01"    testApproachLevel01
        , testCase "testApproachLevel02"    testApproachLevel02
        , testCase "testApproachLevel03"    testApproachLevel03
        , testCase "testApproachLevel04"    testApproachLevel04
        , testCase "testMkTestCFG01"        testMkTestCFG01
        , testCase "testMkTestCFG02"        testMkTestCFG02  
        ]

testGr1 :: Gr NLab ELab
testGr1 = mkGraph grNds1 grEds1
grNds1 = mkNds [-1 .. 3]
grEds1 = mkEds [(0,1), (1,2), (2,3), (3,-1)]

testGr2 :: Gr NLab ELab
testGr2 = mkGraph grNds2 grEds2
grNds2 = mkNds [-1 .. 5]
grEds2 = mkEds [(0,1), (1,2), (1,3), (2,4), (2,5), (4,-1), (5,-1), (3,-1)]

mkNds = map (\i -> (i, ""))
mkEds = map (\(i, j) -> (i, j, ""))

testFindSourceBranch01 :: Assertion
testFindSourceBranch01 = findSourceBranch testGr1 1 [0, -1] @?= []

testApproachLevel01 :: Assertion
testApproachLevel01 = approachLevel testGr2 2 [0, 1, 3, -1] @?= (0, 1, 1)

testApproachLevel02 :: Assertion
testApproachLevel02 = approachLevel testGr2 4 [0, 1, 3, -1] @?= (1, 2, 1)

testApproachLevel03 :: Assertion
testApproachLevel03 = approachLevel testGr2 3 [0, 1, 2, 4, -1] @?= (0, 1, 1)

testApproachLevel04 :: Assertion
testApproachLevel04 = approachLevel testGr1 2 [0, 1, 2, -1] @?= (0, 0, 2)


-- | Example how to verify assertion: runTestTT $ TestCase testMkTestCFG01
testMkTestCFG01 :: Assertion 
testMkTestCFG01 = do
  graph <- mkTestCFG "./tests/Analysis/CFG/Build/resources/test1.js"
  let nodes = [(0,"entry"),(-1,"exit"),(1,"1: if (true)"),(2,"2: block"),(3,"3: if (true)"),(4,"4: block"),(5,"5: block"),(6,"6: block")]
      edges = [(0,1,""),(1,2,"then"),(1,6,"else"),(2,3,""),(3,4,"then"),(3,5,"else"),(4,-1,""),(5,-1,""),(6,-1,"")]
      result = mkGraph nodes edges
  graph @?= result

testMkTestCFG02 :: Assertion 
testMkTestCFG02 = do
  graph <- mkTestCFG "./tests/Analysis/CFG/Build/resources/test2.js"
  let nodes = [(0,"entry"),(-1,"exit"),(1,"1: if (a)"),(2,"2: block"),(3,"3: while (b)"),(4,"4: block"),(5,"5: block"),(6,"6: x")]
      edges = [(0,1,""),(1,2,"then"),(1,5,"else"),(2,3,""),(3,4,"inWhile"),(3,6,"outWhile"),(4,3,""),(5,6,""),(6,-1,"")]
      result = mkGraph nodes edges
  graph @?= result  
