module Genetic.CrossoverJS.Tests (tests) where

tests :: TestTree
tests = testGroup "Genetic.CrossoverJS"
        [ testCase "testFindSourceBranch01" testFindSourceBranch01  
        ]
