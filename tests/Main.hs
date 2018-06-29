module Main
       ( main
       ) where

import Test.Tasty (defaultMain, testGroup, withResource)
import qualified Analysis.CFG.Build.Tests

main = defaultMain $ testGroup "Tests"
       [ Analysis.CFG.Build.Tests.tests 
       ]
