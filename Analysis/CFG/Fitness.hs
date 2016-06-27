-- module Analysis.CFG.Fitness (computeCfgLevel, distanceToExit) where
module Analysis.CFG.Fitness where

import Data.Graph.Inductive.Graph (LNode, LEdge, match, outdeg, mkGraph)
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Basic (grev)
import Data.Graph.Analysis.Algorithms.Commons (pathTree, cyclesIn')
import Analysis.CFG.Data
import Safe (maximumByNote, headNote, at)
import Data.List (nub, groupBy, find, findIndex)
import Data.Maybe (mapMaybe)
import Control.Applicative ((<|>))
import Data.Function (on)
import Debug.Trace

import Analysis.CFG.Util 
import Analysis.CFG.Build
import Control.Monad



-- | Given CFG, reversed execution path an target node the function returns distanses from problem node to target
findPathToTarget :: Gr NLab ELab -> GPath -> SLab -> Maybe [GPath]
findPathToTarget graph path target = find (not . null) $  map (findAllPathsBetweenTwoNodes graph target) path


findAllPathsBetweenTwoNodes :: Gr NLab ELab -> SLab -> SLab -> [GPath]
findAllPathsBetweenTwoNodes graph target start = filter (\path -> target == last path) $ pathTree $ match start graph



allCompletePaths2Target :: Gr NLab ELab -> SLab -> [GPath]
allCompletePaths2Target graph target =
  let reverseGraph         = grev graph
      paths2Target         = pathTree $ match target reverseGraph
      completePaths2Target = filter ((0==) . last) paths2Target
      cycles               = cyclesIn' graph
      cycleGroups          = groupBy ((==) `on` last) $ map reverse  cycles  
      result = mkLoopTransitiveClosure $ reverse (completePaths2Target:cycleGroups)
  in -- completePaths2Target
   concat result


findPathFromProblemNode2Target :: GPath -> GPath -> Maybe GPath
findPathFromProblemNode2Target run path =
  let (pref, suf) =  break (`elem`run)  path
  in  if suf == [0] then Nothing else Just ((head suf):reverse pref)  


pathsFromProblemNodes2Target :: Gr NLab ELab -> SLab -> GPath -> [GPath]
pathsFromProblemNodes2Target graph target run =
  let completePaths2Target = allCompletePaths2Target graph target
  in  nub $ mapMaybe (findPathFromProblemNode2Target run) completePaths2Target
  

computeCfgLevel :: Gr NLab ELab -> SLab -> GPath -> (Int, SLab, Bool)
computeCfgLevel graph target run =
  let paths2ProblemNodes = pathsFromProblemNodes2Target graph target run
      maxProblemPath = maximumByNote "computeCfgLevel" (compare `on` length) paths2ProblemNodes
      problemNode = maxProblemPath `at` 0
      level = length maxProblemPath - 1
      isException = let (_, pathFromProblemNode) = break (==problemNode) run
                    in  (length pathFromProblemNode > 1) && (pathFromProblemNode `at` 1 == -100)
  in  (level, problemNode, isException)


distanceToExit :: Gr NLab ELab -> GPath -> Int
distanceToExit graph run =
  let lastNode     = run!!(length run - 2)
      paths2Exit   = filter ((-1==) . last) $ pathTree $ match lastNode graph
      maxPath2Exit = maximumByNote "distanceToExit" (compare `on` length) paths2Exit
  in  if (last run == -1)
      then 0
      else (length maxPath2Exit) - 1
           

testGr :: Gr NLab ELab
testGr = mkGraph grNodes grEdges  
grNodes = [(0,"entry"),(-1,"exit"),(1,"funcdecl"),(2,"2: if true"),(3,"3: block"),(4,"4: n++"),(5,"5: if true"),(6,"6: block"),(7,"7: n++"),(8,"8: block"),(9,"9: n++")]
grEdges = [(0,1,""),(1,2,""),(2,3,""),(2,5,""),(3,4,""),(4,-1,""),(5,6,""),(5,8,""),(6,7,""),(7,-1,""),(8,9,""),(9,-1,"")]

test = liftM
       (mkLoopTransitiveClosure . reverse . groupBy ((==) `on` head) . cyclesIn')
       $ mkTestCFG  "./examples/apps/sudoku/revealAll.js"

