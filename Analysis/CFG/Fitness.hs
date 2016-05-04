-- module Analysis.CFG.Fitness (computeCfgLevel, distanceToExit) where
module Analysis.CFG.Fitness where

import Data.Graph.Inductive.Graph (LNode, LEdge, match, outdeg, mkGraph)
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Basic (grev)
import Data.Graph.Analysis.Algorithms.Common (pathTree, cyclesIn')
import Analysis.CFG.Data (GPath, NLab, SLab, ELab)
import Safe (maximumByNote, headNote, at)
import Data.List (nub, groupBy)
import Data.Maybe (mapMaybe)
import Data.Function (on)
import Debug.Trace


import Analysis.CFG.Build
import Control.Monad


allCompletePaths2Target :: Gr NLab ELab -> SLab -> [GPath]
allCompletePaths2Target graph target =
  let reverseGraph         = grev graph
      paths2Target         = pathTree $ match target reverseGraph
      completePaths2Target = filter ((0==) . last) paths2Target
      cycles               = cyclesIn' graph
      cycleGroups          = groupBy ((==) `on` last) $ map reverse $ trace (show cycles) cycles  
      result = mkLoopTransitiveClosure $ reverse ((trace (show completePaths2Target) completePaths2Target):(trace (show cycleGroups) cycleGroups))
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


mkLoopTransitiveClosure :: [[GPath]] -> [[GPath]]
mkLoopTransitiveClosure [loop]       = [loop]
mkLoopTransitiveClosure (loop:loops) = mkLoopTransitiveClosure (injectLoopsInGroups loop loops)

injectLoopsInPath :: [GPath] -> GPath -> ([GPath], Bool) 
injectLoopsInPath loops path =
  let leadElem = last $ head loops
      (pref, suff) = break (==leadElem) path
  in  if (null suff)
      then ([path], False)
      else (map (\l -> pref ++ [head suff] ++ l ++ tail suff) loops, True)

injectLoopsInGroup :: [GPath] -> [GPath] -> ([GPath], Bool)
injectLoopsInGroup loops group =
  let (group' , cond) = unzip $ map (injectLoopsInPath loops) group
  in  (concat group', or cond)

injectLoopsInGroups :: [GPath] -> [[GPath]] -> [[GPath]]       
injectLoopsInGroups _ [] = []
injectLoopsInGroups loops (group:groups) =
  let (group', cond) =  injectLoopsInGroup loops group
  in  if cond
      then group':groups
      else group:injectLoopsInGroups loops groups
           


testGr :: Gr NLab ELab
testGr = mkGraph grNodes grEdges  
grNodes = [(0,"entry"),(-1,"exit"),(1,"funcdecl"),(2,"2: if true"),(3,"3: block"),(4,"4: n++"),(5,"5: if true"),(6,"6: block"),(7,"7: n++"),(8,"8: block"),(9,"9: n++")]
grEdges = [(0,1,""),(1,2,""),(2,3,""),(2,5,""),(3,4,""),(4,-1,""),(5,6,""),(5,8,""),(6,7,""),(7,-1,""),(8,9,""),(9,-1,"")]

test = liftM
       (mkLoopTransitiveClosure . reverse . groupBy ((==) `on` head) . cyclesIn')
       $ mkTestCFG  "./examples/apps/sudoku/revealAll.js"
