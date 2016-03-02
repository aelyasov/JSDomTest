module Analysis.CFG.Fitness (computeCfgLevel) where

import Data.Graph.Inductive.Graph (LNode, LEdge, match, outdeg)
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Basic (grev)
import Data.Graph.Analysis.Algorithms.Common (pathTree)
import Analysis.CFG.Data (GPath, NLab, SLab, ELab)
import Safe (maximumByNote, headNote, at)
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Function (on)


allCompletePaths2Target :: Gr NLab ELab -> SLab -> [GPath]
allCompletePaths2Target graph target =
  let reverseGraph = grev graph
      paths2Target = pathTree $ match target reverseGraph
      completePaths2Target = filter ((0==) . last) paths2Target
  in  completePaths2Target


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
      maxProblemPath = maximumByNote "in approachLevel" (compare `on` length) paths2ProblemNodes
      problemNode = maxProblemPath `at` 0
      level = length maxProblemPath - 1
      isException = let (_, pathFromProblemNode) = break (==problemNode) run
                    in  (length pathFromProblemNode > 1) && (pathFromProblemNode `at` 1 == -100)
  in  (level, problemNode, isException)
