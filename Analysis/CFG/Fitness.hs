module Analysis.CFG.Fitness where

import Data.Graph.Inductive.Graph (LNode, LEdge, match, outdeg)
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Basic (grev)
import Data.Graph.Analysis.Algorithms.Common (pathTree)
import Analysis.CFG.Data (GPath, NLab, SLab, ELab)
import Safe (minimumByNote, at)
import Data.List (nub)
import Data.Maybe (mapMaybe)

-- | The findSourceBranch function is invoked with the argument representing path equal to the revirsed actual path
findSourceBranch :: Gr NLab ELab -> SLab -> GPath -> [GPath]
findSourceBranch gr tL run = 
    let rgr   = grev gr
        paths = pathTree $ match tL rgr
        pathsFromTarget = filter ((0==) . last) paths 
        fstRunIntersect r p = let (pref, suf) =  break (`elem`r)  p
                              in  if suf == [0] then Nothing else Just ((head suf):reverse pref)
    in nub $ mapMaybe (fstRunIntersect run) pathsFromTarget


compApproachLevel :: Gr NLab ELab -> SLab -> GPath -> (Int, Int, SLab)
compApproachLevel gr tL path = (distance, length path - 1, fstDeviationPoint) 
    where
      distance          = max (foldr foldBranch 0 path - 1) 0
      fstDeviationPoint = path `at` 0
      foldBranch x xs   = if (isBranch x) && (isCritical x)
                          then xs + 1
                          else xs           
      isBranch   n      = let out = outdeg gr n in out `notElem` [0,1]
      isCritical n      = let paths = filter (((-1)==) . last) $ pathTree $ match n gr 
                          in  or $ map (tL`notElem`) paths


approachLevel :: Gr NLab ELab -> SLab -> GPath -> (Int, Int, SLab)
approachLevel gr tL run = let sourceBranches = findSourceBranch gr tL run
                          in  minimumByNote "in approachLevel"
                              (\(a1, p1, _) (a2, p2, _) -> (a1 + p1) `compare` (a2 + p2))
                              $ map (compApproachLevel gr tL) sourceBranches
