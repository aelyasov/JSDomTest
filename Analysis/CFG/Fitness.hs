module Analysis.CFG.Fitness where

import Data.Graph.Inductive.Graph (LNode, LEdge, match, outdeg, mkGraph)
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Basic (grev)
import Data.Graph.Analysis.Algorithms.Commons (pathTree, cyclesIn')
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Analysis.CFG.Data
import Safe (maximumByNote, headNote, at, tailNote)
import Data.List (nub, groupBy, find, findIndex, inits, minimumBy, sortBy)
import Data.Maybe (mapMaybe, fromJust)
import Data.Ord 
import Control.Applicative ((<|>))
import Data.Function (on)
import Debug.Trace

import Analysis.CFG.Util 
import Analysis.CFG.Build
import Control.Monad
import Genetic.DataJS
import System.Log.Logger (rootLoggerName, infoM, debugM, noticeM)


computeFitness :: Gr NLab ELab -> LoopIterationMap -> SLab -> GPath -> [BranchDist] -> IO Double
computeFitness cfg loopIterMap target path distances = 
  if False -- (length path >= maxExecutionLength)
  then computeFitnessExecutionLengthExceeded target path 
  else computeFitnessExecutionLengthNotExceeded cfg loopIterMap target path distances
  where
    maxExecutionLength :: Int
    maxExecutionLength = 250

    fitnessValueExecutionLengthExceeded :: Double
    fitnessValueExecutionLengthExceeded = 100.0 


    computeFitnessExecutionLengthNotExceeded :: Gr NLab ELab
                                             -> LoopIterationMap
                                             -> SLab
                                             -> GPath
                                             -> [BranchDist]
                                             -> IO Double
    computeFitnessExecutionLengthNotExceeded cfg loopIterMap target path distances = do
      let (cfgLevel, problemNode) = computeRealCfgLevelOne cfg loopIterMap path target
          branchDistances  = filter ((problemNode==) . getBrLab) distances
          branchLevel      = if null branchDistances
                             then 0
                             else getBrDist $ minimumBy (comparing getBrDist) branchDistances
          normBrLevel      = branchDistNormalize branchLevel
          isException      = last path == (-100) && ((last $ init path) == problemNode)
          problemNodeLevel = if isException then 1 else (0.5 * normBrLevel)
          fitnessVal       = if target `elem` path then 0 else fromIntegral (cfgLevel - 1) + problemNodeLevel
          logger           = rootLoggerName
      infoM logger $ "Computing fitness value for the location #" ++ show target    
      infoM logger $ "ProblemNode: " ++ show problemNode
      infoM logger $ "CfgLevel: " ++ show cfgLevel
      infoM logger $ "Branch level: " ++ show branchLevel
      infoM logger $ "ProblemNodeLevel: " ++ show problemNodeLevel
      infoM logger $ "Fitness value for the location #" ++ show target ++ ": " ++ show fitnessVal 
      return fitnessVal

    computeFitnessExecutionLengthExceeded :: SLab -> GPath -> IO Double
    computeFitnessExecutionLengthExceeded target path = do
      infoM rootLoggerName $ "Execution path has exceeded maximum permitted length: " ++ show maxExecutionLength
      infoM rootLoggerName $ "Fitness value for location #" ++ show target ++ ": " ++ show fitnessValueExecutionLengthExceeded
      return fitnessValueExecutionLengthExceeded


branchDistNormalize :: Int -> Double
branchDistNormalize b = fromIntegral b / fromIntegral (b+1)

computeRealCfgLevelOne :: Gr NLab ELab -> LoopIterationMap -> GPath -> SLab -> (Int, SLab)
computeRealCfgLevelOne graph initIterMap path target =
  headNote "computeRealCfgLevelOne" $ computeRealCfgLevel graph initIterMap path target
  
  
computeRealCfgLevel :: Gr NLab ELab -> LoopIterationMap -> GPath -> SLab -> [(Int, SLab)]
computeRealCfgLevel graph initIterMap path target = map cfgLevelAndProblemNode shortestPathsToTarget
  where
    shortestPathsToTarget = getShortestsPathsToTarget graph initIterMap path target
    cfgLevelAndProblemNode (pToTarget, sz) = (sz, headNote "computeRealCfgLevel" pToTarget)
    

getShortestsPathsToTarget :: Gr NLab ELab -> LoopIterationMap -> GPath -> SLab -> [(GPath, Int)]
getShortestsPathsToTarget graph initIterMap path target =
  headNote "getShortestsPathsToTarget" $ groupBy ((==) `on` snd) $ sortBy (comparing snd) allPaths
  where
    allPaths = estimateAllPath graph initIterMap path target


estimateAllPath :: Gr NLab ELab -> LoopIterationMap -> GPath -> SLab -> [(GPath, Int)]
estimateAllPath graph initIterMap path target = -- trace ("\ninitIterMap: " ++ (show initIterMap)) $
                                                map estimatePathToTaget allPathsToTarget
  where
    allPathsToTarget         = findAllPathToTarget graph path target
    loopIterMap              = countLoopIterations graph initIterMap
    updatedLoopIterMap ppath = updateLoopIterMap initIterMap loopIterMap ppath
    loopMaxSizeMap     ppath =
      -- trace ("\nactualLoopMap: " ++ (show actualLoopMap))
      computeLoopMaxSizeMap graph initIterMap actualLoopMap
      where
        actualLoopMap = updatedLoopIterMap $ init ppath
    estimatePathToTaget (pathToTg, ppath) =
      -- trace ("\n-------------------------\n" ++
      --        "pathToTg: " ++ (show pathToTg) ++ "\n" ++
      --        "ppath: "    ++ (show ppath)    ++ "\n" ++
      --        "loopMaxSizeMp: " ++ (show loopMaxSizeMp) ++ "\n" ++
      --        "pathToTgEstim: " ++ (show pathToTgEstim) 
      --       )
      (pathToTg, pathToTgEstim)
      where
        loopMaxSizeMp = loopMaxSizeMap ppath
        pathToTgEstim = estimatePath pathToTg loopMaxSizeMp


findAllPathToTarget :: Gr NLab ELab -> GPath -> SLab -> [(GPath, GPath)]
findAllPathToTarget graph path target =
  filter (not . null . fst) $
  concatMap (\p -> zip (findAllPathsBetweenTwoNodes graph target $ last p) (repeat p))
  $ tailNote "findAllPathToTarget"
  $ inits path


findPathToTarget :: Gr NLab ELab -> GPath -> SLab -> Maybe [GPath]
findPathToTarget graph path target = find (not . null)
                                     $ map (findAllPathsBetweenTwoNodes graph target) path


findAllPathsBetweenTwoNodes :: Gr NLab ELab -> SLab -> SLab -> [GPath]
findAllPathsBetweenTwoNodes graph target start =
  filter (\path -> target == last path) $ pathTree $ match start graph


-----------------------------------------------------------
--   Code below is currently not used
-----------------------------------------------------------

allCompletePaths2Target :: Gr NLab ELab -> SLab -> [GPath]
allCompletePaths2Target graph target =
  let reverseGraph         = grev graph
      paths2Target         = pathTree $ match target reverseGraph
      completePaths2Target = filter ((0==) . last) paths2Target
      cycles               = cyclesIn' graph
      cycleGroups          = groupBy ((==) `on` head) cycles  
      result = mkLoopTransitiveClosure $ reverse (map reverse completePaths2Target):cycleGroups
  in concat result


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

