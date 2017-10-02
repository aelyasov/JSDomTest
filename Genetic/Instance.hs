{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Genetic.Instance (runAlgorithm, Algorithm(..)) where

import Genetic.DataJS
import Genetic.RandomJS
import Genetic.CrossoverJS (crossoverJSArgs)
import Genetic.ScoreJS
import Genetic.MutationJS (mutateJSArg)

import Html5C.Tags

import Control.Monad
import System.Random
-- import qualified Data.ByteString as BS
import System.Log.Logger (rootLoggerName, infoM, debugM, noticeM, criticalM)
import Data.Configurator (load, Worth(..), require)
import Analysis.Static (removeDuplicates)
import Text.XML.Statistics
import Safe (headNote)
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Analysis.CFG.Util (replaceElemInList)
import Data.Graph.Inductive.Graph (LEdge)
import Analysis.CFG.Data (ELab)

import Debug.Trace

import GA.GA

-- | sig = [JS_DOM,JS_STRING]
-- | pool = ([], ["node"], ([TAG_IFRAME], ["node"], [], []))
instance Entity [JSArg] ScoredPath Target (JSSig, JSCPool) IO where

  genRandom (sig, pool) seed = do
    debugM rootLoggerName $ "Generating random population for the signature: " ++ show sig
    let pool' = removeDuplicates pool
    args <- mapM (genRandomVal pool') sig
    return args

  crossover _ _ seed args1 args2 = do
    let gen        = mkStdGen seed
        pairedArgs = zip args1 args2
        crossArgId = fst $ randomR (0, length pairedArgs - 1) gen
    debugM rootLoggerName $ "Crossing over arguments: " ++ show args1 ++ " and " ++ show args2
    debugM rootLoggerName $ "Crossover point is: " ++ show crossArgId
    crossArg  <- crossoverJSArgs gen (pairedArgs!!crossArgId)
    crossArgs <- liftM ([args1, args2]!!) $ randomRIO (0, 1)
    let cresult = replaceElemInList crossArgId (Just crossArg) crossArgs
    debugM rootLoggerName $ "Crossed arguments:  " ++ show cresult
    return $ Just cresult
          
  mutation (sig, pool) _ seed args = do
    let uniquePool = removeDuplicates pool
        gen        = mkStdGen seed
        mutId      = fst $ randomR (0, length args - 1) gen
        arg        = args !! mutId
        typ        = sig !! mutId 
    debugM rootLoggerName $ "Mutating arguments: " ++ show args
    debugM rootLoggerName $ "Mutation point is: " ++ show mutId
    mutArg <- mutateJSArg arg typ gen uniquePool
    let mresult = replaceElemInList mutId (Just mutArg) args
    debugM rootLoggerName $ "Mutated arguments:  " ++ show mresult
    return $ Just mresult

  score = fitnessScore

  isPerfect (_,scoredPath) = (sum $ scores scoredPath) == 0.0
  -- isPerfect (_,s) = s < 1.0

  showGeneration gi (pop,archive) = if (null archive) then "Archive is empty" else showBestEntity
                                    -- ++ "\nArchive Statistics:\n"
                                    -- ++ showArchive archive
    --                                 ++ "\nPopulation statistics:\n"
    --                                 ++ showPopulation pop
    where
      showBestEntity = "Best entity (gen. " ++ show gi ++ ") fitness: " ++ show fitness ++ "\n" ++ (show e) ++ "\n" ++ (showStatistics e) 
      (Just fitness, e) = headNote "showGeneration" archive
      -- showArchive = intercalate "\n------------\n" . map showScoredEntity 
      -- showScoredEntity (f, p) = "fitness: " ++ showFitness f ++ "\n" ++ showEntity p
      -- showEntity = showStatistics
      -- showFitness = show . fromMaybe (-1)
      -- showPopulation = intercalate "\n------------\n" . map showStatistics
      
  hasConverged size archives = (length archives >= size) && (hasConvergedAll $ take size archives)
    where
      hasConvergedAll archs = allTheSame $ concat [ [ scores $ fromMaybe (ScoredPath [] []) $ fst se
                                                    | se <- ar]
                                                  | ar <- archs ]
      allTheSame :: (Eq a) => [a] -> Bool
      allTheSame xs = all (== head xs) (tail xs)



readGenetcAlgConfig :: IO (Int, Int, Int, Float, Float, Float, Float, Bool, Bool, Int, Bool)
readGenetcAlgConfig = do
  config         <- load [ Required "jsdomtest.cfg"]
  population     <- require config "genetic.population"
  archive        <- require config "genetic.archive"
  generations    <- require config "genetic.generations"
  crossoverRate  <- require config "genetic.crossover_rate"
  mutationRate   <- require config "genetic.mutation_rate"
  crossoverParam <- require config "genetic.crossover_param"
  mutationParam  <- require config "genetic.mutation_param"
  checkpointing  <- require config "genetic.checkpointing"
  rescorearchive <- require config "genetic.rescorearchive"
  convergeSize   <- require config "genetic.converge_archive_size"
  checkConverge  <- require config "genetic.check_converge"
  return ( population
         , archive
         , generations
         , crossoverRate
         , mutationRate
         , crossoverParam
         , mutationParam
         , checkpointing
         , rescorearchive
         , convergeSize
         , checkConverge)
  

runGenetic :: Target -> (JSSig, JSCPool) -> [(Int, LEdge ELab)] -> IO [JSArg]
runGenetic target@(Target cfg targetPath) pool@(sig, (intP, floatP, stringP, (tagP, idP, nameP, classP))) branches = do
  ( population,
    archive,
    generations,
    crossoverRate,
    mutationRate,
    crossoverParam,
    mutationParam,
    checkpointing,
    rescorearchive,
    convergeSize,
    checkConverge) <- readGenetcAlgConfig
  criticalM rootLoggerName "Genetic algorithm configurations"
  criticalM rootLoggerName $ "genetic.population: "     ++ show population
  criticalM rootLoggerName $ "genetic.archive: "        ++ show archive
  criticalM rootLoggerName $ "genetic.generations: "    ++ show generations
  criticalM rootLoggerName $ "genetic.crossover_rate: " ++ show crossoverRate
  criticalM rootLoggerName $ "genetic.mutation_rate: "  ++ show mutationRate
  let config = GAConfig population
                        archive
                        generations
                        crossoverRate
                        mutationRate
                        crossoverParam
                        mutationParam
                        checkpointing
                        rescorearchive
                        convergeSize
                        checkConverge
      gen    = mkStdGen 0
  evolveVerboseUntilArchiveConverged 0 gen config pool (Target cfg targetPath) branches
      

evolveVerboseUntilArchiveConverged :: Int
                                   -> StdGen
                                   -> GAConfig
                                   -> (JSSig, JSCPool)
                                   -> Target
                                   -> [(Int, LEdge ELab)]
                                   -> IO [JSArg]
evolveVerboseUntilArchiveConverged gi gen config pool (Target cfg targetPath) branches = do
    (resArchive, hasConverged, giLast) <- evolveVerbose gi gen config pool (Target cfg targetPath)
    let (Just (ScoredPath fitnessVals execPath), bestScoredEntry) = head resArchive
    if hasConverged
      then do let newTargetPath = updateTargetPath execPath targetPath branches
                  giNew         = giLast + 1
                  newConfig     = if newTargetPath == targetPath
                                  then config{checkArchiveConvergence = False}
                                  else config
                  targetNew     = Target cfg $ trace ("newTargetPath: " ++ show newTargetPath) newTargetPath
              evolveVerboseUntilArchiveConverged giNew gen (trace (show newConfig) newConfig) pool targetNew branches
      else do criticalM rootLoggerName $ "Best fitness value: " ++ show fitnessVals
              criticalM rootLoggerName $ "Best entity (GA): " ++ show bestScoredEntry
              return bestScoredEntry

  
readRandomAlgConfig :: IO Int
readRandomAlgConfig = do
  config <- load [ Required "jsdomtest.cfg"]
  budget <- require config "random.budget"
  return budget
  

runRandom :: Target -> (JSSig, JSCPool) -> [(Int, LEdge ELab)] -> IO [JSArg]
runRandom target pool _  = do
  budget <- readRandomAlgConfig
  es <- randomSearch (mkStdGen 0) budget pool target
  putStrLn $ "Best fitness value: " ++ (show $ fst $ head es)
  return $ snd $ head es


data Algorithm = GEN | RAND
               deriving (Show, Read)

runAlgorithm :: Algorithm ->  Target -> (JSSig, JSCPool) -> [(Int, LEdge ELab)] -> IO [JSArg]
runAlgorithm GEN  = runGenetic
runAlgorithm RAND = runRandom
