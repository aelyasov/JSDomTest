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
import System.Log.Logger (rootLoggerName, infoM, debugM, noticeM)
import Data.Configurator (load, Worth(..), require)
import Analysis.Static (removeDuplicates)
import Text.XML.Statistics
import Safe (headNote)
import Data.Maybe (fromMaybe)
import Data.List (intercalate)

import Debug.Trace

import GA.GA

replaceElemInList :: Int -> a -> [a] -> [a]
replaceElemInList i x as = let (pre, post) = splitAt i as
                           in pre ++ x:tail post


-- | sig = [JS_DOM,JS_STRING]
-- | pool = ([], ["node"], ([TAG_IFRAME], ["node"], [], []))
instance Entity [JSArg] Double Target (JSSig, JSCPool) IO where

  genRandom pool@(sig, env) seed = do
    debugM rootLoggerName $ "Generating random population for the signature: " ++ show sig
    let uniqueEnv = removeDuplicates env
    args <- mapM (genRandomVal uniqueEnv) sig
    return args

  crossover pool _ seed args1 args2 = do
    let gen        = mkStdGen seed
        pairedArgs = zip args1 args2
        crossArgId = fst $ randomR (0, length pairedArgs - 1) gen
    debugM rootLoggerName $ "Crossing over arguments: " ++ show args1 ++ " and " ++ show args2
    debugM rootLoggerName $ "Crossover point is: " ++ show crossArgId
    crossArg  <- crossoverJSArgs gen (pairedArgs!!crossArgId)
    crossArgs <- liftM ([args1, args2]!!) $ randomRIO (0, 1)
    let cresult = replaceElemInList crossArgId crossArg crossArgs
    debugM rootLoggerName $ "Crossed arguments:  " ++ show cresult
    return $ Just cresult
          
  mutation (sig, pool) _ seed args = do
    -- let gen       = mkStdGen seed
    --     typedArgs = zip sig args
    --     mutArgId  = fst $ randomR (0, length typedArgs - 1) gen      
    -- debugM rootLoggerName $ "Mutating arguments: " ++ show args
    -- debugM rootLoggerName $ "Mutation point is: " ++ show mutArgId
    -- mutArg <- mutateJSArg (typedArgs!!mutArgId) gen pool
    -- let mresult = replaceElemInList mutArgId mutArg args
    -- debugM rootLoggerName $ "Mutated arguments:  " ++ show mresult

    mresult <- genRandom (sig, pool) seed
    return $ Just mresult

  score = fitnessScore

  isPerfect (_,s) = s == 0.0
  -- isPerfect (_,s) = s < 1.0

  showGeneration gi (pop,archive) = showBestEntity
                                    -- ++ "\nArchive Statistics:\n"
                                    -- ++ showArchive archive
    --                                 ++ "\nPopulation statistics:\n"
    --                                 ++ showPopulation pop
    where
      showBestEntity = "Best entity (gen. " ++ show gi ++ ") fitness: " ++ show fitness
      (Just fitness, e) = headNote "showGeneration" archive
      -- showArchive = intercalate "\n------------\n" . map showScoredEntity 
      -- showScoredEntity (f, p) = "fitness: " ++ showFitness f ++ "\n" ++ showEntity p
      -- showEntity = showStatistics
      -- showFitness = show . fromMaybe (-1)
      -- showPopulation = intercalate "\n------------\n" . map showStatistics


readGenetcAlgConfig :: IO (Int, Int, Int, Float, Float, Float, Float, Bool, Bool)
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
  return (population, archive, generations, crossoverRate, mutationRate, crossoverParam,mutationParam, checkpointing, rescorearchive)
  
  

runGenetic :: Target -> (JSSig, JSCPool) -> IO [JSArg]
runGenetic target pool@(sig, (intP, stringP, (tagP, idP, nameP, classP))) = do
  (population, archive, generations, crossoverRate, mutationRate, crossoverParam, mutationParam, checkpointing, rescorearchive) <- readGenetcAlgConfig
  let conf = GAConfig population archive generations crossoverRate mutationRate crossoverParam mutationParam checkpointing rescorearchive 
      g = mkStdGen 0            
  es <- evolveVerbose g conf pool target
  -- es <- evolve g conf pool target
  noticeM rootLoggerName $ "Best fitness value: " ++ (show $ fst $ head es)
  return $ snd $ head es


readRandomAlgConfig :: IO Int
readRandomAlgConfig = do
  config <- load [ Required "jsdomtest.cfg"]
  budget <- require config "random.budget"
  return budget
  

runRandom :: Target -> (JSSig, JSCPool) -> IO [JSArg]
runRandom target pool  = do
  budget <- readRandomAlgConfig
  es <- randomSearch (mkStdGen 0) budget pool target
  putStrLn $ "Best fitness value: " ++ (show $ fst $ head es)
  return $ snd $ head es


data Algorithm = GEN | RAND
               deriving (Show, Read)

runAlgorithm :: Algorithm ->  Target -> (JSSig, JSCPool) -> IO [JSArg]
runAlgorithm GEN  = runGenetic
runAlgorithm RAND = runRandom
