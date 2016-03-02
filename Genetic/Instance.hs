{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Genetic.Instance (runAlgorithm, Algorithm(..)) where

import Genetic.DataJS
import Genetic.RandomJS
import Genetic.CrossoverJS
import Genetic.ScoreJS
import Genetic.MutationJS (mutateHtml, mutateJSInt, mutateJSString)

import Html5C.Tags

import Control.Monad
import System.Random
-- import qualified Data.ByteString as BS
import System.Log.Logger (rootLoggerName, infoM, debugM)
import Data.Configurator (load, Worth(..), require)

import GA.GA


-- | sig = [JS_DOM,JS_STRING]
-- | pool = ([], ["node"], ([TAG_IFRAME], ["node"], [], []))
instance Entity [JSArg] Double Target (JSSig, JSCPool) IO where

  genRandom pool@(sig, env) seed = do
    debugM rootLoggerName $ "Generating random population for a signature: " ++ (show sig) ++ "\n" ++ (show env)
    args <- mapM (genRandomVal env) sig
    return args

  crossover pool _ seed e1 e2 = do
    liftM Just $ crossAllArgs gen cps
    where
      gen = mkStdGen seed
      cps = zipWith (\x y -> [x,y]) e1 e2

      crossAllArgs :: StdGen -> [[JSArg]] -> IO [JSArg]
      crossAllArgs g [] = return []
      crossAllArgs g (arg:args) = do 
        let (a, g')  = random g :: (Int, StdGen)
        d <- case arg of
               [DomJS d1, DomJS d2] -> liftM DomJS $ crossoverHTML g d1 d2
               [IntJS i1, IntJS i2] -> liftM (IntJS . ([i1,i2]!!)) $ randomRIO (0, 1)
               [StringJS i1, StringJS i2] -> liftM (StringJS . ([i1,i2]!!)) $ randomRIO (0, 1)
               otherwise -> error "crossover of non-DOM elements isn't defined"
               -- otherwise            -> return $ arg!!(a `mod` 2) 
        args' <- crossAllArgs g' args
        return (d:args')
          
  mutation (sig, pool) p seed e = liftM Just $ mutateAllArgs gen e
    where
      gen = mkStdGen seed
      mutateAllArgs :: StdGen -> [JSArg] -> IO [JSArg]
      mutateAllArgs g [] = return []
      mutateAllArgs g (arg:args) = do
        let (a, g')  = random g :: (Int, StdGen)
        d <- case arg of
              DomJS d1   -> liftM DomJS $ mutateHtml g d1
              IntJS _    -> mutateJSInt pool
              StringJS _ -> mutateJSString pool
              otherwise  ->  error "mutation of non-DOM elements isn't defined"
        args' <- mutateAllArgs g' args
        return (d:args')      

  score = fitnessScore

  isPerfect (_,s) = s == 0.0


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
  putStrLn $ "Best fitness value: " ++ (show $ fst $ head es)
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
