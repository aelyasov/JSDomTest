{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Genetic.Instance where

import Genetic.DataJS
import Genetic.RandomJS
import Genetic.CrossoverJS
import Genetic.ScoreJS

import Html5C.Tags

import Control.Monad
import System.Random
-- import qualified Data.ByteString as BS

import GA


-- | sig = [JS_DOM,JS_STRING]
-- | pool = ([], ["node"], ([TAG_IFRAME], ["node"], [], []))
instance Entity [JSArg] Double Target (JSSig, JSCPool) IO where
    -- genRandom :: (JSSig, JSCPool) -> Int -> IO [JSArg]
    genRandom pool@(sig, env) seed = mapM (genRandomVal env) sig 

-- generate a random entity, i.e. a random string
  -- assumption: max. 100 chars, only 'printable' ASCII (first 128)
  -- genRandom pool seed = return $ take n $ map ((!!) pool) is
  --   where
  --       g = mkStdGen seed
  --       n = (fst $ random g) `mod` 101
  --       k = length pool
  --       is = map (flip mod k) $ randoms g

    -- crossover :: (JSSig, JSCPool) -> Float -> Int -> [JSArg] -> [JSArg] -> IO (Maybe [JSArg])
    crossover pool _ seed e1 e2 = liftM Just $ crossAllArgs gen cps
        where
      gen = mkStdGen seed
      cps = zipWith (\x y -> [x,y]) e1 e2

      crossAllArgs :: StdGen -> [[JSArg]] -> IO [JSArg]
      crossAllArgs g [] = return []
      crossAllArgs g (arg:args) = do 
        let (a, g') = random g
        d <- case arg of
               [DomJS d1, DomJS d2] -> liftM DomJS$ crossoverHTML g d1 d2
               otherwise            -> return $ arg!!(a `mod` 2) 
        args' <- crossAllArgs g' args
        return (d:args')
          
    mutation pool p seed e = undefined

    -- score :: d -> e -> m (Maybe s)
    score = fitnessScore 


runGenetic :: Target -> (JSSig, JSCPool) -> IO [JSArg]
runGenetic targ pool@(sig, (intP, stringP, (tagP, idP, nameP, classP))) = do
  let conf = GAConfig 100   -- population size
                      25    -- archive size (best entities to keep track of)
                      300   -- maximum number of generations
                      0.8   -- crossover rate (% of entities by crossover)
                      0.2   -- mutation rate (% of entities by mutation)
                      0.0   -- parameter for crossover (not used here)
                      0.2   -- parameter for mutation (% of replaced letters)
                      False -- whether or not to use checkpointing
                      False -- don't rescore archive in each generation
      g = mkStdGen 0 -- random generator            
  es <- evolveVerbose g conf pool targ
  return $ snd $ head es -- :: [JSArg]

