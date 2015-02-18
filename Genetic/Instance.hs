{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Genetic.Instance where

import Genetic.DataJS
import Genetic.RandomJS
import Genetic.CrossoverJS

import Control.Monad
import System.Random
-- import qualified Data.ByteString as BS

import GA (Entity(..))



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
    score _ e = undefined


