{-# LANGUAGE RankNTypes #-}

module SYB.Data.Generics.Schemes (everywhereM') where

import Data.Generics

-- | Monadic variation on everywhere'
-- | Apply a transformation everywhere in top-down manner
everywhereM' :: (Monad m) => GenericM m -> GenericM m
everywhereM' f x = do x' <- f x
                      gmapM (everywhereM' f) x'
