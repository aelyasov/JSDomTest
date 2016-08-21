{-# LANGUAGE OverloadedStrings #-}

module Util.Debug where

import Debug.Trace
import Data.Configurator
import Control.Monad

traceCond :: Bool -> String -> a -> a
traceCond cond mes = if cond then trace mes else id

debug = traceCond False

setCondBreakPoint :: IO ()
setCondBreakPoint = do
  config <- load [Required "jsdomtest.cfg"]
  isBreakEnabled <- require config "breakpoint.enabled"
  when isBreakEnabled $ getLine >> return ()
