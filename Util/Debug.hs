module Util.Debug where

import Debug.Trace

traceCond :: Bool -> String -> a -> a
traceCond cond mes = if cond then trace mes else id

debug = traceCond False
