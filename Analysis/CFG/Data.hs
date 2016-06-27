module Analysis.CFG.Data where

import Data.IntMap

type SLab = Int
type NLab = String
type ELab = String
type GPath = [SLab]

type LoopSizeMap = IntMap Int
type LoopIterationMap = IntMap Int
type LoopMaxSizeMap = IntMap Int

data LoopTree = Node { getLoopIter :: Int,
                       getLoopHead :: Int,
                       getLoopBody :: [LoopTree] }
              | Leaf { getLoopNode :: Int }
              deriving (Show, Eq)
