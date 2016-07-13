module Analysis.CFG.Data where

import Data.IntMap (IntMap)

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
              deriving Eq


instance Show LoopTree where
  show (Node iter head body) = "Node " ++ (show iter) ++ " " ++ (show head) ++ " " ++ (show body)
  show (Leaf node)           = "Leaf " ++ (show node)

