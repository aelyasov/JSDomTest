module Analysis.CFG.Data where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Graph.Inductive (LEdge(..))

type SLab = Int
type NLab = String
type ELab = String
type GPath = [SLab]
type EnumLEdge = [(Int, LEdge ELab)]

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


map2IntMap :: Map String Int -> IntMap Int
map2IntMap mp = IntMap.fromList $ map (\(s, i) -> (read s :: Int, i)) $ Map.toList mp 
