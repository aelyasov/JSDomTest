module Data.Graph.Analysis.Algorithms.Commons where

import Data.Graph.Inductive.Graph
import Data.List(unfoldr, foldl', foldl1', intersect, (\\), delete, tails, nub, nubBy)
import Data.Maybe(isJust)
import Control.Arrow(first)
import Data.Function(on)


-- | A grouping of 'Node's.
type NGroup = [Node]

-- | Find all possible paths from this given node, avoiding loops,
--   cycles, etc.
pathTree             :: (DynGraph g) => Decomp g a b -> [NGroup]
pathTree (Nothing,_) = []
pathTree (Just ct,g)
    | isEmpty g = []
    | null sucs = [[n]]
    | otherwise = (:) [n] . map (n:) . concatMap (subPathTree g') $ sucs
    where
      n = node' ct
      sucs = suc' ct
      -- Avoid infinite loops by not letting it continue any further
      ct' = makeLeaf ct
      g' = ct' & g
      subPathTree gr n' = pathTree $ match n' gr

-- | Remove all outgoing edges
makeLeaf           :: Context a b -> Context a b
makeLeaf (p,n,a,_) = (p', n, a, [])
    where
      -- Ensure there isn't an edge (n,n)
      p' = filter (\(_,n') -> n' /= n) p


-- | Find all cycles in the given graph, returning just the nodes.
cyclesIn' :: (DynGraph g) => g a b -> [NGroup]
cyclesIn' = concat . unfoldr findCycles . mkSimple


-- | Find all cycles containing a chosen node.
findCycles :: (DynGraph g) => g a b -> Maybe ([NGroup], g a b)
findCycles g
    | isEmpty g = Nothing
    | otherwise = Just . getCycles . matchAny $ g
    where
      getCycles (ctx,g') = (cyclesFor (ctx, g'), g')


-- | Find all cycles for the given node.
cyclesFor :: (DynGraph g) => GDecomp g a b -> [NGroup]
cyclesFor = map init .
            filter isCycle .
            pathTree .
            first Just
    where
      isCycle p = not (single p) && (head p == last p)      


-- | Makes the graph a simple one, by removing all duplicate edges and loops.
--   The edges removed if duplicates exist are arbitrary.
mkSimple :: (DynGraph gr) => gr a b -> gr a b
mkSimple = gmap simplify
    where
      rmLoops n = filter ((/=) n . snd)
      rmDups = nubBy ((==) `on` snd)
      simpleEdges n = rmDups . rmLoops n
      simplify (p,n,l,s) = (p',n,l,s')
          where
            p' = simpleEdges n p
            s' = simpleEdges n s


-- | Return true if and only if the list contains a single element.
single     :: [a] -> Bool
single [_] = True
single  _  = False
