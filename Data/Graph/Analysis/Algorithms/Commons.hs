module Data.Graph.Analysis.Algorithms.Commons where

import Data.Graph.Inductive.Graph
import Data.List(unfoldr, foldl', foldl1', intersect, (\\), delete, tails, nub, nubBy)
import Data.Maybe(isJust)
import Control.Arrow(first)
import Data.Function(on)
import Control.Monad(ap)
import Data.Maybe(fromJust)

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


-- | Find all possible paths from this given node, avoiding loops,
--   cycles, etc.
pathTree'             :: (DynGraph g) => Decomp g a b -> [NGroup]
pathTree' (Nothing,_) = []
pathTree' (Just ct,g)
    | isEmpty g = []
    | null sucs = [[n]]
    | otherwise = map (n:) . concatMap (subPathTree g') $ sucs
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


-- | Find all chains in the given graph.
chainsIn   :: (DynGraph g, Eq b) => g a b -> [LNGroup a]
chainsIn g = map (addLabels g)
             $ chainsIn' g

-- | Find all chains in the given graph.
chainsIn'   :: (DynGraph g, Eq b) => g a b -> [NGroup]
chainsIn' g = filter (not . single) -- Remove trivial chains
              . map (getChain g')
              $ filterNodes' isChainStart g'
    where
      -- Try to make this work on two-element cycles, undirected
      -- graphs, etc.  Also remove multiple edges, etc.
      g' = oneWay $ mkSimple g


-- | Find the chain starting with the given 'Node'.
getChain     :: (Graph g) => g a b -> Node -> NGroup
getChain g n = n : unfoldr (chainLink g) (chainNext g n)

-- | Find the next link in the chain.
chainLink :: (Graph g) => g a b -> Maybe Node
          -> Maybe (Node, Maybe Node)
chainLink _ Nothing = Nothing
chainLink g (Just n)
    | isEmpty g         = Nothing
    | not $ hasPrev g n = Nothing
    | otherwise         = Just (n, chainNext g n)

-- | Determines if the given node is the start of a chain.
isChainStart     :: (Graph g) => g a b -> Node -> Bool
isChainStart g n = hasNext g n
                   && case (pre g n \\ [n]) of
                        [n'] -> not $ isChainStart g n'
                        _    -> True

-- | Determine if the given node matches the chain criteria in the given
--   direction, and if so what the next node in that direction is.
chainFind         :: (Graph g) => (g a b -> Node -> NGroup)
                  -> g a b -> Node -> Maybe Node
chainFind f g n = case (nub (f g n) \\ [n]) of
                    [n'] -> Just n'
                    _    -> Nothing

-- | Find the next node in the chain.
chainNext :: (Graph g) => g a b -> Node -> Maybe Node
chainNext = chainFind suc

-- | Determines if this node matches the successor criteria for chains.
hasNext   :: (Graph g) => g a b -> Node -> Bool
hasNext g = isJust . chainNext g

-- | Determines if this node matches the predecessor criteria for chains.
hasPrev   :: (Graph g) => g a b -> Node -> Bool
hasPrev g = isJust . chainFind pre g

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


-- | This is a pseudo-inverse of 'undir': any edges that are both successor
--   and predecessor become successor edges only.
oneWay :: (DynGraph g, Eq b) => g a b -> g a b
oneWay = gmap rmPre
    where
      rmPre (p,n,l,s) = (p \\ s,n,l,s)


-- | Obtain the labels for a list of 'Node's.
--   It is assumed that each 'Node' is indeed present in the given graph.
addLabels    :: (Graph g) => g a b -> [Node] -> [LNode a]
addLabels gr = map (ap (,) (fromJust . lab gr))

-- | Find all the labelled nodes in the graph that match the given predicate.
filterNodes     :: (Graph g) => (g a b -> LNode a -> Bool) -> g a b -> [LNode a]
filterNodes p g = filter (p g) (labNodes g)

-- | Find all the nodes in the graph that match the given predicate.
filterNodes'     :: (Graph g) => (g a b -> Node -> Bool) -> g a b -> [Node]
filterNodes' p g = filter (p g) (nodes g)

type LNGroup a = [LNode a]
