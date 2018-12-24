{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
module Path
  (shortestPath)
where

import Protolude

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set
import Data.Set (Set)

import Data.Foldable (foldl')

import qualified Data.PQueue.Prio.Min as Queue

-- | find the shortest path in a graph
dijkstra ::
  forall v w. (Ord v, Ord w, Num w)
  => (v -> [(w, v)]) -- ^ transition function frow vertex *v* to new vertices associated with weight *w*.
  -> (w -> w -> w) -- ^ weight combination function, usually (+) for distances
  -> v -- ^ starting vertex
  -> v -- ^ end vertex
  -> Map v (w, v) -- ^ associate a vertex *v* with its weight from the starting vertex and its previous vertex
dijkstra getNext combineWeight start end = go (Queue.singleton 0 start) Map.empty Set.empty
  where
    go :: Queue.MinPQueue w v -> Map v (w, v) -> Set v -> Map v (w, v)
    go queue prevs done =
      case Queue.minViewWithKey queue of
        Nothing -> prevs
        Just ((w, currentPoint), queue') ->
          if currentPoint `Set.member` done
            then if currentPoint == end
                 then prevs
                 else go queue' prevs done
            else let
              nexts = getNext currentPoint
              nextPriority = map (\(w', v) -> (w' `combineWeight` w, v)) nexts

              -- update queue
              queue'' = foldl' (\acc (k, a) -> Queue.insert k a acc) queue' nextPriority
              -- update prevs
              upPrevs = Map.fromList (map (\(w, v) -> (v, (w, currentPoint))) nextPriority)

              fUnion p0@(w, _) p1@(w', _)
                | w <= w' = p0
                | otherwise = p1
              in go queue'' (Map.unionWith fUnion prevs upPrevs) (Set.insert currentPoint done)

-- | find the shortest path in a graph
shortestPath ::
  forall v w. (Show w, Ord v, Ord w, Num w)
  => (v -> [(w, v)]) -- ^ transition function frow vertex *v* to new vertices associated with weight *w*.
  -> (w -> w -> w) -- ^ weight combination function, usually (+) for distances
  -> v -- ^ starting vertex
  -> v -- ^ ending vertex
  -> Maybe (w, [v]) -- ^ the list of vertices of the path associated with the weight
shortestPath getNext combineWeight start end = let
  d = dijkstra getNext combineWeight start end
  in buildPath start end d

buildPath ::
  (Show w, Ord v, Ord w, Num w)
  => v -- ^ starting vertex
  -> v -- ^ ending vertex
  -> Map v (w, v) -- ^ result of *dijkstra*
  -> Maybe (w, [v]) -- ^ resulting path with its weight
buildPath start end d
  | start == end = Just (0, [])
  | otherwise = case Map.lookup end d of
  Nothing -> Nothing
  Just (w, _prev) -> Just (w, go end [])
    where
      go current acc
        | current == start = acc
        | otherwise = let Just (_, prev) = Map.lookup current d
                      in go prev (current:acc)
