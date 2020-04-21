{-# LANGUAGE MultiParamTypeClasses, TupleSections, ViewPatterns #-}
module Lib
 where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Maybe as Maybe
import qualified Data.Hashable as H
import qualified Data.Graph as G
import qualified Data.Graph.DGraph as GDG
import qualified Data.Graph.Types as GT

data State = IdI Integer | IdS String | Dead deriving (Eq, Ord, Show)

instance H.Hashable State where
    hashWithSalt i state = H.hashWithSalt i (show state)

data ReturnState = Running | Timeout | Term Bool deriving (Eq, Show)

data Runtime = I Integer | Infinite deriving (Show, Ord, Eq)

decRuntime :: Runtime -> Runtime
decRuntime (I i) = I (i-1)
decRuntime Infinite = Infinite

type States = S.Set State
type AcceptStates = States

class StateMachine sm where
    constructStateMachine :: (Ord a) => State -> AcceptStates -> States -> S.Set a -> [(State, S.Set a, State)] -> sm a
    simpleConstructStateMachine :: (Ord a) => State -> [State] -> [State] -> [a] -> [(State, a, State)] -> sm a
    stepMachine :: (Ord a) => State -> a -> sm a -> S.Set State
    stepMachine Dead _ _ = S.singleton Dead
    run :: (Ord a) => [a] -> Runtime -> sm a -> ReturnState

class RunningStateMachine rsm where
    getReturnState :: (Ord a) => rsm a -> ReturnState
    step :: (Ord a) => rsm a -> rsm a
    runSM :: (Ord a) => rsm a -> rsm a
    runSM running
        | getReturnState running' == Running = runSM running'
        | otherwise = running'
        where running' = step running

toNestedMap' :: (Ord a, Ord b) => [(a,b,c)] -> (c -> c -> c) -> M.Map a (M.Map b c)
toNestedMap' [] _ = M.empty
toNestedMap' ((a, b, c):xs) f = M.insertWith (\_ m -> M.insertWith f b c (res M.! a)) a (M.singleton b c) res
    -- | a `M.member` res = M.insert a  res
    -- | otherwise = M.insert a  res
    where res = toNestedMap' xs f

conv2 :: (Ord b) => (a, S.Set b, c) -> [(a, b, c)]
conv2 (a, s, c) = map (a, , c) (S.toList s)
conv3 :: (Ord b) => (a, b, S.Set c) -> [(a, b, c)]
conv3 (a, b, s) = map (a, b, ) (S.toList s)

toNestedMap :: (Ord a, Ord b) => [(a,S.Set b,c)] -> (c -> c -> c) -> M.Map a (M.Map b c)
toNestedMap xs = toNestedMap' (concatMap conv2 xs)

fromTupleList :: (Ord b) => [(a,b,c)] -> [(a,S.Set b,c)]
fromTupleList = map (\(a,b,c) -> (a, S.singleton b, c))

fromSingleton :: S.Set a -> a
fromSingleton (S.toList -> [x]) = x
fromSingleton _ = error "tried to get single item from non-singleton set"

someFunc :: IO ()
someFunc = putStrLn "someFunc"

ones = 1:ones

toArcs :: [(a, b, a)] -> [GT.Arc a b]
toArcs = map (\(a, b, c) -> GT.Arc a c b)

findArcsWith :: (H.Hashable a, Eq a, Eq b) => (GT.Arc a b -> Bool) -> a -> GDG.DGraph a b -> [GT.Arc a b]
findArcsWith f node dgraph = filter f $ GDG.outboundingArcs dgraph node

findArcsWithTransition :: (H.Hashable a, Eq a, Ord b) => a -> b -> GDG.DGraph a b -> [GT.Arc a b]
findArcsWithTransition node transition = findArcsWith (\a -> transition == (GT.tripleAttribute . GT.toTriple)) node

findArcsMatching :: (H.Hashable a, Eq a, Eq b) => a -> b -> GDG.DGraph a b -> [GT.Arc a b]
-- findArcsMatching node attr = findArcsWith (\x -> node == (GT.tripleOriginVertex . GT.toTriple) x && attr == (GT.tripleAttribute . GT.toTriple) x)
findArcsMatching node attr dgraph = filter (\x -> attr == (GT.tripleAttribute . GT.toTriple) x) $ GDG.outboundingArcs dgraph node

findArcWithDefault :: (H.Hashable a, Eq a, Eq b) => a -> a -> b -> GDG.DGraph a b -> a
findArcWithDefault defNode node attr graph
    | Maybe.isJust arc = (GT.tripleDestVertex . Maybe.fromJust) arc
    | otherwise = defNode
    where arc = L.find (\(_,_,x) -> x == attr) $ map GT.toTriple $ GDG.outboundingArcs graph node

addArc :: (H.Hashable a, Eq a, Ord b) => GT.Arc a b -> GDG.DGraph a (S.Set b) -> GDG.DGraph a (S.Set b)
addArc (GT.Arc a1 a2 b) dgraph
    | Maybe.isJust attr = GDG.insertArc (GT.Arc a1 a2 (S.insert b (Maybe.fromJust attr))) dgraph
    | otherwise = GDG.insertArc (GT.Arc a1 a2 (S.singleton b)) dgraph
    where attr = GT.edgeTriple dgraph a1 a2 >>= Just . GT.tripleAttribute

addArcs :: (H.Hashable a, Eq a, Ord b) => [GT.Arc a b] -> GDG.DGraph a (S.Set b) -> GDG.DGraph a (S.Set b)
addArcs xs dgraph = foldl (flip addArc) dgraph xs
