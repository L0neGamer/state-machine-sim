{-# LANGUAGE MultiParamTypeClasses, TupleSections, ViewPatterns #-}
module Lib
 where

import qualified Data.Map as M
import qualified Data.Set as S

data State = IdI Integer | IdS String | Dead deriving (Eq, Ord, Show)

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

conv :: (Ord b) => (a, S.Set b, c) -> [(a, b, c)]
conv (a, s, c) = map (a, , c) (S.toList s)

toNestedMap :: (Ord a, Ord b) => [(a,S.Set b,c)] -> (c -> c -> c) -> M.Map a (M.Map b c)
toNestedMap xs = toNestedMap' (concatMap conv xs)

fromTupleList :: (Ord b) => [(a,b,c)] -> [(a,S.Set b,c)]
fromTupleList = map (\(a,b,c) -> (a, S.singleton b, c))

fromSingleton :: S.Set a -> a
fromSingleton (S.toList -> [x]) = x
fromSingleton _ = error "tried to get single item from non-singleton set"

someFunc :: IO ()
someFunc = putStrLn "someFunc"

ones = 1:ones
