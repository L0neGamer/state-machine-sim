{-# LANGUAGE MultiParamTypeClasses, TupleSections, ViewPatterns #-}
module Lib
 where

import qualified Data.Map as M
import qualified Data.Set as S

data State = IdI Integer | IdS String | Dead deriving (Eq, Ord, Show)

data ReturnState = Running | Timeout | Term Bool deriving (Eq, Show)

data Runtime = I Integer | Infinite deriving (Show, Ord, Eq)

type States = S.Set State
type AcceptStates = States
type Language a = S.Set a
type Transition a = (State, State, a)
type Transitions a = [Transition a]

data ConsMod = Infer | Ignore deriving (Show, Eq)

decRuntime :: Runtime -> Runtime
decRuntime (I i) = I (i-1)
decRuntime Infinite = Infinite

allIntStates = map IdI [0,1..]
q0:q1:q2:q3:q4:q5:q6:q7:q8:q9:xs = allIntStates

class StateMachine sm where
    constructStateMachine' :: (Ord a) => ConsMod -> States -> Language a -> Transitions a -> State -> AcceptStates -> sm a
    constructStateMachine :: (Ord a) => States -> Language a -> Transitions a -> State -> AcceptStates -> sm a
    constructStateMachine = constructStateMachine' Ignore
    inferStateMachine :: (Ord a) => Transitions a -> State -> AcceptStates -> sm a
    inferStateMachine = constructStateMachine' Infer S.empty S.empty

    addTransition' :: (Ord a) => ConsMod -> Transition a -> sm a -> sm a
    addTransition :: (Ord a) => Transition a -> sm a -> sm a

    stepMachine :: (Ord a) => State -> a -> sm a -> States
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

fromTuplesToMap :: (Ord a, Ord c) => (b -> b -> b) -> [(a, b, c)] -> M.Map a (M.Map c b)
fromTuplesToMap _ []             = M.empty
fromTuplesToMap f ((a, b, c):xs) = M.unionWith (M.unionWith f) (M.singleton a (M.singleton c b)) mp
    where mp = fromTuplesToMap f xs          

fromSingleton :: S.Set a -> a
fromSingleton (S.toList -> [x]) = x
fromSingleton _ = error "tried to get single item from non-singleton set"

someFunc :: IO ()
someFunc = putStrLn "someFunc"

ones = 1:ones

tripleFirst (a,_,_) = a
tripleSecond (_,a,_) = a
tripleThird (_,_,a) = a

tupleSectionFirst (a, b, c) = (, b, c)
tupleSectionSecond (a, b, c) = (a, , c)
tupleSectionThird (a, b, c) = (a, b, )

expandVar :: (t -> a -> b) -> (t -> [a]) -> t -> [b]
expandVar tupleSection chosenTriple x = fmap (tupleSection x) (chosenTriple x)

expandFirst :: [([a], b, c)] -> [(a, b, c)]
expandFirst = concatMap (expandVar tupleSectionFirst tripleFirst)
expandSecond :: [(a, [b], c)] -> [(a, b, c)]
expandSecond = concatMap (expandVar tupleSectionSecond tripleSecond)
expandThird :: [(a, b, [c])] -> [(a, b, c)]
expandThird = concatMap (expandVar tupleSectionThird tripleThird)
-- expandFirst (a, b, c) = fmap (, b, c) a
-- expandFirst (a, b, c) = fmap (, b, c) a

-- expandSource :: (Functor f) => [(f a, b, c)] -> [(a, b, c)]
-- expandSource xs = map expandFirst xs

