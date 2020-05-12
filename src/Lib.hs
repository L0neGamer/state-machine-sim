{-# LANGUAGE MultiParamTypeClasses, TupleSections, ViewPatterns #-}
module Lib
 where

import qualified Data.Map as M
import qualified Data.Set as S

-- import Debug.Trace (trace)

data State = IdI Integer | IdS String | Dead deriving (Eq, Ord, Show)

data ReturnValue = Running | Timeout | Term Bool deriving (Eq, Show)

data Clock = I Integer Integer | Infinite Integer deriving (Show, Ord, Eq)

type States = S.Set State
type AcceptStates = States
type Language a = S.Set a
type Transition a = (State, State, a)
type Transitions a = [Transition a]

tickClock :: Clock -> Clock
tickClock (I i j) = I (i-1) j
tickClock (Infinite i) = Infinite (i+1)

clock :: Integer -> Clock
clock i
    | i >= 0 = I i i
    | otherwise = Infinite 0

getTime :: Clock -> Integer
getTime (I i j) = j - i
getTime (Infinite i) = i

allIntStates :: [State]
allIntStates = map IdI [0,1..]

getStatesAndLang :: Ord a => Transitions a -> (States, Language a)
getStatesAndLang = foldr combine (S.empty, S.empty)
    where combine (a, a', b) (as, bs) = (S.insert a (S.insert a' as), S.insert b bs)

constructStateMachine'' :: (Ord a, Ord b) => (Language a -> Language b) -> (Transitions a -> transitionsMap) -> (States -> Language b -> transitionsMap -> State -> AcceptStates -> sm a) -> States -> Language a -> Transitions a -> State -> AcceptStates -> sm a
constructStateMachine'' langConv transitionsConv stateMachineCons states language transitions startState acceptStates
    | startState `S.notMember` states = error "start state not in set of states"
    | not (acceptStates `S.isSubsetOf` states) = error "accept states not subset of states"
    | not (transitionStates `S.isSubsetOf` states) = error "transition states not subset of states"
    | not ((langConv transitionLanguage) `S.isSubsetOf` language') = error "transition language not subset of language"
    | otherwise = stateMachineCons states language' (transitionsConv transitions) startState acceptStates
    where (transitionStates, transitionLanguage) = getStatesAndLang transitions
          language' = langConv language

constructStateMachine' :: (Ord a) => (Transitions a -> transitionsMap) -> (States -> Language a -> transitionsMap -> State -> AcceptStates -> sm a) -> States -> Language a -> Transitions a -> State -> AcceptStates -> sm a
constructStateMachine' = constructStateMachine'' id

class StateMachine sm where
    constructStateMachine :: (Ord a) => States -> Language a -> Transitions a -> State -> AcceptStates -> sm a

    inferStateMachine :: (Ord a) => Transitions a -> State -> AcceptStates -> sm a
    inferStateMachine transitions = constructStateMachine states language transitions
        where (states, language) = getStatesAndLang transitions

    addTransition :: (Ord a) => Transition a -> sm a -> sm a
    removeTransition :: (Ord a) => Transition a -> sm a -> sm a

    addTransitions :: (Ord a) => Transitions a -> sm a -> sm a
    addTransitions ts sm = foldr addTransition sm ts

    stepMachine :: (Ord a) => State -> a -> sm a -> States
    stepMachine Dead _ _ = S.singleton Dead

    run :: (Ord a) => [a] -> Clock -> sm a -> ReturnValue

    smAcceptStates :: (Ord a) => sm a -> AcceptStates

    nextStates :: (Ord a) => sm a -> State -> States
    reachableStates' :: (Ord a) => sm a -> States -> States -> States
    reachableStates' sm toCheck visited
        | next == (S.singleton Dead) || null next = S.delete Dead $ S.union toCheck visited
        | otherwise = reachableStates' sm next (S.union next visited)
        where next = S.difference (S.unions $ S.map (nextStates sm) toCheck) visited

    reachableStates :: (Ord a) => sm a -> State -> States

    isAcceptable :: (Ord a) => sm a -> State -> Bool
    isAcceptable sm s = or $ S.map (`S.member` (smAcceptStates sm)) (reachableStates sm s)

class RunningStateMachine rsm where
    getReturnValue :: (Ord a) => rsm a -> ReturnValue
    step :: (Ord a) => rsm a -> rsm a
    runSM :: (Ord a) => rsm a -> rsm a
    runSM running
        | getReturnValue running' == Running = runSM running'
        | otherwise = running'
        where running' = step running

fromTuplesToMap :: (Ord a, Ord c) => (b -> b -> b) -> [(a, b, c)] -> M.Map a (M.Map c b)
fromTuplesToMap _ []             = M.empty
fromTuplesToMap f ((a, b, c):xs) = M.unionWith (M.unionWith f) (M.singleton a (M.singleton c b)) mp
    where mp = fromTuplesToMap f xs          

fromSingleton :: S.Set a -> a
fromSingleton (S.toList -> [x]) = x
fromSingleton _ = error $ "tried to get single item from non-singleton set"
