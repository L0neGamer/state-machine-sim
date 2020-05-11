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

decRuntime :: Runtime -> Runtime
decRuntime (I i) = I (i-1)
decRuntime Infinite = Infinite

allIntStates = map IdI [0,1..]

-- constructStateMachine'' :: (Ord a, Ord b, Ord c, StateMachine sm) => (a -> b) -> (c -> c -> c) -> (States -> Language b -> TransitionsMap b c -> State -> AcceptStates -> sm a) -> ConsMod -> States -> Language a -> Transitions a -> State -> AcceptStates -> sm a
-- constructStateMachine'' langConv combiner constructor modifier states language transitions startState acceptStates
--     | startState `S.notMember` states = error "start state not in set of states"
--     | otherwise = constructor states (S.map langConv language) (fromTuplesToMap combiner $ transitions') startState acceptStates'

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

    smStates :: sm a -> States
    smLanguage :: sm a -> Language a
    smStartState :: sm a -> State
    smAcceptStates :: sm a -> AcceptStates

    addTransition :: (Ord a) => Transition a -> sm a -> sm a
    removeTransition :: (Ord a) => Transition a -> sm a -> sm a

    addTransitions :: (Ord a) => Transitions a -> sm a -> sm a
    addTransitions ts sm = foldr addTransition sm ts

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

