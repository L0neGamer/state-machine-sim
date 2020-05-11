{-# LANGUAGE OverloadedStrings, RecordWildCards, InstanceSigs #-}
module NFA
 where

import Lib

import qualified Data.Map as M
import qualified Data.Set as S

data NFATransitionType a = Epsilon | Val a deriving (Show, Eq, Ord)

type NFATransition a = M.Map (NFATransitionType a) States

type NFATransitions a = M.Map State (NFATransition a)

data NFAStateMachine a = 
    NFAStatMac {   
        states :: States,
        language :: Language (NFATransitionType a),
        transitions :: NFATransitions a,
        startState :: State,
        acceptStates :: AcceptStates
    } deriving (Show)

data RunningNFA a =
    RunNFA {
        word :: [a],
        currentStates :: States,
        returnState :: ReturnState,
        remainingIter :: Runtime,
        nfa :: NFAStateMachine a
    } deriving (Show)

stepThroughEpsilons :: (Ord a) => State -> NFAStateMachine a -> S.Set State
stepThroughEpsilons state NFAStatMac {..} = M.findWithDefault S.empty Epsilon (M.findWithDefault M.empty state transitions)

getAllEpsilonStates :: (Ord a) => S.Set State -> NFAStateMachine a -> S.Set State
getAllEpsilonStates s nfa
    | nextStates `S.isSubsetOf` s = s
    | otherwise = getAllEpsilonStates (S.union s nextStates) nfa
    where nextStates = S.unions (S.map (`stepThroughEpsilons` nfa) s)

toNFATransition :: (Ord a) => Transition a -> (State, S.Set State, NFATransitionType a)
toNFATransition (a, b, c) = (a, S.singleton b, Val c)

instance StateMachine NFAStateMachine where
    constructStateMachine states language = constructStateMachine'' (S.map Val) (\t -> fromTuplesToMap S.union $ map toNFATransition t) NFAStatMac states language

    -- constructStateMachine states language transitions startState acceptStates
    --     | startState `S.notMember` states = error "start state not in set of states"
    --     | otherwise = NFAStatMac states (S.map Val language) transitions'' startState acceptStates'
    --     where transitions' = map (\(a, b, c) -> (a, Val b, S.singleton c)) $ filter (\(a, b, c) -> S.fromList [a, b] `S.isSubsetOf` states && c `S.member` language && any (/=Dead) [a, b]) transitions
    --           transitions'' = fromTuplesToMap S.union transitions'
    --           acceptStates' = acceptStates `S.intersection` states
    
    addTransition t = addNFATransition (toNFATransition t)
    removeTransition t = removeNFATransition (toNFATransition t)
              
    stepMachine state transition nfa = getAllEpsilonStates (M.findWithDefault (S.singleton Dead) (Val transition) (M.findWithDefault M.empty state (transitions nfa))) nfa

    run xs iters nfa = returnState $ runSM $ runNFA xs iters nfa

instance RunningStateMachine RunningNFA where
    step RunNFA{word=[],..} = RunNFA [] currentStates (Term ((not . null) $ S.intersection currentStates (acceptStates nfa))) remainingIter nfa
    step RunNFA{word=x:xs,..}
        | remainingIter < I 1 = RunNFA (x:xs) currentStates Timeout remainingIter nfa
        | currentStates == S.singleton Dead = RunNFA (x:xs) currentStates (Term False) remainingIter nfa
        | otherwise = runningNFA
        where nextStates = S.unions $ S.map (\cs -> stepMachine cs x nfa) currentStates
              runningNFA = RunNFA xs nextStates Running (decRuntime remainingIter) nfa
    
    getReturnState = returnState

runNFA :: (Ord a) => [a] -> Runtime -> NFAStateMachine a -> RunningNFA a
runNFA xs iters nfa = RunNFA xs startStates Running iters nfa
    where startStates = getAllEpsilonStates (S.singleton $ startState nfa) nfa


addNFATransition :: (Ord a) => (State, S.Set State, (NFATransitionType a)) -> NFAStateMachine a -> NFAStateMachine a
addNFATransition (start, dest, val) NFAStatMac{..}
    | not ((S.insert start dest) `S.isSubsetOf` states) = error "transition states not subset of states"
    | val `S.notMember` language = error "transition language not subset of language"
    | otherwise = NFAStatMac states language (M.unionWith (M.unionWith S.union) (M.singleton start (M.singleton val dest)) transitions) startState acceptStates
    
removeNFATransition :: (Ord a) => (State, S.Set State, (NFATransitionType a)) -> NFAStateMachine a -> NFAStateMachine a
removeNFATransition (start, dest, val) NFAStatMac{..} = NFAStatMac states language (M.insert start (M.insert val (S.difference dest $ M.findWithDefault S.empty val submap) submap) transitions) startState acceptStates
    where submap = M.findWithDefault M.empty start transitions

-- addNFATransitions :: (Ord a) => Transitions (NFATransitionType a) -> NFAStateMachine a -> NFAStateMachine a
-- addNFATransitions transitions' NFAStatMac{..} = NFAStatMac startState acceptStates states language mapping'
--     where language' = S.union language $ S.fromList $ map tripleThird transitions'
--           transitions'' = transitions''
--           mapping' = M.unionWith (M.unionWith S.union) mapping (M.delete Dead $ toNestedMap transitions' S.union)

-- simpleAddNFATransitions :: (Ord a) => [(State, NFATransitionType a, State)] -> NFAStateMachine a -> NFAStateMachine a
-- simpleAddNFATransitions xs = addNFATransitions (map (\(a, b, c) -> (a, S.singleton b, S.singleton c)) xs)

-- -- -- accepts 101*0(0*1*)*
-- exampleNFA :: NFAStateMachine Integer
-- exampleNFA = constructStateMachine (S.fromList [q0,q1,q2,q3]) (S.fromList lang) (expandSecond [(IdI 3, lang, IdI 3),(IdI 3, lang, IdI 1)] ++ [(IdI 0, 1, IdI 1), (IdI 1, 0, IdI 2), (IdI 2, 1, IdI 2), (IdI 2, 0, IdI 3)]) (IdI 0) (S.fromList [IdI 3]) 
--     where lang = [0,1]

-- exampleNFA' :: NFAStateMachine Integer
-- exampleNFA' = addNFATransitions [(q0, q3, Epsilon)] exampleNFA

-- emptyNFA :: NFAStateMachine Integer
-- emptyNFA = constructStateMachine (S.fromList [q0]) S.empty [] q0 S.empty
