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
        returnValue :: ReturnValue,
        remainingIter :: Clock,
        nfa :: NFAStateMachine a
    } deriving (Show)

stepThroughEpsilons :: (Ord a) => NFAStateMachine a -> State -> States
stepThroughEpsilons NFAStatMac{..} state  = M.findWithDefault S.empty Epsilon (M.findWithDefault M.empty state transitions)

expandEpsilon :: (Ord a) => NFAStateMachine a -> States -> States
expandEpsilon nfa s
    | next `S.isSubsetOf` s = s
    | otherwise = expandEpsilon nfa (S.union s next)
    where next = S.unions $ S.map (stepThroughEpsilons nfa) s

toNFATransition :: (Ord a) => Transition a -> (State, States, NFATransitionType a)
toNFATransition (a, b, c) = (a, S.singleton b, Val c)

removeEpsilons :: (Ord a) => S.Set (NFATransitionType a) -> S.Set a
removeEpsilons s = S.map (\(Val a) -> a) $ S.filter (/=Epsilon) s

instance StateMachine NFAStateMachine where
    constructStateMachine states language = constructStateMachine'' (S.map Val) (\t -> fromTuplesToMap S.union $ map toNFATransition t) NFAStatMac states language
    
    addTransition t = addNFATransition (toNFATransition t)
    removeTransition t = removeNFATransition (toNFATransition t)
              
    stepMachine state transition nfa = expandEpsilon nfa (S.unions $ S.map getStates (expandEpsilon nfa (S.singleton state)))
        where getStates s = (M.findWithDefault (S.singleton Dead) (Val transition) (M.findWithDefault M.empty s (transitions nfa)))

    run xs iters nfa = returnValue $ runSM $ runNFA xs iters nfa

    smAcceptStates = acceptStates

    nextStates nfa@NFAStatMac{..} s = foldr S.union S.empty $ S.map (\l -> stepMachine s l nfa) (removeEpsilons language)

    reachableStates sm curr = reachableStates' sm (expandEpsilon sm (S.singleton curr)) S.empty

instance RunningStateMachine RunningNFA where
    step RunNFA{word=[],..} = RunNFA [] currentStates (Term (not $ S.disjoint currentStates (acceptStates nfa))) remainingIter nfa
    step RunNFA{word=x:xs,..}
        | remainingIter < I 1 0 = RunNFA (x:xs) currentStates Timeout remainingIter nfa
        | currentStates == S.singleton Dead = RunNFA (x:xs) currentStates (Term False) remainingIter nfa
        | otherwise = runningNFA
        where next = S.unions $ S.map (\cs -> stepMachine cs x nfa) currentStates
              runningNFA = RunNFA xs next Running (tickClock remainingIter) nfa
    
    getReturnValue = returnValue

runNFA :: (Ord a) => [a] -> Clock -> NFAStateMachine a -> RunningNFA a
runNFA xs iters nfa = RunNFA xs startStates Running iters nfa
    where startStates = expandEpsilon nfa (S.singleton $ startState nfa)


addNFATransition :: (Ord a) => (State, States, (NFATransitionType a)) -> NFAStateMachine a -> NFAStateMachine a
addNFATransition (start, dest, val) NFAStatMac{..}
    | not ((S.insert start dest) `S.isSubsetOf` states) = error "transition states not subset of states"
    | val `S.notMember` language = error "transition language not subset of language"
    | otherwise = NFAStatMac states language (M.unionWith (M.unionWith S.union) (M.singleton start (M.singleton val dest)) transitions) startState acceptStates

removeNFATransition :: (Ord a) => (State, States, (NFATransitionType a)) -> NFAStateMachine a -> NFAStateMachine a
removeNFATransition (start, dest, val) NFAStatMac{..} = NFAStatMac states language (M.insert start (M.insert val (S.difference dest $ M.findWithDefault S.empty val submap) submap) transitions) startState acceptStates
    where submap = M.findWithDefault M.empty start transitions
