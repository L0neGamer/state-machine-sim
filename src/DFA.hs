{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module DFA
 where

import Lib

import qualified Data.Map as M
import qualified Data.Set as S

type DFATransition a = M.Map a State

type DFATransitions a = M.Map State (DFATransition a)

data DFAStateMachine a = 
    DFAStatMac {   
        states :: States,
        language :: Language a,
        transitions :: DFATransitions a,
        startState :: State,
        acceptStates :: AcceptStates
    } deriving (Show, Eq)

data RunningDFA a =
    RunDFA {
        word :: [a],
        currentState :: State,
        returnValue :: ReturnValue,
        remainingIter :: Clock,
        dfa :: DFAStateMachine a
    } deriving (Show)

instance StateMachine DFAStateMachine where
    constructStateMachine = constructStateMachine' (fromTuplesToMap const) DFAStatMac

    addTransition (start, dest, val) DFAStatMac{..}
        | not ((S.fromList [start, dest]) `S.isSubsetOf` states) = error "transition states not subset of states"
        | val `S.notMember` language = error "transition language not subset of language"
        | otherwise = DFAStatMac states language (M.unionWith M.union (M.singleton start (M.singleton val dest)) transitions) startState acceptStates

    removeTransition (start, dest, val) dfa@DFAStatMac{..}
        | M.findWithDefault Dead val submap == dest = DFAStatMac states language (M.insert start (M.delete val (M.findWithDefault M.empty start transitions)) transitions) startState acceptStates
        | otherwise = dfa
        where submap = M.findWithDefault M.empty start transitions
              
    stepMachine state transition DFAStatMac {..} = S.singleton (M.findWithDefault Dead transition (M.findWithDefault M.empty state transitions))

    run xs iters dfa = returnValue $ runSM $ runDFA xs iters dfa

    smAcceptStates = acceptStates

    nextStates dfa@DFAStatMac{..} s = foldr S.union S.empty $ S.map (\l -> stepMachine s l dfa) language

    reachableStates dfa curr = reachableStates' dfa (S.singleton curr) S.empty

instance RunningStateMachine RunningDFA where
    step RunDFA{word=[],..} = RunDFA [] currentState (Term (currentState `S.member` acceptStates dfa)) remainingIter dfa
    step RunDFA{word=x:xs,..}
        | remainingIter < I 1 = RunDFA (x:xs) currentState Timeout remainingIter dfa
        | otherwise = runningDFA
        where nextState = fromSingleton $ stepMachine currentState x dfa
              returnValue' | nextState == Dead = Term False
                           | otherwise = Running
              runningDFA = RunDFA xs nextState returnValue' (tickClock remainingIter) dfa

    getReturnValue = returnValue

runDFA :: (Ord a) => [a] -> Clock -> DFAStateMachine a -> RunningDFA a
runDFA xs iters dfa = RunDFA xs (startState dfa) Running iters dfa

q0, q1, q2, q3, q4, q5 :: State
q0:q1:q2:q3:q4:q5:_ = allIntStates

-- accepts 101*0
exampleDFA :: DFAStateMachine Integer
exampleDFA = inferStateMachine [(q0, q1, 1), (q1, q2, 0), (q2, q2, 1), (q2, q3, 0)] q0 (S.singleton q3)

emptyDFA :: DFAStateMachine Integer
emptyDFA = constructStateMachine (S.fromList [q0]) S.empty [] q0 S.empty
