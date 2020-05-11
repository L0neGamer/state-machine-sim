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
        returnState :: ReturnState,
        remainingIter :: Runtime,
        dfa :: DFAStateMachine a
    } deriving (Show)

dfaTransitions :: (Ord a, Ord c) => [(a, b, c)] -> M.Map a (M.Map c b)
dfaTransitions = fromTuplesToMap const

instance StateMachine DFAStateMachine where
    constructStateMachine' modifier states language transitions startState acceptStates
        | startState `S.notMember` states = error "start state not in set of states"
        | modifier == Ignore = DFAStatMac states language (dfaTransitions transitions') startState acceptStates'
        | modifier == Infer  = DFAStatMac states' language' (dfaTransitions transitions) startState acceptStates
        where states' = S.insert startState $ S.union (S.union states acceptStates) (S.union (S.fromList (map tripleFirst transitions)) (S.fromList (map tripleSecond transitions)))
              language' = S.union language (S.fromList (map tripleThird transitions))
              transitions' = filter (\(a, b, c) -> S.fromList [a, b] `S.isSubsetOf` states && c `S.member` language && any (/=Dead) [a, b]) transitions
              acceptStates' = acceptStates `S.intersection` states
              
    stepMachine state transition DFAStatMac {..} = S.singleton (M.findWithDefault Dead transition (M.findWithDefault M.empty state transitions))

    run xs iters dfa = returnState $ runSM $ runDFA xs iters dfa

instance RunningStateMachine RunningDFA where
    step RunDFA {word=[],..} = RunDFA [] currentState (Term (currentState `S.member` acceptStates dfa)) remainingIter dfa
    step RunDFA {word=x:xs,..}
        | remainingIter < I 1 = RunDFA (x:xs) currentState Timeout remainingIter dfa
        | otherwise = runningDFA
        where nextState = fromSingleton $ stepMachine currentState x dfa
              returnState' | nextState == Dead = Term False
                           | otherwise = Running
              runningDFA = RunDFA xs nextState returnState' (decRuntime remainingIter) dfa

    getReturnState = returnState

runDFA :: (Ord a) => [a] -> Runtime -> DFAStateMachine a -> RunningDFA a
runDFA xs iters dfa = RunDFA xs (startState dfa) Running iters dfa

-- accepts 101*0
exampleDFA :: DFAStateMachine Integer
exampleDFA = inferStateMachine [(q0, q1, 1), (q1, q2, 0), (q2, q2, 1), (q2, q3, 0)] q0 (S.singleton q3)

emptyDFA :: DFAStateMachine Integer
emptyDFA = constructStateMachine (S.fromList [q0]) S.empty [] q0 S.empty
