{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module DFA
 where

import Lib

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Graph as G
import qualified Data.Graph.DGraph as GDG
import qualified Data.Graph.Types as GT

type DFATransition a = M.Map a State

-- type DFAMapping a = M.Map State (DFATransition a)

type DFAMapping a = GDG.DGraph State (S.Set a)

data DFAStateMachine a = 
    DFAStatMac {   
        startState :: State,
        acceptStates :: S.Set State,
        states :: S.Set State,
        language :: S.Set a,
        mapping :: DFAMapping a
    } deriving (Show, Eq)

data RunningDFA a =
    RunDFA {
        word :: [a],
        currentState :: State,
        returnState :: ReturnState,
        remainingIter :: Runtime,
        dfa :: DFAStateMachine a
    } deriving (Show)

instance StateMachine DFAStateMachine where
    constructStateMachine start accept states lang transitions = DFAStatMac start accept states lang (GT.removeVertex Dead graph)
        where transitions' = filter (\(a,_,c) -> a `S.member` states && c `S.member` states) $ fmap (\(a,b,c) -> (a,b `S.intersection` lang, c)) transitions
              graph = addArcs (toArcs (concatMap conv2 transitions')) GT.empty
    simpleConstructStateMachine start accept states lang transitions = constructStateMachine start (S.fromList accept) (S.fromList states) (S.fromList lang) (map (\(a, b, c) -> (a, S.singleton b, c)) transitions)

    stepMachine state transition DFAStatMac {..} = S.singleton $ findArcWithDefault Dead state transition mapping --S.singleton (M.findWithDefault Dead transition (M.findWithDefault M.empty state mapping))

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
exampleDFA = simpleConstructStateMachine (IdI 0) [IdI 3] [IdI 0, IdI 1, IdI 2, IdI 3] [0,1] [(IdI 0, 1, IdI 1), (IdI 1, 0, IdI 2), (IdI 2, 1, IdI 2), (IdI 2, 0, IdI 3)]

emptyDFA :: DFAStateMachine Integer
emptyDFA = simpleConstructStateMachine (IdI 0) [] [IdI 0] [] []

emptyDGraph :: (Ord a) => GDG.DGraph State a
emptyDGraph = GT.empty
