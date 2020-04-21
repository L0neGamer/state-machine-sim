{-# LANGUAGE OverloadedStrings, RecordWildCards, InstanceSigs #-}
module NFA
 where

import Lib

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Maybe
import qualified Data.Graph as G
import qualified Data.Graph.DGraph as GDG
import qualified Data.Graph.Types as GT

data NFATransitionType a = Epsilon | Val a deriving (Show, Eq, Ord)

-- type NFATransition a = M.Map (NFATransitionType a) (S.Set State)

type NFAMapping a = GDG.DGraph State (S.Set (NFATransitionType a))

data NFAStateMachine a = 
    NFAStatMac {   
        startState :: State,
        acceptStates :: S.Set State,
        states :: S.Set State,
        language :: S.Set a,
        mapping :: NFAMapping a
    } deriving (Show)

data RunningNFA a =
    RunNFA {
        word :: [a],
        currentStates :: S.Set State,
        returnState :: ReturnState,
        remainingIter :: Runtime,
        nfa :: NFAStateMachine a
    } deriving (Show)

stepThroughEpsilons :: State -> NFAStateMachine a -> S.Set State
stepThroughEpsilons state NFAStatMac {..} = S.fromList map (GT.tripleDestVertex . GT.toTriple) $ findArcsMatching state Epsilon 

getAllEpsilonStates :: (Ord a) => S.Set State -> NFAStateMachine a -> S.Set State
getAllEpsilonStates s nfa
    | nextStates `S.isSubsetOf` s = s
    | otherwise = getAllEpsilonStates (S.union s nextStates) nfa
    where nextStates = S.unions (S.map (`stepThroughEpsilons` nfa) s)

instance StateMachine NFAStateMachine where
    constructStateMachine start accept states lang transitions = NFAStatMac start accept states lang mapping
        where transitions' = filter (\(a,_,c) -> a `S.member` states && c `S.member` states) $ fmap (\(a,b,c) -> (a,b `S.intersection` lang, c)) transitions
              arcs = toArcs (concatMap conv2 transitions')
              mapping = addArcs arcs GT.empty
    simpleConstructStateMachine start accept states lang transitions = constructStateMachine start (S.fromList accept) (S.fromList states) (S.fromList lang) (map (\(a, b, c) -> (a, S.singleton b, c)) transitions)

    stepMachine state transition nfa = getAllEpsilonStates (M.findWithDefault (S.singleton Dead) (Val transition) (M.findWithDefault M.empty state (mapping nfa))) nfa
    stepMachine state transition nfa = getAllEpsilonStates (findArcsWith (\a -> transition `S.member` (GT.tripleAttribute . GT.toTriple) a)) nfa
    

    run xs iters nfa = returnState $ runSM $ runNFA xs iters nfa

instance RunningStateMachine RunningNFA where
    step RunNFA {word=[],..} = RunNFA [] currentStates (Term ((not . null) $ S.intersection currentStates (acceptStates nfa))) remainingIter nfa
    step RunNFA {word=x:xs,..}
        | remainingIter < I 1 = RunNFA (x:xs) currentStates Timeout remainingIter nfa
        | currentStates == S.singleton Dead = RunNFA (x:xs) currentStates (Term False) remainingIter nfa
        | otherwise = runningNFA
        where nextStates = S.unions $ S.map (\cs -> stepMachine cs x nfa) currentStates
              runningNFA = RunNFA xs nextStates Running (decRuntime remainingIter) nfa
    
    getReturnState = returnState

runNFA :: (Ord a) => [a] -> Runtime -> NFAStateMachine a -> RunningNFA a
runNFA xs iters nfa = RunNFA xs startStates Running iters nfa
    where startStates = getAllEpsilonStates (S.singleton $ startState nfa) nfa

addNFATransitions :: (Ord a) => [(State, S.Set (NFATransitionType a), S.Set State)] -> NFAStateMachine a -> NFAStateMachine a
addNFATransitions transitions NFAStatMac{..} = NFAStatMac startState acceptStates states language mapping'
    where language' = S.insert Epsilon $ S.map Val language
          transitions' = fmap (\(a,b,c) -> (a,b `S.intersection` language', c)) transitions
          arcs = toArcs $ concatMap conv3 (concatMap conv2 transitions')
          mapping' = addArcs arcs mapping
        --   mapping' = M.unionWith (M.unionWith S.union) mapping (M.delete Dead $ toNestedMap transitions' S.union)

simpleAddNFATransitions :: (Ord a) => [(State, NFATransitionType a, State)] -> NFAStateMachine a -> NFAStateMachine a
simpleAddNFATransitions xs = addNFATransitions (map (\(a, b, c) -> (a, S.singleton b, S.singleton c)) xs)

-- -- accepts 101*0(0*1*)*
exampleNFA :: NFAStateMachine Integer
exampleNFA = constructStateMachine (IdI 0) (S.fromList [IdI 3]) (S.fromList [IdI 0, IdI 1, IdI 2, IdI 3]) lang ([(IdI 3, lang, IdI 3),(IdI 3, lang, IdI 1)] ++ fromTupleList [(IdI 0, 1, IdI 1), (IdI 1, 0, IdI 2), (IdI 2, 1, IdI 2), (IdI 2, 0, IdI 3)])
    where lang = S.fromList [0,1]

exampleNFA' :: NFAStateMachine Integer
exampleNFA' = addNFATransitions [(IdI 0, S.singleton Epsilon, S.singleton (IdI 3))] exampleNFA

emptyNFA :: NFAStateMachine Integer
emptyNFA = simpleConstructStateMachine (IdI 0) [] [] [] []
