module NFA (NFATransition, NFA, RunNFA, runNFA, NFAData (..)) where

import Data.Map as M (lookup)
import Data.Set as S
  ( Set,
    empty,
    intersection,
    isSubsetOf,
    toList,
    union,
    unions,
  )
import Data.Vector ((!?))
import Lib (Error, dropNothings, maybeToEither)
import RunStateMachine
  ( Clock,
    ReturnValue (Running, Term),
    RunningSM (..),
    constructRunningSM,
    runSM,
    updateCurrentState,
  )
import StateMachine (StateID, StateMachine (..), Transition, runStep)

data NFAData a = Epsilon | Val a deriving (Show, Eq, Ord)

type NFA a = StateMachine (NFAData a) Set ()

type NFATransition a = Transition (NFAData a) ()

type RunNFA a = RunningSM [] (NFAData a) Set ()

runNFA :: (Ord a) => [a] -> Clock -> NFA a -> Error (Either (String, RunNFA a) (RunNFA a))
runNFA tape' clk nfa = do
  rnfa <- getRunNFA tape' clk nfa
  return $ runSM rnfa

getRunNFA :: (Ord a) => [a] -> Clock -> NFA a -> Error (RunNFA a)
getRunNFA tape' clk nfa = do
  rnfa <- constructRunningSM (Val <$> tape') clk nfa (\_ x -> tail x) stepFunc haltingFunc
  newStartStates <- expandEpsilon (currentState rnfa) (stateMachine rnfa)
  return $ updateCurrentState newStartStates rnfa
  where
    stepFunc ss l RunSM {..} = do
      statesList <- mapM (\s -> runStep stateMachine s l) (S.toList ss)
      expandedStates <-
        expandEpsilon
          (foldr (S.union . fst) S.empty statesList)
          stateMachine
      return (expandedStates, ())
    haltingFunc ss _ as StateMachine {..}
      | null as && allVals = Term $ not . null $ S.intersection ss acceptStateIDs
      | allVals = Running
      | otherwise = Term False
      where
        allVals = foldr (\a b -> (a >= 0) || b) False ss

expandEpsilon :: (Ord a) => Set StateID -> NFA a -> Error (Set StateID)
expandEpsilon ss nfa@StateMachine {..} = do
  ms <-
    mapM
      (\s -> maybeToEither "Could not find state (expand epsilon)" (transitions !? s))
      (filter (>= 0) $ S.toList ss)
  let ss' = S.unions $ dropNothings (fmap (fmap fst . M.lookup Epsilon) ms)
  if ss' `S.isSubsetOf` ss then return ss else expandEpsilon (S.union ss ss') nfa

-----
-- stepThroughEpsilons :: (Ord a) => NFAStateMachine a -> State -> States
-- stepThroughEpsilons NFAStatMac {..} state = M.findWithDefault S.empty Epsilon (M.findWithDefault M.empty state transitions)

-- expandEpsilon :: (Ord a) => NFAStateMachine a -> States -> States
-- expandEpsilon nfa s
--   | next `S.isSubsetOf` s = s
--   | otherwise = expandEpsilon nfa (S.union s next)
--   where
--     next = S.unions $ S.map (stepThroughEpsilons nfa) s
