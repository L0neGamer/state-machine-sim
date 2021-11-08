-- |
-- Module      :  Data.StateMachines.NFA
-- License     :  BSD3
--
-- Maintainer  :  L0neGamer
-- Stability   :  experimental
--
-- Functions and types for constructing and running DFAs.
module Data.StateMachines.NFA
  ( NFAData (..),
    NFATransition,
    NFA,
    RunNFA,
    RunNFAResult,
    runNFA,
  )
where

import Data.Either.Extra (maybeToEither)
import Data.Map as M (lookup)
import Data.Maybe (mapMaybe)
import Data.Set as S
  ( Set,
    empty,
    intersection,
    isSubsetOf,
    toList,
    union,
    unions,
  )
import Data.StateMachines.Internal (Error)
import Data.StateMachines.RunStateMachine
  ( Clock,
    ReturnValue (Running, Term),
    RunSMResult,
    RunningSM (..),
    constructRunningSM,
    runSM,
    updateCurrentState,
  )
import Data.StateMachines.StateMachine (StateID, StateMachine (..), Transition, runStep)
import Data.Vector ((!?))

-- | A data type meant to ease the use of NFAs.
data NFAData a = Epsilon | Val a deriving (Show, Eq, Ord)

-- | A type alias that represents the default type for NFAs.
type NFA a = StateMachine (NFAData a) Set ()

-- | A type alias that represents the default type for NFA transitions.
type NFATransition a = Transition (NFAData a) ()

-- | A type alias that represents the default type for running NFAs.
type RunNFA a = RunningSM [] (NFAData a) Set ()

-- | A type alias that represents the default type for the result of
-- running a NFA.
type RunNFAResult a = RunSMResult [] (NFAData a) Set ()

-- | Takes an input list of type @a@, a `Data.StateMachines.RunStateMachine.Clock`, and an
-- `Data.StateMachines.NFA.NFA` with language @a@, and returns the result of running that.
--
-- Check `Data.StateMachines.RunStateMachine.extractResult` and
-- `Data.StateMachines.RunStateMachine.extractErrorAndMachine` to see how to
-- extract values from it.
runNFA :: (Ord a) => [a] -> Clock -> NFA a -> RunNFAResult a
runNFA tape' clk nfa = getRunNFA tape' clk nfa >>= runSM

-- | Constructs the `RunNFA` value for a given input, clock, and `NFA`.
getRunNFA :: (Ord a) => [a] -> Clock -> NFA a -> RunNFAResult a
getRunNFA tape' clk nfa = do
  let rnfa = constructRunningSM (Val <$> tape') clk nfa (\_ x -> tail x) stepFunc haltingFunc
  newStartStates <- leftFunc (expandEpsilon (currentState rnfa) (stateMachine rnfa)) rnfa
  return $ updateCurrentState newStartStates rnfa
  where
    leftFunc (Left s) rnfa = Left (s, rnfa)
    leftFunc (Right nss) _ = Right nss
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

-- | Continuously expands the current set of states via epsilon transitions until the set
-- of states no longer expands from these states.
expandEpsilon :: (Ord a) => Set StateID -> NFA a -> Error (Set StateID)
expandEpsilon ss nfa@StateMachine {..} = do
  ms <- mapM stepEpsilons (filter (>= 0) $ S.toList ss)
  let ss' = S.unions $ mapMaybe (fmap fst . M.lookup Epsilon) ms
  if ss' `S.isSubsetOf` ss then return ss else expandEpsilon (S.union ss ss') nfa
  where
    stepEpsilons s = maybeToEither "Could not find state (expandEpsilon)" (transitions !? s)
