module StateMachineSim.StateMachines.NFA
  ( NFATransition,
    NFA,
    RunNFA,
    runNFA,
    NFAData (..),
  )
where

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
import StateMachineSim.Lib.Lib (Error, dropNothings, maybeToEither)
import StateMachineSim.Lib.RunStateMachine
  ( Clock,
    ReturnValue (Running, Term),
    RunSMResult,
    RunningSM (..),
    constructRunningSM,
    runSM,
    updateCurrentState,
  )
import StateMachineSim.Lib.StateMachine (StateID, StateMachine (..), Transition, runStep)

-- | @NFAData@ is a data type meant to ease the use of NFAs
data NFAData a = Epsilon | Val a deriving (Show, Eq, Ord)

-- | @NFA@ is a type alias that represents the default type for NFAs
type NFA a = StateMachine (NFAData a) Set ()

-- | @NFATransition@ is a type alias that represents the default type for NFA transitions
type NFATransition a = Transition (NFAData a) ()

-- | @RunNFA@ is a type alias that represents the default type for running NFAs
type RunNFA a = RunningSM [] (NFAData a) Set ()

-- | @RunNFAResult@ is a type alias that represents the default type for the result of
-- running a NFA
type RunNFAResult a = RunSMResult [] (NFAData a) Set ()

-- | @runNFA@ takes an input list of type @a@, a Clock, and an NFA with language @a@, and
-- returns the result of running that.
-- Check @extractResult@ and @extractErrorAndMachine@ from @RunStateMachine@ to see how to
-- extract values from it.
runNFA :: (Ord a) => [a] -> Clock -> NFA a -> RunNFAResult a
runNFA tape' clk nfa = getRunNFA tape' clk nfa >>= runSM

-- | @getRunNFA@ constructs the @RunNFA@ value for a given input, clock, and @NFA@
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

-- | @expandEpsilon@ continuously expands the current set of states via epsilon
-- transitions until the set of states no longer expands from these states
expandEpsilon :: (Ord a) => Set StateID -> NFA a -> Error (Set StateID)
expandEpsilon ss nfa@StateMachine {..} = do
  ms <- mapM stepEpsilons (filter (>= 0) $ S.toList ss)
  let ss' = S.unions $ dropNothings (fmap (fmap fst . M.lookup Epsilon) ms)
  if ss' `S.isSubsetOf` ss then return ss else expandEpsilon (S.union ss ss') nfa
  where
    stepEpsilons s = maybeToEither "Could not find state (expandEpsilon)" (transitions !? s)

-----
-- stepThroughEpsilons :: (Ord a) => NFAStateMachine a -> State -> States
-- stepThroughEpsilons NFAStatMac {..} state = M.findWithDefault S.empty Epsilon (M.findWithDefault M.empty state transitions)

-- expandEpsilon :: (Ord a) => NFAStateMachine a -> States -> States
-- expandEpsilon nfa s
--   | next `S.isSubsetOf` s = s
--   | otherwise = expandEpsilon nfa (S.union s next)
--   where
--     next = S.unions $ S.map (stepThroughEpsilons nfa) s
