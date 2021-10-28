module NFA (NFATransition, NFA, RunNFA, runNFA) where

import Data.Map as M (lookup)
import Data.Set as S (Set, empty, foldr, intersection, null, toList, union, unions)
import Data.Vector ((!?))
import Lib (Error, dropNothings, maybeToError)
import RunStateMachine
  ( Clock,
    ReturnValue (Running, Term),
    RunningSM (..),
    constructRunningSM,
    runSM,
  )
import StateMachine (StateID, StateMachine (..), Transition, runStep)

type NFA a = StateMachine (Maybe a) Set ()

type NFATransition a = Transition (Maybe a) ()

type RunNFA a = RunningSM [] (Maybe a) Set ()

runNFA :: (Ord a) => [a] -> Clock -> NFA a -> Error (Either (String, RunNFA a) (RunNFA a))
runNFA tape' clk nfa = do
  rnfa <- getRunNFA tape' clk nfa
  Right $ runSM rnfa

getRunNFA :: (Ord a) => [a] -> Clock -> NFA a -> Error (RunNFA a)
getRunNFA tape' clk nfa = constructRunningSM (Just <$> tape') clk nfa (\_ x -> tail x) stepFunc haltingFunc
  where
    stepFunc ss l RunSM {..} = do
      cleanss' <- expandEpsilon ss stateMachine
      statesList <- mapM (\s -> runStep stateMachine s l) (S.toList cleanss')
      return (Prelude.foldr (S.union . fst) S.empty statesList, ())
    haltingFunc ss _ as StateMachine {..}
      | Prelude.null as && allVals = Term $ not . S.null $ ss `S.intersection` acceptStateIDs
      | allVals = Running
      | otherwise = Term False
      where
        allVals = S.foldr (\a b -> (a >= 0) || b) False ss

expandEpsilon :: (Ord a) => Set StateID -> NFA a -> Error (Set StateID)
expandEpsilon ss nfa@StateMachine {..} = do
  ms <- mapM (\s -> maybeToError "Could not find state (expand epsilon)" (transitions !? s)) (filter (>= 0) $ S.toList ss)
  let ss' = S.unions $ ss : dropNothings (fmap (fmap fst . M.lookup Nothing) ms)
  if ss == ss' then return ss' else expandEpsilon ss' nfa

-----
-- stepThroughEpsilons :: (Ord a) => NFAStateMachine a -> State -> States
-- stepThroughEpsilons NFAStatMac {..} state = M.findWithDefault S.empty Epsilon (M.findWithDefault M.empty state transitions)

-- expandEpsilon :: (Ord a) => NFAStateMachine a -> States -> States
-- expandEpsilon nfa s
--   | next `S.isSubsetOf` s = s
--   | otherwise = expandEpsilon nfa (S.union s next)
--   where
--     next = S.unions $ S.map (stepThroughEpsilons nfa) s
