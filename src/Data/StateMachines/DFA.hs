-- |
-- Module      :  Data.StateMachines.DFA
-- License     :  BSD3
--
-- Maintainer  :  L0neGamer
-- Stability   :  experimental
--
-- Functions and types for constructing and running DFAs.
module Data.StateMachines.DFA
  ( DFATransition,
    DFA,
    RunDFA,
    RunDFAResult,
    runDFA,
  )
where

import Data.Functor.Identity (Identity (Identity))
import Data.Set as S (member)
import Data.StateMachines.RunStateMachine
  ( Clock,
    ReturnValue (Running, Term),
    RunSMResult,
    RunningSM (..),
    constructRunningSM,
    runSM,
  )
import Data.StateMachines.StateMachine (StateMachine (..), Transition, runStep)

-- | A type alias that represents the default type for DFAs.
type DFA a = StateMachine a Identity ()

-- | A type alias that represents the default type for DFA transitions.
type DFATransition a = Transition a ()

-- | A type alias that represents the default type for running DFAs.
type RunDFA a = RunningSM [] a Identity ()

-- | A type alias that represents the default type for the result of
-- running a DFA.
type RunDFAResult a = RunSMResult [] a Identity ()

-- | Takes an input list of type @a@, a `Data.StateMachines.RunStateMachine.Clock`, and a
-- `Data.StateMachines.DFA.DFA` with language @a@, and returns the result of running that.
--
-- Check `Data.StateMachines.RunStateMachine.extractResult` and
-- `Data.StateMachines.RunStateMachine.extractErrorAndMachine` to see how to
-- extract values from it.
runDFA :: Ord a => [a] -> Clock -> DFA a -> RunDFAResult a
runDFA as ck dfa = runSM $ getRunDFA as ck dfa

-- TODO: further unify NFAs, DFAs, and Turing machines in their getRun functions
-- there are a lot of similarities between them, so unifying them would be good

-- | Constructs the `RunDFA` value for a given input, clock, and `DFA`.
getRunDFA :: (Ord a) => [a] -> Clock -> DFA a -> RunDFA a
getRunDFA tape' clk dfa =
  constructRunningSM
    tape'
    clk
    dfa
    (\_ x -> tail x)
    stepFunc
    haltingFunc
  where
    stepFunc (Identity s) l RunSM {..} = do
      (s', _) <- runStep stateMachine s l
      return (s', ())
    haltingFunc (Identity s) _ as StateMachine {..}
      | null as && s >= 0 = Term $ s `S.member` acceptStateIDs
      | s >= 0 = Running
      | otherwise = Term False
