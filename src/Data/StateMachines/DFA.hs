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

-- | @DFA@ is a type alias that represents the default type for DFAs
type DFA a = StateMachine a Identity ()

-- | @DFATransition@ is a type alias that represents the default type for DFA transitions
type DFATransition a = Transition a ()

-- | @RunDFA@ is a type alias that represents the default type for running DFAs
type RunDFA a = RunningSM [] a Identity ()

-- | @RunDFAResult@ is a type alias that represents the default type for the result of
-- running a DFA
type RunDFAResult a = RunSMResult [] a Identity ()

-- | @runDFA@ takes an input list of type @a@, a Clock, and a DFA with language @a@, and
-- returns the result of running that.
-- Check @extractResult@ and @extractErrorAndMachine@ from @RunStateMachine@ to see how to
-- extract values from it.
runDFA :: Ord a => [a] -> Clock -> DFA a -> RunDFAResult a
runDFA as ck dfa = runSM $ getRunDFA as ck dfa

-- | @getRunDFA@ constructs the @RunDFA@ value for a given input, clock, and @DFA@
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
