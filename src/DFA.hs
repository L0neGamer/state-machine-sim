module DFA (DFATransition, DFA, RunDFA, RunDFAResult, runDFA) where

import Data.Set as S (member)
import Lib (Error, Single (..))
import RunStateMachine
  ( Clock,
    ReturnValue (Running, Term),
    RunSMResult,
    RunningSM (..),
    constructRunningSM,
    runSM,
  )
import StateMachine (StateMachine (..), Transition, runStep)

type DFA a = StateMachine a Single ()

type DFATransition a = Transition a ()

type RunDFA a = RunningSM [] a Single ()

type RunDFAResult a = RunSMResult [] a Single ()

runDFA :: (Ord a) => [a] -> Clock -> DFA a -> RunDFAResult a
runDFA tape' clk dfa = do
  rdfa <- getRunDFA tape' clk dfa
  return $ runSM rdfa

getRunDFA :: (Ord a) => [a] -> Clock -> DFA a -> Error (RunDFA a)
getRunDFA tape' clk dfa =
  constructRunningSM
    tape'
    clk
    dfa
    (\_ x -> tail x)
    stepFunc
    haltingFunc
  where
    stepFunc (Single s) l RunSM {..} = do
      (s', _) <- runStep stateMachine s l
      return (s', ())
    haltingFunc (Single s) _ as StateMachine {..}
      | null as && s >= 0 = Term $ s `S.member` acceptStateIDs
      | s >= 0 = Running
      | otherwise = Term False
