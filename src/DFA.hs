module DFA where

import Data.Set as S (empty, member, singleton)
import Lib (Error, Single (..))
import RunStateMachine
import StateMachine

type DFA a = StateMachine a Single ()

type DFATransition a = Transition a ()

type RunDFA a = RunningSM [] a Single ()

runDFA :: (Ord a) => [a] -> Clock -> DFA a -> Error (Either (String, RunDFA a) (RunDFA a))
runDFA tape' clk dfa = do
  rdfa <- getRunDFA tape' clk dfa
  Right $ runSM rdfa

exampleDFA :: Error (DFA Int)
exampleDFA = inferStateMachine "DFA accepts 101*0" (fmap tupleToSimpleTransition [(q0, q1, 1), (q1, q2, 0), (q2, q2, 1), (q2, q3, 0)]) q0 (S.singleton q3) const

emptyDFA :: (Ord a) => Error (DFA a)
emptyDFA = constructStateMachine "empty DFA" S.empty (S.singleton q0) [] q0 S.empty const

getRunDFA :: (Ord a) => [a] -> Clock -> DFA a -> Error (RunDFA a)
getRunDFA tape' clk dfa = constructRunningSM tape' clk dfa (\_ x -> tail x) stepFunc haltingFunc
  where
    stepFunc (Single s) l RunSM {..} = do
      (s', _) <- runStep stateMachine s l
      return (s', ())
    haltingFunc (Single s) _ as StateMachine {..}
      | null as && s >= 0 = Term $ s `S.member` acceptStateIDs
      | s >= 0 = Running
      | otherwise = Term False

runExampleDFA :: [Int] -> Error (Either (String, RunDFA Int) (RunDFA Int))
runExampleDFA inp = exampleDFA >>= runDFA inp (clock 100)
