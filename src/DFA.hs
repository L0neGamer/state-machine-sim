module DFA where

import Data.Set as S
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
      | Prelude.null as && s >= 0 = Term $ s `S.member` acceptStateIDs
      | s >= 0 = Running
      | otherwise = Term False

runExampleDFA :: [Int] -> Error (Either (String, RunDFA Int) (RunDFA Int))
runExampleDFA inp = exampleDFA >>= runDFA inp (clock 100)

-----
-- instance StateMachine DFAStateMachine where
--   constructStateMachine = constructStateMachine' (fromTuplesToMap const) DFAStatMac

--   addTransition (start, dest, val) DFAStatMac {..}
--     | not (S.fromList [start, dest] `S.isSubsetOf` states) = error "transition states not subset of states"
--     | val `S.notMember` language = error "transition language not subset of language"
--     | otherwise = DFAStatMac states language (M.unionWith M.union (M.singleton start (M.singleton val dest)) transitions) startState acceptStates

--   removeTransition (start, dest, val) dfa@DFAStatMac {..}
--     | M.findWithDefault Dead val submap == dest = DFAStatMac states language (M.insert start (M.delete val (M.findWithDefault M.empty start transitions)) transitions) startState acceptStates
--     | otherwise = dfa
--     where
--       submap = M.findWithDefault M.empty start transitions

--   stepMachine state transition DFAStatMac {..} = S.singleton (M.findWithDefault Dead transition (M.findWithDefault M.empty state transitions))

--   run xs iters dfa = returnValue $ runSM $ runDFA xs iters dfa

--   smAcceptStates = acceptStates

--   nextStates dfa@DFAStatMac {..} s = foldr S.union S.empty $ S.map (\l -> stepMachine s l dfa) language

--   reachableStates dfa curr = reachableStates' dfa (S.singleton curr) S.empty

-- instance RunningStateMachine RunningDFA where
--   step RunDFA {word = [], ..} = RunDFA [] currentState (Term (currentState `S.member` acceptStates dfa)) remainingIter dfa
--   step RunDFA {word = x : xs, ..}
--     | remainingIter < I 1 0 = RunDFA (x : xs) currentState Timeout remainingIter dfa
--     | otherwise = runningDFA
--     where
--       nextState = fromSingleton $ stepMachine currentState x dfa
--       returnValue'
--         | nextState == Dead = Term False
--         | otherwise = Running
--       runningDFA = RunDFA xs nextState returnValue' (tickClock remainingIter) dfa

--   getReturnValue = returnValue

-- runDFA :: (Ord a) => [a] -> Clock -> DFAStateMachine a -> RunningDFA a
-- runDFA xs iters dfa = RunDFA xs (startState dfa) Running iters dfa

-- q0, q1, q2, q3, q4, q5 :: State
-- q0 : q1 : q2 : q3 : q4 : q5 : _ = allIntStates

-- -- accepts 101*0
-- exampleDFA :: DFAStateMachine Integer
-- exampleDFA = inferStateMachine [(q0, q1, 1), (q1, q2, 0), (q2, q2, 1), (q2, q3, 0)] q0 (S.singleton q3)

-- emptyDFA :: DFAStateMachine Integer
-- emptyDFA = constructStateMachine (S.fromList [q0]) S.empty [] q0 S.empty
