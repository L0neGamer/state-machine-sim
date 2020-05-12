{-# LANGUAGE OverloadedStrings, RecordWildCards, InstanceSigs #-}
module TuringMachine
 where

import Lib

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (genericTake)

data Sym a = Blank | Val a deriving (Show, Eq, Ord)

data TapeDir = L
             | R
             deriving (Show, Eq)

type TuringTransition a = M.Map a (State, a, TapeDir)

type TuringTransitions a = M.Map State (TuringTransition a)

data Tape a = Tape {
      right  :: [a]
    , left   :: [a]
    , cursor :: Integer
}

instance Show a => Show (Tape a) where
    show Tape{..} = "Tape " ++ show (take 10 right) ++ (' ':show (take 10 left)) ++ ' ':displayCursor
        where displayCursor | cursor < 0 = '(':show cursor ++ ")"
                            | otherwise = show cursor

getVal :: Tape a -> a
getVal Tape{..} = head right

moveCursor :: TapeDir -> Tape a -> Tape a
moveCursor L Tape{..} = Tape (head left:right) (tail left) (cursor - 1)
moveCursor R Tape{..} = Tape (tail right) (head right:left) (cursor + 1)

insertVal :: a -> Tape a -> Tape a
insertVal val Tape{..} = Tape (val:tail right) left cursor

data TuringMachine a =
    TuringMac {
        states :: States,
        language :: Language a,
        blank :: a,
        startState :: State,
        acceptStates :: AcceptStates,
        transitions :: TuringTransitions a
    } deriving (Show)

data RunningTM a =
    RunTM {
        tape :: Tape a,
        currentState :: State,
        returnValue :: ReturnValue,
        remainingIter :: Clock,
        turingMachine :: TuringMachine a
    } deriving (Show)

constructTuringMachine :: Ord a => States -> Language a -> a -> State -> AcceptStates -> Transitions (a, a, TapeDir) -> TuringMachine a
constructTuringMachine states language blank startState acceptStates transitions
    | startState `S.notMember` states = error "start state not in set of states"
    | not (acceptStates `S.isSubsetOf` states) = error "accept states not subset of states"
    | blank `S.notMember` language = error "blank symbol not in language"
    | not (transitionStates `S.isSubsetOf` states) = error "transition states not subset of states"
    | not (transitionLanguage `S.isSubsetOf` language) = error "transition language not subset of language"
    | not $ S.disjoint (S.fromList (map (\(a,_,_) -> a) transitions)) (acceptStates) = error "some accept states have outward transitions"
    | otherwise = TuringMac states language blank startState acceptStates transitions'
    where (transitionStates, transitionLanguage') = getStatesAndLang (map (\(a, b, (c, _, _)) -> (a, b, c)) transitions)
          transitionLanguage = S.union transitionLanguage' (S.fromList $ map (\(_, _, (_, a, _)) -> a) transitions)
          transitions' = fromTuplesToMap const $ map (\(a, b, (c, d, e)) -> (a, (b, d, e), c)) transitions

inferTuringMachine :: Ord a => a -> State -> AcceptStates -> Transitions (a, a, TapeDir) -> TuringMachine a
inferTuringMachine blank startState acceptStates transitions = constructTuringMachine transitionStates transitionLanguage blank startState acceptStates transitions
    where (transitionStates, transitionLanguage') = getStatesAndLang (map (\(a, b, (c, _, _)) -> (a, b, c)) transitions)
          transitionLanguage = S.union transitionLanguage' (S.fromList $ map (\(_, _, (_, a, _)) -> a) transitions)

stepTuringMachine :: (Ord a) => State -> a -> TuringMachine a -> (State, a, TapeDir)
stepTuringMachine Dead a _ = (Dead, a, R)
stepTuringMachine state transition TuringMac{..} = M.findWithDefault (Dead, transition, R) transition (M.findWithDefault M.empty state transitions)

runTuringMachine' :: (Ord a) => [a] -> Clock -> TuringMachine a -> RunningTM a
runTuringMachine' xs iters tm = runSM $ runTM xs iters tm

runTuringMachine :: (Ord a) => [a] -> Clock -> TuringMachine a -> ReturnValue
runTuringMachine xs iters tm = returnValue $ runTuringMachine' xs iters tm

runTM :: (Ord a) => [a] -> Clock -> TuringMachine a -> RunningTM a
runTM xs iters tm = RunTM tape (startState tm) Running iters tm
    where emptyTape = (blank tm):emptyTape
          tape = Tape (xs ++ emptyTape) emptyTape 0

instance RunningStateMachine RunningTM where
    getReturnValue = returnValue
    step RunTM{currentState=Dead,..} = RunTM tape Dead (Term False) remainingIter turingMachine
    step RunTM{..}
        | currentState `S.member` (acceptStates turingMachine) = RunTM tape currentState (Term True) remainingIter turingMachine
        | remainingIter < I 1 0 = RunTM tape currentState Timeout remainingIter turingMachine
        | otherwise = runningTM
        where (nextState, toWrite, dir) = stepTuringMachine currentState (getVal tape) turingMachine
              tape' = moveCursor dir $ insertVal toWrite tape
              runningTM = RunTM tape' nextState Running (tickClock remainingIter) turingMachine

integerTape :: Tape Integer
integerTape = Tape [0,0..] [0,0..] 0

q0, q1, q2, q3, q4, q5 :: State
q0:q1:q2:q3:q4:q5:_ = allIntStates

qH :: State
qH = IdI (-1)

type BusyBeaverStore = (Integer, Integer, TuringMachine Integer)

-- https://en.wikipedia.org/wiki/Busy_beaver#Examples

-- 6 1s, 14 steps
busyBeaver3State :: BusyBeaverStore
busyBeaver3State = (6, 14, inferTuringMachine 0 q0 (S.singleton qH) transitions)
    where transitions = ([(q0,q1,(0,1,R)), (q0,qH,(1,1,R)),
                          (q1,q2,(0,0,R)), (q1,q1,(1,1,R)),
                          (q2,q2,(0,1,L)), (q2,q0,(1,1,L))])

-- 13 1s, 107 steps
busyBeaver4State :: BusyBeaverStore
busyBeaver4State = (13, 107, inferTuringMachine 0 q0 (S.singleton qH) transitions)
    where transitions = ([(q0,q1,(0,1,R)), (q0,q1,(1,1,L)),
                          (q1,q0,(0,1,L)), (q1,q2,(1,0,L)),
                          (q2,qH,(0,1,R)), (q2,q3,(1,1,L)),
                          (q3,q3,(0,1,R)), (q3,q0,(1,0,R))])

-- 4098 1s, 47176870 steps
busyBeaver5State :: BusyBeaverStore
busyBeaver5State = (4098, 47176870, inferTuringMachine 0 q0 (S.singleton qH) transitions)
    where transitions = ([(q0,q1,(0,1,R)), (q0,q2,(1,1,L)),
                          (q1,q2,(0,1,R)), (q1,q1,(1,1,R)),
                          (q2,q3,(0,1,R)), (q2,q4,(1,0,L)),
                          (q3,q0,(0,1,L)), (q3,q3,(1,1,L)),
                          (q4,qH,(0,1,R)), (q4,q0,(1,0,L))])

busyBeaverCheck :: BusyBeaverStore -> (Integer, Integer, RunningTM Integer)
busyBeaverCheck (ones, steps, tm) = (ones - tapeSum, steps - timeSpent, runMachine)
    where runMachine = runTuringMachine' [] (clock (-1)) tm
          timeSpent = getTime (remainingIter runMachine)
          tape' = tape runMachine
          tapeSum = (sum (genericTake (timeSpent - cursor tape') $ right tape')) + (sum (genericTake (timeSpent + cursor tape') $ left tape'))
