{-# LANGUAGE OverloadedStrings, RecordWildCards, InstanceSigs #-}
module TuringMachine
 where

import Lib

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (genericSplitAt, genericIndex, genericTake)

import Debug.Trace (trace)

data Sym a = Blank | Val a deriving (Show, Eq, Ord)

data TapeDir = L 
            --  | N 
             | R 
             deriving (Show, Eq)

type TuringTransition a = M.Map a (State, a, TapeDir)

type TuringTransitions a = M.Map State (TuringTransition a)

data Tape a = Tape {
      cursor :: Integer
    , right  :: [a]
    , left   :: [a]
}

instance Show a => Show (Tape a) where
    show Tape{..} = "Tape " ++ show cursor ++ " " ++ show (genericTake cursor' right) ++ " " ++ show (genericTake cursor' left)
        where cursor' = max ((abs cursor) * 2) 10

(!<) :: Tape a -> Integer -> a
(!<) Tape{..} i
    | i < 0 = left `genericIndex` ((abs i) - 1)
    | otherwise = right `genericIndex` i

getVal :: Tape a -> a
getVal t@Tape{..} = t !< cursor

moveCursor :: TapeDir -> Tape a -> Tape a
moveCursor L Tape{..} = Tape (cursor - 1) right left
moveCursor R Tape{..} = Tape (cursor + 1) right left
-- moveCursor N t = t

insertAt :: Integer -> a -> Tape a -> Tape a
insertAt i val Tape{..}
    | i < 0 = Tape cursor right left'
    | otherwise = Tape cursor right' left
    where i' = (abs i) - 1
          (as, _:bs) = genericSplitAt i right
          (as', _:bs') = genericSplitAt i' left
          right' = as ++ val:bs
          left' = as' ++ val:bs'

insertVal :: a -> Tape a -> Tape a
insertVal val t@Tape{..} = insertAt cursor val t

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

runTuringMachine xs iters tm = returnValue $ runTuringMachine' xs iters tm

runTM :: (Ord a) => [a] -> Clock -> TuringMachine a -> RunningTM a
runTM xs iters tm = RunTM tape (startState tm) Running iters tm
    where emptyTape = (blank tm):emptyTape
          tape = Tape 0 (xs ++ emptyTape) emptyTape

instance RunningStateMachine RunningTM where
    getReturnValue = returnValue
    step RunTM{currentState=Dead,..} = RunTM tape Dead (Term False) remainingIter turingMachine
    step RunTM{..}
        | currentState `S.member` (acceptStates turingMachine) = RunTM tape currentState (Term True) remainingIter turingMachine
        | otherwise = runningTM
        where (nextState, toWrite, dir) = stepTuringMachine currentState (getVal tape) turingMachine
              tape' = insertVal toWrite tape
              tape'' = moveCursor dir tape'
              runningTM = RunTM tape'' nextState Running (tickClock remainingIter) turingMachine

integerTape :: Tape Integer
integerTape = Tape 0 [0,0..] [0,0..] 

q0, q1, q2, q3, q4, q5 :: State
q0:q1:q2:q3:q4:q5:_ = allIntStates

busyBeaver3State :: TuringMachine Integer
busyBeaver3State = inferTuringMachine 0 q0 (S.singleton q3) transitions
    where transitions = ([(q0,q1,(0,1,R)), (q0,q2,(1,1,L)),
                          (q1,q0,(0,1,L)), (q1,q1,(1,1,R)),
                          (q2,q1,(0,1,L)), (q2,q3,(1,1,R))])

busyBeaverCheck :: Tape Integer
busyBeaverCheck = tape $ runTuringMachine' [] (Infinite 0) busyBeaver3State