module Examples
  ( busyBeaver3State,
    busyBeaver4State,
    busyBeaver5State,
    busyBeaverCheck,
    emptyDFA,
    exampleDFA,
    runExampleDFA,
  )
where

import DFA (DFA, RunDFAResult, runDFA)
import Data.Foldable (Foldable (toList))
import Data.Set as S (empty, singleton)
import GHC.OldList (genericTake)
import Lib (Error)
import RunStateMachine
  ( RunningSM (remainingIter, tape),
    clock,
    extractErrorAndMachine,
    getTime,
  )
import StateMachine
  ( State (State),
    Transition (Transition),
    constructStateMachine,
    inferStateMachine,
    tupleToSimpleTransition,
  )
import TuringMachine
  ( RunTuringMachine,
    Tape (cursor, left, right),
    TapeDir (L, R),
    TuringMachine,
    runTuringMachine,
    startingTape,
  )

q0, q1, q2, q3, q4 :: State
q0 : q1 : q2 : q3 : q4 : _ = fmap (State . show) ([0 ..] :: [Integer])

qH :: State
qH = State "haltingState"

--- DFA testing

-- | @exampleDFA@ accepts 101*0
exampleDFA :: Error (DFA Int)
exampleDFA =
  inferStateMachine
    "DFA accepts 101*0"
    ( fmap
        tupleToSimpleTransition
        [ (q0, q1, 1),
          (q1, q2, 0),
          (q2, q2, 1),
          (q2, q3, 0)
        ]
    )
    q0
    (S.singleton q3)
    const

-- | @runExampleDFA@ runs @exampleDFA@ on a given inpu
runExampleDFA :: [Int] -> RunDFAResult Int
runExampleDFA inp = exampleDFA >>= runDFA inp (clock 100)

-- | @emptyDFA@ accepts nothing
emptyDFA :: (Ord a) => Error (DFA a)
emptyDFA = constructStateMachine "empty DFA" S.empty (S.singleton q0) [] q0 S.empty const

--- BusyBeaver testing (turing machines)
-- see https://en.wikipedia.org/wiki/Busy_beaver#Examples
type BusyBeaverStore = (Integer, Integer, TuringMachine Integer)

-- | Utility function so that I don't have to manually recode the busybeavers
convOldFormNewForm ::
  (State, State, (Integer, Integer, TapeDir)) ->
  Transition Integer (Integer, TapeDir)
convOldFormNewForm (s, s', (a, e, e')) = Transition s s' a (e, e')

-- | @busyBeaver3State@ is a turing machine that outputs 6 1's over 14 steps
busyBeaver3State :: Error BusyBeaverStore
busyBeaver3State = do
  sm <- inferStateMachine "busyBeaver3State" transitions q0 (S.singleton qH) const
  return (6, 14, sm)
  where
    transitions =
      fmap
        convOldFormNewForm
        [ (q0, q1, (0, 1, R)),
          (q0, qH, (1, 1, R)),
          (q1, q2, (0, 0, R)),
          (q1, q1, (1, 1, R)),
          (q2, q2, (0, 1, L)),
          (q2, q0, (1, 1, L))
        ]

-- | @busyBeaver4State@ is a turing machine that outputs 13 1's over 107 steps
busyBeaver4State :: Error BusyBeaverStore
busyBeaver4State = do
  sm <- inferStateMachine "busyBeaver4State" transitions q0 (S.singleton qH) const
  return (13, 107, sm)
  where
    transitions =
      fmap
        convOldFormNewForm
        [ (q0, q1, (0, 1, R)),
          (q0, q1, (1, 1, L)),
          (q1, q0, (0, 1, L)),
          (q1, q2, (1, 0, L)),
          (q2, qH, (0, 1, R)),
          (q2, q3, (1, 1, L)),
          (q3, q3, (0, 1, R)),
          (q3, q0, (1, 0, R))
        ]

-- | @busyBeaver5State@ is a turing machine that outputs 4098 1's over 47176870 steps
busyBeaver5State :: Error BusyBeaverStore
busyBeaver5State = do
  sm <- inferStateMachine "busyBeaver5State" transitions q0 (S.singleton qH) const
  return (4098, 47176870, sm)
  where
    transitions =
      fmap
        convOldFormNewForm
        [ (q0, q1, (0, 1, R)),
          (q0, q2, (1, 1, L)),
          (q1, q2, (0, 1, R)),
          (q1, q1, (1, 1, R)),
          (q2, q3, (0, 1, R)),
          (q2, q4, (1, 0, L)),
          (q3, q0, (0, 1, L)),
          (q3, q3, (1, 1, L)),
          (q4, qH, (0, 1, R)),
          (q4, q0, (1, 0, L))
        ]

-- | @busyBeaverCheck@ takes a @BusyBeaverStore@ and returns whether the number
-- of 1's and the number of steps matches the requested ones
busyBeaverCheck :: BusyBeaverStore -> Error (Bool, Bool, RunTuringMachine Integer)
busyBeaverCheck (ones, steps, tm) = do
  (_, runMachine) <- extractErrorAndMachine runMachine'
  let timeSpent = getTime (remainingIter runMachine)
      tape' = tape runMachine
      tapeSum =
        sum (genericTake (timeSpent - cursor tape') $ toList (right tape'))
          + sum (genericTake (timeSpent + cursor tape') $ toList (left tape'))
  return (ones == tapeSum, steps == timeSpent, runMachine)
  where
    runMachine' = runTuringMachine (startingTape 0) (clock 0) tm
