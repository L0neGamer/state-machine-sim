-- |
-- Module      :  Data.StateMachines.Examples
-- License     :  BSD3
--
-- Maintainer  :  L0neGamer
-- Stability   :  experimental
--
-- Functions and state machines meant for demonstrating how to use various state machines.
--
-- Includes some DFAs and "Busy beaver" examples for Turing machines.
module Data.StateMachines.Examples
  ( emptyDFA,
    exampleDFA,
    runExampleDFA,
    helloThereRegexNFA,
    BusyBeaverStore,
    busyBeaver3State,
    busyBeaver4State,
    busyBeaver5State,
    busyBeaverCheck,
  )
where

import Data.Foldable (Foldable (toList))
import Data.List (genericTake)
import Data.Set as S (empty, singleton)
import Data.StateMachines.DFA (DFA, RunDFAResult, runDFA)
import Data.StateMachines.Internal (Const (..), Error, fromConst)
import Data.StateMachines.NFA (NFA)
import Data.StateMachines.Regex (regexStrToNFA)
import Data.StateMachines.RunStateMachine
  ( Clock (time),
    RunningSM (remainingIter, tape),
    clock,
    extractErrorAndMachine,
  )
import Data.StateMachines.StateMachine
  ( ConsSM (consSM, inferSM),
    State (State),
    Transition (Transition),
    tupleToSimpleTransition,
  )
import Data.StateMachines.TuringMachine
  ( RunTuringMachine,
    Tape (cursor, left, right),
    TapeDir (L, R),
    TuringMachine,
    TuringMachineTransition,
    blankTape,
    runTuringMachine,
  )

-- | Basic states for use in examples.
q0, q1, q2, q3, q4 :: State
q0 : q1 : q2 : q3 : q4 : _ = fmap (State . show) ([0 ..] :: [Integer])

-- | A state earmarked for being the final state, but has no unique properties.
qH :: State
qH = State "haltingState"

--- DFA testing

-- | A DFA that accepts @101*0@.
exampleDFA :: Error (DFA Int)
exampleDFA =
  inferSM
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

-- | Runs `exampleDFA` on a given input, with a maximum clock time of 100.
runExampleDFA :: [Int] -> Error (RunDFAResult Int)
runExampleDFA inp = runDFA inp (clock 100) <$> exampleDFA

-- | This simple DFA accepts no input.
emptyDFA :: Ord a => Error (DFA a)
emptyDFA = consSM "empty DFA" S.empty (S.singleton q0) [] q0 S.empty

--- NFA and REGEX testing

helloThereRegexNFA :: Error (NFA Char)
helloThereRegexNFA = regexStrToNFA "hel*o the(re|ba)*"

--- BusyBeaver testing (turing machines)
-- see https://en.wikipedia.org/wiki/Busy_beaver#Examples

-- | A convenient way to store data about busy beaver machines. Stores the number of 1s
-- produced, the number of steps required, and the machine which should achieve those.
type BusyBeaverStore = (Integer, Integer, TuringMachine (Const Integer))

-- | A utility function so that the old format for the busy beavers can be reused.
convOldFormNewForm ::
  (State, State, (Integer, Integer, TapeDir)) -> TuringMachineTransition (Const Integer)
convOldFormNewForm (s, s', (a, e, e')) = Transition s s' (Const a) (Const e, e')

-- | A turing machine that outputs six 1's over 14 steps.
busyBeaver3State :: Error BusyBeaverStore
busyBeaver3State = do
  sm <- inferSM "busyBeaver3State" transitions q0 (S.singleton qH)
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

-- | A turing machine that outputs thirteen 1's over 107 steps.
busyBeaver4State :: Error BusyBeaverStore
busyBeaver4State = do
  sm <- inferSM "busyBeaver4State" transitions q0 (S.singleton qH)
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

-- | A turing machine that outputs 4098 1's over 47176870 steps. This will take a long
-- time to run and process, so execute with caution - but it will halt at some stage.
busyBeaver5State :: Error BusyBeaverStore
busyBeaver5State = do
  sm <- inferSM "busyBeaver5State" transitions q0 (S.singleton qH)
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

-- | Takes a `Data.StateMachines.Examples.BusyBeaverStore` and returns whether the number
-- of 1's and the number of steps matches the requested ones. If the first two values are
-- true, then the check was performed successfully. The end
-- `Data.StateMachines.TuringMachine.RunTuringMachine` is also returned if manual
-- inspection is wanted.
busyBeaverCheck :: BusyBeaverStore -> (Bool, Bool, RunTuringMachine (Const Integer))
busyBeaverCheck (ones, steps, tm) = (ones == tapeSum, steps == timeSpent, runMachine)
  where
    runMachine' = runTuringMachine (blankTape (Const 0)) (clock 0) tm
    (_, runMachine) = extractErrorAndMachine runMachine'
    timeSpent = time (remainingIter runMachine)
    tape' = fromConst <$> tape runMachine
    tapeSum =
      sum (genericTake (timeSpent - cursor tape') $ toList (right tape'))
        + sum (genericTake (timeSpent + cursor tape') $ toList (left tape'))
