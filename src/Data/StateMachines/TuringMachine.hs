-- |
-- Module      :  Data.StateMachines.TuringMachine
-- License     :  BSD3
--
-- Maintainer  :  L0neGamer
-- Stability   :  experimental
--
-- Functions and types for constructing and running Turing machines. Includes new data
-- types such as `Tape`s for guaranteed infinite inputs.
module Data.StateMachines.TuringMachine
  ( TapeDir (L, R),
    Tape (..),
    blankTape,
    fromListTape,
    TuringMachineTransition,
    TuringMachine,
    RunTuringMachine,
    RunTuringMachineResult,
    runTuringMachine,
    Stream (..),
    fromListStream,
    showStream,
  )
where

import Data.Foldable (Foldable (toList))
import Data.Functor.Identity (Identity (Identity))
import Data.List (genericTake)
import Data.Set as S (member)
import Data.StateMachines.RunStateMachine (Clock, Peekable (..), ReturnValue (Running, Term), RunSMResult, RunningSM (..), constructRunningSM, runSM)
import Data.StateMachines.StateMachine (StateMachine (StateMachine, acceptStateIDs), Transition, runStep)
import Safe (fromJustDef)

-- | A data type that determines which direction the tape should move.
data TapeDir
  = L
  | S
  | R
  deriving (Show, Eq)

-- | A type alias that represents the default type for Turing machines.
type TuringMachine a = StateMachine a Identity (a, TapeDir)

-- | A type alias that represents the default type for Turing machine transitions.
type TuringMachineTransition a = Transition a (a, TapeDir)

-- | A type alias that represents the default type for running Turing machines.
type RunTuringMachine a = RunningSM Tape a Identity (a, TapeDir)

-- | A type alias that represents the default type for the result of running a Turing
-- machine.
type RunTuringMachineResult a = RunSMResult Tape a Identity (a, TapeDir)

-- | An infinite data storage type.
data Stream a = Stream a (Stream a)

instance Functor Stream where
  fmap f (Stream a as) = Stream (f a) (fmap f as)

instance Foldable Stream where
  foldr f b (Stream a as) = a `f` foldr f b as

-- | The `Show` instance limits the output to 15 values by default, using `showStream`.
instance (Show a) => Show (Stream a) where
  show = showStream showStreamLimit
    where
      showStreamLimit = 15 :: Int

-- | Takes a default value of type @a@, a list of @a@s, and returns a
-- `Stream` prepended with the values in the list before having an infinite stream of the
-- default @a@s.
fromListStream :: a -> [a] -> Stream a
fromListStream a [] = st
  where
    st = Stream a st
fromListStream a (a' : as) = Stream a' (fromListStream a as)

-- | Returns a string representing the given `Stream` up to a given depth.
showStream :: (Show a, Integral i) => i -> Stream a -> String
showStream i st@(Stream a _) = "fromListStream (" ++ show infVal ++ ") " ++ show lst
  where
    lst = (genericTake i . toList) st
    infVal
      | i <= 0 = a
      | otherwise = last lst

-- | A `Stream` of unit values.
nullStream :: Stream ()
nullStream = Stream () nullStream

-- | Move the head of the first `Stream` to that of the other.
moveHeadTo :: Stream a -> Stream a -> (Stream a, Stream a)
moveHeadTo (Stream a as) bs = (as, Stream a bs)

-- | The data type that represents an infinite tape going off to the `left` and `right`,
-- with a `cursor` designating how far the tape currently is from the start location.
data Tape a = Tape
  { -- | The tape contents including the cursor, and going to the right.
    right :: Stream a,
    -- | The tape contents to the left of the cursor.
    left :: Stream a,
    -- | Where the tape is in relation to the starting location.
    cursor :: Integer
  }

instance (Show a) => Show (Tape a) where
  show (Tape r l c) = "Tape (" ++ show r ++ ") (" ++ show l ++ ") " ++ show c

instance Functor Tape where
  fmap f (Tape r l i) = Tape (fmap f r) (fmap f l) i

instance Peekable Tape where
  peek (Tape (Stream a _) _ _) = return a
  swapFirst a (Tape (Stream _ as) bs i) = Tape (Stream a as) bs i

-- | Constructs a `Tape` that is entirely the given value.
blankTape :: a -> Tape a
blankTape a = fmap (const a) (Tape nullStream nullStream 0)

-- | Constructs a `Tape` where the right side of the tape is prepended with the given
-- list, and the rest of both streams are the given value.
fromListTape :: a -> [a] -> Tape a
fromListTape a as = Tape (fromListStream a as) (fromListStream a []) 0

-- | Moves the values within a tape such that when given a `TapeDir`, the values in the
-- `left` and `right` `Stream`s are moved, and the cursor is incremented or decremented.
moveCursor :: TapeDir -> Tape a -> Tape a
moveCursor L Tape {..} = Tape r l (cursor - 1)
  where
    (l, r) = left `moveHeadTo` right
moveCursor R Tape {..} = Tape r l (cursor + 1)
  where
    (r, l) = right `moveHeadTo` left
moveCursor S t = t

-- | Takes an input `Tape` of type @a@, a `Clock`, and a `TuringMachine` with language
-- @a@, and returns the result of running that.
--
-- Check `Data.StateMachines.RunStateMachine.extractResult` and
-- `Data.StateMachines.RunStateMachine.extractErrorAndMachine` to see how to
-- extract values from it.
runTuringMachine :: (Ord a) => Tape a -> Clock -> TuringMachine a -> RunTuringMachineResult a
runTuringMachine tape' clk dfa = runSM (getRunTuringMachine tape' clk dfa)

-- | Constructs the `RunTuringMachine` value for a given input, clock, and
-- `TuringMachine`.
getRunTuringMachine :: (Ord a) => Tape a -> Clock -> TuringMachine a -> RunTuringMachine a
getRunTuringMachine tape' clk turingMachine = constructRunningSM tape' clk turingMachine modifyTape' stepFunc haltingFunc
  where
    modifyTape' (a, tapeDir) t = moveCursor tapeDir (swapFirst a t)
    stepFunc (Identity s) l RunSM {..} = do
      (s', e') <- runStep stateMachine s l
      return (s', fromJustDef (l, S) e')
    haltingFunc (Identity s) _ _ StateMachine {..}
      | s `S.member` acceptStateIDs = Term True
      | s >= 0 = Running
      | otherwise = Term False
