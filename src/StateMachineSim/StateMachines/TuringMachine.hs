module StateMachineSim.StateMachines.TuringMachine
  ( TuringMachine,
    TuringMachineTransition,
    RunTuringMachine,
    RunTuringMachineResult,
    TapeDir (L, R),
    Tape (..),
    Stream (..),
    runTuringMachine,
    blankTape,
    fromListStream,
    fromListTape,
  )
where

import Data.Foldable (Foldable (toList))
import Data.List (genericTake)
import Data.Set as S (member)
import StateMachineSim.Lib.Lib (Peekable (..), Single (..), fromJust)
import StateMachineSim.Lib.RunStateMachine (Clock, ReturnValue (Running, Term), RunSMResult, RunningSM (..), constructRunningSM, runSM)
import StateMachineSim.Lib.StateMachine(StateMachine (StateMachine, acceptStateIDs), Transition, runStep)

-- | @TapeDir@ is a data type that determines which direction the tape should move
data TapeDir
  = L
  | S
  | R
  deriving (Show, Eq)

-- | @TuringMachine@ is a type alias that represents the default type for Turing machines
type TuringMachine a = StateMachine a Single (a, TapeDir)

-- | @TuringMachineTransition@ is a type alias that represents the default type for Turing
-- machine transitions
type TuringMachineTransition a = Transition a (a, TapeDir)

-- | @RunTuringMachine@ is a type alias that represents the default type for running
-- Turing machines
type RunTuringMachine a = RunningSM Tape a Single (a, TapeDir)

-- | @RunTuringMachineResult@ is a type alias that represents the default type for the
-- result of running a Turing machine
type RunTuringMachineResult a = RunSMResult Tape a Single (a, TapeDir)

-- | @Stream@ is an infinite data storage type. The @Show@ instance limits the output to
-- 15 values by default
data Stream a = Stream a (Stream a)

instance Functor Stream where
  fmap f (Stream a as) = Stream (f a) (fmap f as)

instance Foldable Stream where
  foldr f b (Stream a as) = a `f` foldr f b as

instance (Show a) => Show (Stream a) where
  show = showStream showStreamLimit
    where
      showStreamLimit = 15 :: Int

-- | @fromListStream@ takes a default value of type @a@, a list of @a@s, and returns a
-- @Stream@ prepended with the values in the list before having an infinite stream of the
-- default @a@s
fromListStream :: a -> [a] -> Stream a
fromListStream a [] = st
  where
    st = Stream a st
fromListStream a (a' : as) = Stream a' (fromListStream a as)

-- | @showStream@ returns a string representing the given @Stream@ up to a given depth
showStream :: (Show a, Integral i) => i -> Stream a -> String
showStream i st@(Stream a _) = "fromListStream (" ++ show infVal ++ ") " ++ show lst
  where
    lst = (genericTake i . toList) st
    infVal
      | i <= 0 = a
      | otherwise = last lst

nullStream :: Stream ()
nullStream = Stream () nullStream

moveHeadTo :: Stream a -> Stream a -> (Stream a, Stream a)
moveHeadTo (Stream a as) bs = (as, Stream a bs)

-- | @Tape@ is the data type that represents an infinite tape going off to the left and
-- right, with a @cursor@ designating how far the tape currently is from the start
-- location
data Tape a = Tape
  { right :: Stream a,
    left :: Stream a,
    cursor :: Integer
  }

instance (Show a) => Show (Tape a) where
  show (Tape r l c) = "Tape " ++ show r ++ " " ++ show l ++ " " ++ show c

instance Functor Tape where
  fmap f (Tape r l i) = Tape (fmap f r) (fmap f l) i

instance Peekable Tape where
  peek (Tape (Stream a _) _ _) = return a
  swapFirst a (Tape (Stream _ as) bs i) = Tape (Stream a as) bs i

-- | @blankTape@ constructs a @Tape@ that is entirely the given value
blankTape :: a -> Tape a
blankTape a = fmap (const a) (Tape nullStream nullStream 0)

-- | @fromListTape@ constructs a @Tape@ where the right side of the tape is prepended with
-- the given list, and the rest of both streams are the given value
fromListTape :: a -> [a] -> Tape a
fromListTape a as = Tape (fromListStream a as) (fromListStream a []) 0

-- | @moveCursor@ moves the values within a tape such that when given a @TapeDir@, the
-- values in the @left@ and @right@ @Stream@s are moved, and the cursor is incremented or
-- decremented
moveCursor :: TapeDir -> Tape a -> Tape a
moveCursor L Tape {..} = Tape r l (cursor - 1)
  where
    (l, r) = left `moveHeadTo` right
moveCursor R Tape {..} = Tape r l (cursor + 1)
  where
    (r, l) = right `moveHeadTo` left
moveCursor S t = t

-- | @runTuringMachine@ takes an input @Tape@ of type @a@, a Clock, and a Turing machine
-- with language @a@, and returns the result of running that.
-- Check @extractResult@ and @extractErrorAndMachine@ from @RunStateMachine@ to see how to
-- extract values from it.
runTuringMachine :: (Ord a) => Tape a -> Clock -> TuringMachine a -> RunTuringMachineResult a
runTuringMachine tape' clk dfa = runSM (getRunTuringMachine tape' clk dfa)

-- | @getRunTuringMachine@ constructs the @RunTuringMachine@ value for a given input,
-- clock, and @TuringMachine@
getRunTuringMachine :: (Ord a) => Tape a -> Clock -> TuringMachine a -> RunTuringMachine a
getRunTuringMachine tape' clk turingMachine = constructRunningSM tape' clk turingMachine modifyTape' stepFunc haltingFunc
  where
    modifyTape' (a, tapeDir) t = moveCursor tapeDir (swapFirst a t)
    stepFunc (Single s) l RunSM {..} = do
      (s', e') <- runStep stateMachine s l
      return (s', fromJust (l, S) e')
    haltingFunc (Single s) _ _ StateMachine {..}
      | s `S.member` acceptStateIDs = Term True
      | s >= 0 = Running
      | otherwise = Term False
