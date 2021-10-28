module TuringMachine where

import Data.Foldable (Foldable (toList))
import Data.Set as S (member)
import Lib (Error, Peekable (..), Single (..), fromJust)
import RunStateMachine (Clock, ReturnValue (Running, Term), RunningSM (..), constructRunningSM, runSM)
import StateMachine (StateMachine (StateMachine, acceptStateIDs), Transition, runStep)

data TapeDir
  = L
  | S
  | R
  deriving (Show, Eq)

type TuringMachine a = StateMachine a Single (a, TapeDir)

type TuringMachineTransition a = Transition a (a, TapeDir)

type RunTuringMachine a = RunningSM Tape a Single (a, TapeDir)

data Stream a = Stream a (Stream a)

instance Functor Stream where
  fmap f (Stream a as) = Stream (f a) (fmap f as)

instance Foldable Stream where
  foldr f b (Stream a as) = a `f` foldr f b as

instance (Show a) => Show (Stream a) where
  show s = showHelp s
    where
      showStreamLimit = 15
      showHelp = show . take showStreamLimit . toList

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
  peek (Tape (Stream a _) _ _) = Right a
  swapFirst a (Tape (Stream _ as) bs i) = Tape (Stream a as) bs i

nullStream :: Stream ()
nullStream = Stream () nullStream

startingTape :: a -> Tape a
startingTape a = fmap (const a) (Tape nullStream nullStream 0)

moveHeadTo :: Stream a -> Stream a -> (Stream a, Stream a)
moveHeadTo (Stream a as) bs = (as, Stream a bs)

moveCursor :: TapeDir -> Tape a -> Tape a
moveCursor L Tape {..} = Tape r l (cursor - 1)
  where
    (l, r) = left `moveHeadTo` right
moveCursor R Tape {..} = Tape r l (cursor + 1)
  where
    (r, l) = right `moveHeadTo` left
moveCursor S t = t

runTuringMachine :: (Ord a) => Tape a -> Clock -> TuringMachine a -> Error (Either (String, RunTuringMachine a) (RunTuringMachine a))
runTuringMachine tape' clk dfa = do
  rtm <- getRunTuringMachine tape' clk dfa
  Right $ runSM rtm

getRunTuringMachine :: (Ord a) => Tape a -> Clock -> TuringMachine a -> Error (RunTuringMachine a)
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
