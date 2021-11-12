-- |
-- Module      :  Data.StateMachines.RunStateMachine
-- License     :  BSD3
--
-- Maintainer  :  L0neGamer
-- Stability   :  experimental
--
-- Functions and data types for running state machines of all types.
module Data.StateMachines.RunStateMachine
  ( Peekable (..),
    Clock (time),
    Bound (..),
    clock,
    ReturnValue (Running, Term),
    extractResult,
    extractErrorAndMachine,
    HaltingFunction,
    RunSMResult,
    RunningSM (..),
    constructRunningSM,
    runSM,
    updateTape,
    updateCurrentState,
    updateReturnValue,
    updateRemainingIter,
    updateStateMachine,
    updateModifyTape,
    updateHalting,
  )
where

import Control.Monad.State
import Data.Either (fromRight, isLeft)
import Data.Functor
import Data.StateMachines.Internal (Error)
import Data.StateMachines.StateMachine (StateID, StateLike (fromSingle), StateMachine (..))

-- | A data type that stores a timer ticking upwards, either unbounded or
-- bounded.
data Clock = Clock {time :: Integer, bound :: Bound}
  deriving (Show, Ord, Eq)

-- | Represents some upper (integer) limit, or the absence of such a limit.
data Bound = Bound Integer | Unbounded deriving (Show, Ord, Eq)

-- | Increments the `time` in the given `Clock`.
tickClock :: Clock -> Clock
tickClock c = c {time = time c + 1}

-- | Constructs a clock from the given integer, setting a `Bound`ed clock with
-- limit @i@ if @i@ is greater than 0, and setting an `Unbounded` clock otherwise, while
-- initialising both to 0 time.
clock :: Integer -> Clock
clock i
  | i > 0 = Clock 0 (Bound i)
  | otherwise = Clock 0 Unbounded

-- | Represents either that a machine is running or that it has terminated.
data ReturnValue = Running | Term Bool deriving (Eq, Show)

-- | Says whether a given value is a terminal value or not.
isTerm :: ReturnValue -> Bool
isTerm (Term _) = True
isTerm _ = False

-- | Either errors if the clock has run out of time or returns the input `ReturnValue`.
returnValueCheckClock :: ReturnValue -> Clock -> Error ReturnValue
returnValueCheckClock r (Clock i (Bound j))
  | i >= j = Left "Ran out of time"
  | otherwise = return r
returnValueCheckClock r _ = return r

-- | A type class which gives functions for viewing and setting the first value.
--
-- @peek (swapFirst a as) == peek as >> return a@
class Peekable a where
  -- | Gets the first item in the type, and errors otherwise.
  peek :: a b -> Error b

  -- | Swaps the first item in the type for the given value.
  swapFirst :: b -> a b -> a b

instance Peekable [] where
  peek [] = Left "Empty List when peeking"
  peek (x : _) = return x
  swapFirst _ [] = []
  swapFirst a (_ : as) = a : as

-- | A type alias for a function that determines whether the machine
-- should halt.
type HaltingFunction f l s e = s StateID -> Maybe e -> f l -> StateMachine l s e -> ReturnValue

-- | The data type that contains a running state machine, as well as all
-- the state required to continue execution of the state machine.
-- - @f@ is the container that is read from throughout the execution
-- - @l@ is the type of the language used
-- - @s@ is the type of the `StateLike` container used
-- - @e@ is the type of the additional outputs
data RunningSM f l s e = RunSM
  { tape :: Peekable f => f l,
    currentState :: !(StateLike s => s StateID),
    returnValue :: ReturnValue,
    remainingIter :: Clock,
    stateMachine :: !(StateMachine l s e),
    modifyTape :: Maybe e -> f l -> f l,
    halting :: HaltingFunction f l s e
  }

instance
  (Show (f l), Show l, Show (s StateID), Show e, StateLike s, Semigroup e, Peekable f) =>
  Show
    (RunningSM f l s e)
  where
  show rsm =
    "RunSM "
      ++ contained tape
      ++ contained currentState
      ++ contained returnValue
      ++ contained remainingIter
      ++ contained stateMachine
    where
      contained f = "(" ++ show (f rsm) ++ ") "

-- | Overwrites the `tape` in a given `RunningSM`.
updateTape :: Peekable f => f l -> RunningSM f l s e -> RunningSM f l s e
updateTape v rsm = rsm {tape = v}

-- | Overwrites the `currentState` in a given `RunningSM`.
updateCurrentState :: StateLike s => s StateID -> RunningSM f l s e -> RunningSM f l s e
updateCurrentState v rsm = rsm {currentState = v}

-- | Overwrites the `returnValue` in a given `RunningSM`.
updateReturnValue :: ReturnValue -> RunningSM f l s e -> RunningSM f l s e
updateReturnValue v rsm = rsm {returnValue = v}

-- | Overwrites the `remainingIter` in a given `RunningSM`.
updateRemainingIter :: Clock -> RunningSM f l s e -> RunningSM f l s e
updateRemainingIter v rsm = rsm {remainingIter = v}

-- | Overwrites the `stateMachine` in a given `RunningSM`.
updateStateMachine :: StateMachine l s e -> RunningSM f l s e -> RunningSM f l s e
updateStateMachine v rsm = rsm {stateMachine = v}

-- | Overwrites the `modifyTape` function in a given `RunningSM`.
updateModifyTape :: (Maybe e -> f l -> f l) -> RunningSM f l s e -> RunningSM f l s e
updateModifyTape v rsm = rsm {modifyTape = v}

-- | Overwrites the `halting` function in a given `RunningSM`.
updateHalting :: HaltingFunction f l s e -> RunningSM f l s e -> RunningSM f l s e
updateHalting v rsm = rsm {halting = v}

checkMachineHalting :: RunningSM f l s e -> Error ReturnValue
checkMachineHalting RunSM {..} = returnValueCheckClock returnValue remainingIter

-- | Constructs a `RunningSM` from:
-- - `Peekable` data structure @f@ containing values of type @l@
-- - a `Clock`
-- - a `StateMachine` (that operates on the language @l@, uses the `StateLike` @s@ type,
-- and puts out an output @e@ on a step),
-- - a `StepFunction`
-- - a `HaltingFunction`.
constructRunningSM ::
  (StateLike s, Peekable f) =>
  f l ->
  Clock ->
  StateMachine l s e ->
  (Maybe e -> f l -> f l) ->
  HaltingFunction f l s e ->
  RunningSM f l s e
constructRunningSM tape' iter sm =
  RunSM
    tape'
    (fromSingle (startStateID sm))
    Running
    iter
    sm

-- | A type alias to more concisely work with the result of running a state machine. The alias is for @Either (String, `RunningSM` f l s e) (`RunningSM` f l s e)@.
type RunSMResult f l s e = Either (String, RunningSM f l s e) (RunningSM f l s e)

runSM' :: (Peekable f, StateLike s) => State (RunningSM f l s e) (Error ReturnValue)
runSM' = do
  sm <- gets stateMachine
  t <- gets tape <&> peek
  if isLeft t
    then return $ toLeft t
    else do
      currentState' <- gets currentState
      let t' = unsafeFromRight t
          se = step sm currentState' t' sm
      if isLeft se
        then return $ toLeft se
        else do
          let (s, e) = unsafeFromRight se
          modify $ \rsm -> updateTape (modifyTape rsm e (tape rsm)) rsm
          modify $ \rsm -> updateRemainingIter (tickClock (remainingIter rsm)) rsm
          rv <- do
            halting' <- gets halting
            tape' <- gets tape
            remainingIter' <- gets remainingIter
            return $ returnValueCheckClock (halting' s e tape' sm) remainingIter'
          if isLeft rv
            then return $ toLeft rv
            else do
              let rv' = unsafeFromRight rv
              modify $ updateReturnValue rv'
              if isTerm rv'
                then return $ return rv'
                else runSM'
  where
    unsafeFromRight (Right b) = b
    unsafeFromRight _ = error "using unsafeFromRight unsafely!"
    toLeft (Left a) = Left a
    func (Left a) _ = return $ Left a
    func (Right a) f = f a

-- | Runs a given `RunningSM` to completion or error.
runSM ::
  (Peekable f, StateLike s) =>
  RunningSM f l s e ->
  RunSMResult f l s e
runSM rsm@RunSM {stateMachine = sm@StateMachine {..}, ..} = do
  -- get the value to act on
  t <- leftWrap $ peek tape
  -- perform a step
  (s, e) <- leftWrap $ step currentState t sm
  -- based on the values retrieved, modify the tape and the clock
  let tape' = modifyTape e tape
      remainingIter' = tickClock remainingIter
  -- check if the clock has overrun, or if the machine is in a halting state
  returnValue' <-
    leftWrap $
      returnValueCheckClock (halting s e tape' sm) remainingIter'
  -- update the running state machine with the new tape, current state, clock, and return
  -- value
  let rsm' =
        updateReturnValue returnValue' $
          updateRemainingIter remainingIter' $
            updateCurrentState s $
              updateTape tape' rsm
  if isTerm returnValue' then return rsm' else runSM rsm'
  where
    leftWrap (Left s) = Left (s, rsm)
    leftWrap (Right r) = return r

-- | Takes a `RunSMResult` and returns either its error or the `ReturnValue`
-- in the successful run.
extractResult :: RunSMResult f l s e -> Error ReturnValue
extractResult (Left (s, _)) = Left s
extractResult (Right rsm) = return $ returnValue rsm

-- | Takes a `RunSMResult` and returns a message and the final state of the `RunningSM`.
extractErrorAndMachine :: RunSMResult f l s e -> (String, RunningSM f l s e)
extractErrorAndMachine (Left s) = s
extractErrorAndMachine (Right r) = ("Success", r)
