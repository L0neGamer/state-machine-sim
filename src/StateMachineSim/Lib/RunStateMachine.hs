module StateMachineSim.Lib.RunStateMachine
  ( Clock (time),
    ReturnValue (Running, Term),
    RunSMResult,
    RunningSM (..),
    updateTape,
    updateCurrentState,
    updateReturnValue,
    updateRemainingIter,
    updateStateMachine,
    updateModifyTape,
    updateStep,
    updateHalting,
    constructRunningSM,
    runSM,
    clock,
    extractResult,
    extractErrorAndMachine,
  )
where

import StateMachineSim.Lib.Lib (Error, Peekable (..))
import StateMachineSim.Lib.StateMachine(StateID, StateLike (fromStateID), StateMachine (startStateID))

-- | @Clock@ is a data type that stores a timer ticking upwards, either unbounded or
-- bounded
data Clock = Clock {time :: Integer, bound :: Bound}
  deriving (Show, Ord, Eq)

-- | @Bound@ represents some upper (integer) limit, or the absence of such a limit
data Bound = Bound Integer | Unbounded deriving (Show, Ord, Eq)

-- | @tickClock@ increments the @time@ in the given @Clock@
tickClock :: Clock -> Clock
tickClock c = c {time = time c + 1}

-- | @clock@ constructs a clock from the given integer, setting a @Bound@ed clock with
-- limit @i@ if @i@ is greater than 0, and setting an @Unbounded@ clock otherwise, while
-- initialising both to 0 time
clock :: Integer -> Clock
clock i
  | i > 0 = Clock 0 (Bound i)
  | otherwise = Clock 0 Unbounded

-- | @returnValueCheckClock@ either errors if the clock has run out of time or returns the
-- input @ReturnValue@
returnValueCheckClock :: ReturnValue -> Clock -> Error ReturnValue
returnValueCheckClock r (Clock i (Bound j))
  | i >= j = Left "Ran out of time"
  | otherwise = return r
returnValueCheckClock r _ = return r

-- | @ReturnValue@ represents either that a machine is running or that it has terminated
data ReturnValue = Running | Term Bool deriving (Eq, Show)

-- | @isTerm@ says whether a given value is a terminal value or not
isTerm :: ReturnValue -> Bool
isTerm (Term _) = True
isTerm _ = False

-- | @StepFunction@ is a type alias for a function that gets the next states and extra
-- output
type StepFunction f l s e = s StateID -> l -> RunningSM f l s e -> Error (s StateID, e)

-- | @HaltingFunction@ is a type alias for a function that determines whether the machine
-- should halt
type HaltingFunction f l s e = s StateID -> e -> f l -> StateMachine l s e -> ReturnValue

-- | @RunningSM@ is the data type that contains a running state machine, as well as all
-- the state required to continue execution of the state machine.
-- @f@ is the container that is read from throughout the execution
-- @l@ is the type of the language used
-- @s@ is the type of the @StateLike@ container used
-- @e@ is the type of the additional outputs
data RunningSM f l s e = RunSM
  { tape :: Peekable f => f l,
    currentState :: !(StateLike s => s StateID),
    returnValue :: ReturnValue,
    remainingIter :: Clock,
    stateMachine :: !(StateMachine l s e),
    modifyTape :: e -> f l -> f l,
    step :: StepFunction f l s e,
    halting :: HaltingFunction f l s e
  }

instance
  (Show (f l), Show l, Show (s StateID), Show e, StateLike s, Peekable f) =>
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

-- | @updateTape@ overwrites the @tape@ in a given @RunningSM@
updateTape :: Peekable f => f l -> RunningSM f l s e -> RunningSM f l s e
updateTape v rsm = rsm {tape = v}

-- | @updateCurrentState@ overwrites the @currentState@ in a given @RunningSM@
updateCurrentState :: StateLike s => s StateID -> RunningSM f l s e -> RunningSM f l s e
updateCurrentState v rsm = rsm {currentState = v}

-- | @updateReturnValue@ overwrites the @returnValue@ in a given @RunningSM@
updateReturnValue :: ReturnValue -> RunningSM f l s e -> RunningSM f l s e
updateReturnValue v rsm = rsm {returnValue = v}

-- | @updateRemainingIter@ overwrites the @remainingIter@ in a given @RunningSM@
updateRemainingIter :: Clock -> RunningSM f l s e -> RunningSM f l s e
updateRemainingIter v rsm = rsm {remainingIter = v}

-- | @updateStateMachine@ overwrites the @stateMachine@ in a given @RunningSM@
updateStateMachine :: StateMachine l s e -> RunningSM f l s e -> RunningSM f l s e
updateStateMachine v rsm = rsm {stateMachine = v}

-- | @updateModifyTape@ overwrites the @modifyTape@ function in a given @RunningSM@
updateModifyTape :: (e -> f l -> f l) -> RunningSM f l s e -> RunningSM f l s e
updateModifyTape v rsm = rsm {modifyTape = v}

-- | @updateStep@ overwrites the @step@ function in a given @RunningSM@
updateStep :: StepFunction f l s e -> RunningSM f l s e -> RunningSM f l s e
updateStep v rsm = rsm {step = v}

-- | @updateHalting@ overwrites the @halting@ function in a given @RunningSM@
updateHalting :: HaltingFunction f l s e -> RunningSM f l s e -> RunningSM f l s e
updateHalting v rsm = rsm {halting = v}

-- | @constructRunningSM@ constructs a @RunningSM@ from a @Peekable@ data structure @f@
-- containing values of type @l@, a @Clock@, a @StateMachine@ (that operates on the
-- language @l@, uses the @StateLike@ @s@ type, and puts out an output @e@ on a step), a
-- @StepFunction@, and a @HaltingFunction@
constructRunningSM ::
  (StateLike s, Peekable f) =>
  f l ->
  Clock ->
  StateMachine l s e ->
  (e -> f l -> f l) ->
  StepFunction f l s e ->
  HaltingFunction f l s e ->
  RunningSM f l s e
constructRunningSM tape' iter sm =
  RunSM
    tape'
    (fromStateID (startStateID sm))
    Running
    iter
    sm

-- | @RunSMResult@ is a type alias to more concisely work with the result of running a
-- state machine
type RunSMResult f l s e = Either (String, RunningSM f l s e) (RunningSM f l s e)

-- | @runSM@ runs a given @RunningSM@ to completion or error
runSM ::
  (Peekable f, StateLike s) =>
  RunningSM f l s e ->
  RunSMResult f l s e
runSM rsm@RunSM {..} = do
  t <- leftWrap $ peek tape
  (s, e) <- leftWrap $ step currentState t rsm
  let tape' = modifyTape e tape
      remainingIter' = tickClock remainingIter
  returnValue' <-
    leftWrap $
      returnValueCheckClock (halting s e tape' stateMachine) remainingIter'
  let rsm' =
        updateReturnValue returnValue' $
          updateRemainingIter remainingIter' $
            updateCurrentState s $
              updateTape tape' rsm
  if isTerm returnValue' then return rsm' else runSM rsm'
  where
    leftWrap (Left s) = Left (s, rsm)
    leftWrap (Right r) = return r

-- @extractResult@ takes a @RunSMResult@ and returns either its error or the @ReturnValue@
-- in the successful run
extractResult :: RunSMResult f l s e -> Error ReturnValue
extractResult (Left (s, _)) = Left s
extractResult (Right rsm) = return $ returnValue rsm

-- @extractErrorAndMachine@ takes a @RunSMResult@ and returns a message and the final
-- state of the @RunningSM@
extractErrorAndMachine :: RunSMResult f l s e -> (String, RunningSM f l s e)
extractErrorAndMachine (Left s) = s
extractErrorAndMachine (Right r) = ("Success", r)
