module RunStateMachine
  ( Clock,
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
    getTime,
  )
where

import Lib (Error, Peekable (..))
import StateMachine (StateID, StateLike (fromStateID), StateMachine (startStateID))

data Clock
  = Countdown {time :: Integer, limit :: Integer}
  | Infinite {count :: Integer}
  deriving (Show, Ord, Eq)

tickClock :: Clock -> Clock
tickClock (Countdown i j) = Countdown (i -1) j
tickClock (Infinite i) = Infinite (i + 1)

clock :: Integer -> Clock
clock i
  | i > 0 = Countdown i i
  | otherwise = Infinite 0

getTime :: Clock -> Integer
getTime (Countdown i j) = j - i
getTime (Infinite i) = i

returnValueCheckClock :: ReturnValue -> Clock -> Error ReturnValue
returnValueCheckClock r@(Term _) _ = return r
returnValueCheckClock r (Infinite _) = return r
returnValueCheckClock r (Countdown i _)
  | i <= 0 = Left "Ran out of time"
  | otherwise = return r

data ReturnValue = Running | Term Bool deriving (Eq, Show)

isTerm :: ReturnValue -> Bool
isTerm (Term _) = True
isTerm _ = False

type StepFunction f l s e = s StateID -> l -> RunningSM f l s e -> Error (s StateID, e)

type HaltingFunction f l s e = s StateID -> e -> f l -> StateMachine l s e -> ReturnValue

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

updateTape :: Peekable f => f l -> RunningSM f l s e -> RunningSM f l s e
updateTape v RunSM {..} =
  RunSM v currentState returnValue remainingIter stateMachine modifyTape step halting

updateCurrentState :: StateLike s => s StateID -> RunningSM f l s e -> RunningSM f l s e
updateCurrentState v RunSM {..} =
  RunSM tape v returnValue remainingIter stateMachine modifyTape step halting

updateReturnValue :: ReturnValue -> RunningSM f l s e -> RunningSM f l s e
updateReturnValue v RunSM {..} =
  RunSM tape currentState v remainingIter stateMachine modifyTape step halting

updateRemainingIter :: Clock -> RunningSM f l s e -> RunningSM f l s e
updateRemainingIter v RunSM {..} =
  RunSM tape currentState returnValue v stateMachine modifyTape step halting

updateStateMachine :: StateMachine l s e -> RunningSM f l s e -> RunningSM f l s e
updateStateMachine v RunSM {..} =
  RunSM tape currentState returnValue remainingIter v modifyTape step halting

updateModifyTape :: (e -> f l -> f l) -> RunningSM f l s e -> RunningSM f l s e
updateModifyTape v RunSM {..} =
  RunSM tape currentState returnValue remainingIter stateMachine v step halting

updateStep :: StepFunction f l s e -> RunningSM f l s e -> RunningSM f l s e
updateStep v RunSM {..} =
  RunSM tape currentState returnValue remainingIter stateMachine modifyTape v halting

updateHalting :: HaltingFunction f l s e -> RunningSM f l s e -> RunningSM f l s e
updateHalting v RunSM {..} =
  RunSM tape currentState returnValue remainingIter stateMachine modifyTape step v

constructRunningSM ::
  StateLike s =>
  f l ->
  Clock ->
  StateMachine l s e ->
  (e -> f l -> f l) ->
  StepFunction f l s e ->
  HaltingFunction f l s e ->
  Error (RunningSM f l s e)
constructRunningSM tape' iter sm' modifyTape' step' halting' =
  return $
    RunSM
      tape'
      (fromStateID (startStateID sm'))
      Running
      iter
      sm'
      modifyTape'
      step'
      halting'

runSM ::
  (Peekable f, StateLike s) =>
  RunningSM f l s e ->
  Either (String, RunningSM f l s e) (RunningSM f l s e)
runSM rsm@RunSM {..} = do
  t <- leftWrap $ peek tape
  (s, e) <- leftWrap $ step currentState t rsm
  let tape' = modifyTape e tape
      remainingIter' = tickClock remainingIter
  returnValue' <-
    leftWrap $
      returnValueCheckClock (halting s e tape' stateMachine) remainingIter'
  let rsm' =
        updateReturnValue returnValue' $updateRemainingIter remainingIter' $
          updateCurrentState s $ updateTape tape' rsm
  if isTerm returnValue' then return rsm' else runSM rsm'
  where
    leftWrap (Left s) = Left (s, rsm)
    leftWrap (Right r) = return r

type RunSMResult f l s e = Error (Either (String, RunningSM f l s e) (RunningSM f l s e))

extractResult :: RunSMResult f l s e -> Error ReturnValue
extractResult (Left s) = Left s
extractResult (Right (Left (s, _))) = Left s
extractResult (Right (Right rsm)) = return $ returnValue rsm

extractErrorAndMachine :: RunSMResult f l s e -> Error (String, RunningSM f l s e)
extractErrorAndMachine (Left s) = Left s
extractErrorAndMachine (Right (Left r)) = return r
extractErrorAndMachine (Right (Right r)) = return ("successful run", r)
