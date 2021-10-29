-- |
-- Module      :  Data.StateMachines.StateMachine
-- License     :  BSD3
--
-- Maintainer  :  L0neGamer
-- Stability   :  experimental
--
-- Functions and data types for creating state machines of all types.
module Data.StateMachines.StateMachine
  ( StateID,
    State (..),
    StateLike (..),
    Transition (..),
    StateMachine (..),
    constructStateMachine,
    inferStateMachine,
    runStep,
    updateName,
    updateLanguage,
    updateTransitions,
    updateStartStateID,
    updateAcceptStateIDs,
    updateAddOutput,
    updateNamesToNumbers,
    tupleToSimpleTransition,
    addTransition,
  )
where

import Control.Monad (when)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Either.Extra (maybeToEither)
import Data.Functor.Identity (Identity (Identity))
import Data.Map as M
  ( Map,
    alter,
    empty,
    findWithDefault,
    fromList,
    insert,
    lookup,
    member,
    toList,
    (!),
  )
import Data.Set as S
  ( Set,
    delete,
    empty,
    fromList,
    insert,
    notMember,
    singleton,
    size,
    toAscList,
    toList,
    union,
  )
import Data.StateMachines.Internal
  ( Error,
    lookupEither',
    updateVector,
  )
import Data.Vector as V (Vector, replicate, (!?), (//))

-- TODO: the below
--   removeTransition :: (Ord a) => Transition a -> sm a -> sm a
--   reachableStates :: (Ord a) => sm a -> State -> States
--   isAcceptable :: (Ord a) => sm a -> State -> Bool
--   isAcceptable sm s = or $ S.map (`S.member` smAcceptStates sm) (reachableStates sm s)

-- | A type alias for Int to make it clearer what is being referred to.
type StateID = Int

-- | A data type that makes it easier to initially construct a state machine.
data State
  = Dead
  | State String
  deriving (Show, Eq, Ord)

-- | A type class that adds some utility functions to an @s@ for use in computation.
class StateLike s where
  -- | Construct a `StateLike` data type from a `StateID`.
  fromStateID :: StateID -> s StateID

  -- | How to combine a new `StateID` with an existing `StateLike` data type.
  combineStates :: StateID -> s StateID -> s StateID

  -- | How to combine two `StateLike` data types (if they are the same).
  combineStateLike :: s StateID -> s StateID -> s StateID

  -- | How to turn a `StateLike` data type into a `Set`.
  toSet :: s a -> Set a

instance StateLike Set where
  fromStateID = singleton
  combineStates s ss = S.insert s ss
  combineStateLike = S.union
  toSet = id

instance StateLike Identity where
  fromStateID = Identity
  combineStates s _ = Identity s
  combineStateLike = const
  toSet (Identity s) = S.singleton s

-- | @StateMachine@ is the data type that contains a state machine as well as some
-- information about it
-- - @l@ is the type of the language used
-- - @s@ is the type of the `StateLike` container used
-- - @e@ is the type of the additional outputs
data StateMachine l s e = StateMachine
  { name :: !String,
    language :: !(Set l),
    transitions :: !(StateLike s => Vector (Map l (s StateID, e))),
    startStateID :: !StateID,
    acceptStateIDs :: !(Set StateID),
    addOutput :: e -> e -> e,
    namesToNumbers :: !(Map State StateID)
  }

-- | Overwrites the `name` in a given `StateMachine`.
updateName :: String -> StateMachine l s e -> StateMachine l s e
updateName v sm = sm {name = v}

-- | Overwrites the `language` in a given `StateMachine`.
updateLanguage :: Set l -> StateMachine l s e -> StateMachine l s e
updateLanguage v sm = sm {language = v}

-- | Overwrites the `transitions` in a given `StateMachine`.
updateTransitions :: (StateLike s) => Vector (Map l (s StateID, e)) -> StateMachine l s e -> StateMachine l s e
updateTransitions v sm = sm {transitions = v}

-- | Overwrites the `startStateID` in a given `StateMachine`.
updateStartStateID :: StateID -> StateMachine l s e -> StateMachine l s e
updateStartStateID v sm = sm {startStateID = v}

-- | Overwrites the `AcceptStateIDs` in a given `StateMachine`.
updateAcceptStateIDs :: Set StateID -> StateMachine l s e -> StateMachine l s e
updateAcceptStateIDs v sm = sm {acceptStateIDs = v}

-- | Overwrites the `AddOutput` in a given `StateMachine`.
updateAddOutput :: (e -> e -> e) -> StateMachine l s e -> StateMachine l s e
updateAddOutput v sm = sm {addOutput = v}

-- | Overwrites the `namesToNumbers` in a given `StateMachine`.
updateNamesToNumbers :: Map State StateID -> StateMachine l s e -> StateMachine l s e
updateNamesToNumbers v sm = sm {namesToNumbers = v}

instance (Show l, Show (s StateID), Show e, StateLike s) => Show (StateMachine l s e) where
  show sm =
    "StateMachine "
      ++ contained name
      ++ contained language
      ++ contained transitions
      ++ contained startStateID
      ++ contained acceptStateIDs
      ++ contained namesToNumbers
    where
      contained f = "(" ++ show (f sm) ++ ") "

-- | A data type for holding information about a single transition between two states.
data Transition a b = Transition
  { startStateT :: State,
    endStateT :: State,
    characterT :: a,
    outputT :: b
  }
  deriving (Show, Eq)

-- | Takes a tuple that has two states and an @a@, and returns the equivalent
-- `Transition`.
tupleToSimpleTransition :: (State, State, a) -> Transition a ()
tupleToSimpleTransition (s, s', a) = Transition s s' a ()

-- | Returns a set of the states and a set of the language characters used by a list of
-- transitions.
getStatesAndLang :: (Ord a) => [Transition a b] -> (Set State, Set a)
getStatesAndLang = foldr combine (S.empty, S.empty)
  where
    combine (Transition s s' a _) (ss, as) = (S.insert s (S.insert s' ss), S.insert a as)

-- | Runs a step of the given `StateMachine` when a particular state and character to step
-- on are given. Useful for creating the more complex step functions needed in
-- `Data.StateMachines.RunStateMachine.RunningSM` data types.
runStep :: (Ord l, StateLike s) => StateMachine l s e -> StateID -> l -> Error (s StateID, Maybe e)
runStep sm sid l
  | sid >= 0 = do
    m <- maybeToEither "Could not find state (runStep)" (t !? sid)
    interpretNothing $ M.lookup l m
  | otherwise = interpretNothing Nothing
  where
    t = transitions sm
    interpretNothing Nothing = return (fromStateID (-1), Nothing)
    interpretNothing (Just (s, e)) = return (s, Just e)

-- | Adds a single `Transition` to a given `StateMachine`. Recommended to use
-- `addTransitions` for bulk additions as this uses the slower `updateVector`.
addTransition :: (StateLike s, Ord l) => Transition l e -> StateMachine l s e -> Error (StateMachine l s e)
addTransition t sm@StateMachine {..} = do
  (ss, (l, se)) <- addTransitions' t sm
  m <- maybeToEither "Could not find start state (addTransition)" $ transitions !? ss
  ts <- updateVector ss (M.insert l se m) transitions
  return $ updateTransitions ts sm

-- | A helper function for `addTransitions` that turns a single `Transition` into a nested
-- tuple for use in `addTransitions`.
addTransitions' :: (StateLike s, Ord l) => Transition l e -> StateMachine l s e -> Error (StateID, (l, (s StateID, e)))
addTransitions' Transition {..} StateMachine {..} = do
  ss <- lookupEither' ("Could not locate startStateT " ++ show startStateT ++ " (addTransitions')") startStateT namesToNumbers
  es <- lookupEither' ("Could not locate endStateT " ++ show endStateT ++ " (addTransitions')") endStateT namesToNumbers
  let l = characterT
      e = outputT
  when (l `S.notMember` language) $ Left "Character not in language"
  m <- maybeToEither "Could not find start state (addTransition)" $ transitions !? ss
  let combined
        | l `M.member` m = bimap (combineStates es) (addOutput e) (m M.! l)
        | otherwise = (fromStateID es, e)
  return (ss, (l, combined))

--TODO: foldrM and uses maps and stuff instead of doing all the accesses above and then redoing them again and again (type: Transition l e -> Map StateID (Map l (s StateID,e)) -> Error (Map StateID (Map l (s StateID,e))) )
-- foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b

-- | Takes a list of `Transition`s and a `StateMachine`, and returns a `StateMachine`
-- updated with those transitions.
addTransitions :: (StateLike s, Ord l) => [Transition l e] -> StateMachine l s e -> Error (StateMachine l s e)
addTransitions ts sm@StateMachine {..} = do
  tups <- mapM (`addTransitions'` sm) ts
  let v = transitions V.// M.toList (foldr hFunc M.empty tups)
  return $ updateTransitions v sm
  where
    hFunc (sid, (l, (s, e))) m = M.insert sid (M.alter hFunc' l (findWithDefault M.empty sid m)) m
      where
        hFunc' (Just (s', e')) = Just (combineStateLike s s', addOutput e e')
        hFunc' Nothing = Just (s, e)

-- | @constructStateMachine@ takes the following values and returns either an error from
-- the construction of the machine or return the state machine itself.
-- - the machine name
-- - the language
-- - all the states in the machine
-- - all the transitions between states, using the language and producing side effects
-- of type e
-- - the start state
-- - the accept states
-- - a way to combine side effects and will
constructStateMachine :: (Ord l, StateLike s) => String -> Set l -> Set State -> [Transition l e] -> State -> Set State -> (e -> e -> e) -> Error (StateMachine l s e)
constructStateMachine name' language' states' transitions' startState' acceptStates' addOutput' = do
  acceptStatesList <- mapM (\s -> lookupEither' ("Could not find accept state " ++ show s) s namesToNumbers') (S.toList (S.delete Dead acceptStates'))
  startStateID' <- lookupEither' ("Could not find start state " ++ show startState') startState' namesToNumbers'
  let smASI = updateAcceptStateIDs (S.fromList acceptStatesList) sm
      smSSID = updateStartStateID startStateID' smASI
  addTransitions transitions' smSSID
  where
    states'' = S.delete Dead states'
    namesToNumbers' = M.insert Dead (-1) $ M.fromList $ zip (S.toAscList states'') [0 ..]
    sm = StateMachine name' language' (V.replicate (size states'') M.empty) (-2) S.empty addOutput' namesToNumbers'

inferStateMachine' :: (Ord l, StateLike s) => (String -> Set l -> Set State -> [Transition l e] -> State -> Set State -> (e -> e -> e) -> Error (StateMachine l s e)) -> String -> [Transition l e] -> State -> Set State -> (e -> e -> e) -> Error (StateMachine l s e)
inferStateMachine' conSM name' transitions' startState acceptStates addOutput' = conSM name' language' states' transitions' startState acceptStates addOutput'
  where
    (states', language') = getStatesAndLang transitions'

-- | Infers a `StateMachine` from the transitions, using `getStatesAndLang`. Gets all the
-- states from the transitions, and gets the language from the transitions too.
inferStateMachine :: (Ord l, StateLike s) => String -> [Transition l e] -> State -> Set State -> (e -> e -> e) -> Error (StateMachine l s e)
inferStateMachine = inferStateMachine' constructStateMachine
