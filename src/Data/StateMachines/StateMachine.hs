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
    stateName',
    StateLike (..),
    Transition (..),
    StateMachine (..),
    constructStateMachine,
    inferStateMachine',
    inferStateMachine,
    ConsSM (..),
    StepFunction,
    step',
    updateName,
    updateLanguage,
    updateTransitions,
    updateStartStateID,
    updateAcceptStateIDs,
    updateStep,
    updateNamesToNumbers,
    tupleToSimpleTransition,
    addTransitions,
    addTransition,
  )
where

import Control.Monad (when)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Either.Combinators (maybeToRight)
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
import Diagrams.Core.Names (IsName)

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
  | State {stateName :: String}
  deriving (Show, Eq, Ord)

-- | Utility function to get the name of the given state, whether it is `Dead` or not.
stateName' :: State -> String
stateName' Dead = "Dead"
stateName' s = stateName s

instance IsName State

-- | A type class that adds some utility functions to an @s@ for use in computation.
class StateLike s where
  -- | Construct a `StateLike` data type from a `StateID`.
  fromSingle :: a -> s a

  -- | How to put a new item into an existing `StateLike` data type. Prefer the new item.
  combineStates :: Ord a => a -> s a -> s a
  combineStates a = combineStateLike (fromSingle a)

  -- | How to combine two `StateLike` data types (if they are the same). Prefer the first
  -- `StateLike`.
  combineStateLike :: Ord a => s a -> s a -> s a

  -- | How to turn a `StateLike` data type into a `Set`.
  toSet :: s a -> Set a

  -- | Check if this item contains only one element.
  isSingle :: s a -> Bool

instance StateLike Set where
  fromSingle = singleton
  combineStateLike = S.union
  toSet = id
  isSingle (S.toList -> [_]) = True
  isSingle _ = False

instance StateLike Identity where
  fromSingle = Identity
  combineStateLike = const
  toSet (Identity s) = S.singleton s
  isSingle _ = True

-- | A type alias for a function that gets the next states and extra
-- output.
type StepFunction l s e = s StateID -> l -> StateMachine l s e -> Error (s StateID, Maybe e)

-- | @StateMachine@ is the data type that contains a state machine as well as some
-- information about it
-- - @l@ is the type of the language used
-- - @s@ is the type of the `StateLike` container used
-- - @e@ is the type of the additional outputs
data StateMachine l s e = StateMachine
  { name :: !String,
    language :: !(Set l),
    transitions :: !((StateLike s, Semigroup e) => Vector (Map l (s StateID, e))),
    startStateID :: !StateID,
    acceptStateIDs :: !(Set StateID),
    step :: StepFunction l s e,
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

-- | Overwrites the `acceptStateIDs` in a given `StateMachine`.
updateAcceptStateIDs :: Set StateID -> StateMachine l s e -> StateMachine l s e
updateAcceptStateIDs v sm = sm {acceptStateIDs = v}

-- | Overwrites the `step` function in a given `StateMachine`.
updateStep :: StepFunction l s e -> StateMachine l s e -> StateMachine l s e
updateStep v sm = sm {step = v}

-- | Overwrites the `namesToNumbers` in a given `StateMachine`.
updateNamesToNumbers :: Map State StateID -> StateMachine l s e -> StateMachine l s e
updateNamesToNumbers v sm = sm {namesToNumbers = v}

instance (Show l, Show (s StateID), Show e, StateLike s, Semigroup e) => Show (StateMachine l s e) where
  show StateMachine {..} =
    "StateMachine "
      ++ show name
      ++ " "
      ++ contained language
      ++ show transitions
      ++ " "
      ++ contained startStateID
      ++ contained acceptStateIDs
      ++ show namesToNumbers
    where
      contained f = "(" ++ show f ++ ") "

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
step' :: (Ord l, StateLike s, Semigroup e) => StateID -> l -> StateMachine l s e -> Error (s StateID, Maybe e)
step' sid l sm
  | sid >= 0 = do
    m <- maybeToRight ("Could not find state (step): " ++ show sid) (t !? sid)
    interpretNothing $ M.lookup l m
  | otherwise = interpretNothing Nothing
  where
    t = transitions sm
    interpretNothing Nothing = return (fromSingle (namesToNumbers sm M.! Dead), Nothing)
    interpretNothing (Just (s, e)) = return (s, Just e)

-- | Adds a single `Transition` to a given `StateMachine`. Recommended to use
-- `addTransitions` for bulk additions as this uses the slower `updateVector`.
addTransition :: (StateLike s, Ord l, Semigroup e) => Transition l e -> StateMachine l s e -> Error (StateMachine l s e)
addTransition t sm@StateMachine {..} = do
  (ss, (l, se)) <- addTransitions' t sm
  m <- maybeToRight "Could not find start state (addTransition)" $ transitions !? ss
  ts <- updateVector ss (M.insert l se m) transitions
  return $ updateTransitions ts sm

-- | A helper function for `addTransitions` that turns a single `Transition` into a nested
-- tuple for use in `addTransitions`.
addTransitions' :: (StateLike s, Ord l, Semigroup e) => Transition l e -> StateMachine l s e -> Error (StateID, (l, (s StateID, e)))
addTransitions' Transition {..} StateMachine {..} = do
  ss <- lookupEither' ("Could not locate startStateT " ++ show startStateT ++ " (addTransitions')") startStateT namesToNumbers
  es <- lookupEither' ("Could not locate endStateT " ++ show endStateT ++ " (addTransitions')") endStateT namesToNumbers
  let l = characterT
      e = outputT
  when (l `S.notMember` language) $ Left "Character not in language"
  m <- maybeToRight "Could not find start state (addTransition)" $ transitions !? ss
  let combined
        | l `M.member` m = bimap (combineStates es) (e <>) (m M.! l)
        | otherwise = (fromSingle es, e)
  return (ss, (l, combined))

--TODO: foldrM and uses maps and stuff instead of doing all the accesses above and then redoing them again and again (type: Transition l e -> Map StateID (Map l (s StateID,e)) -> Error (Map StateID (Map l (s StateID,e))) )
-- foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b

-- | Takes a list of `Transition`s and a `StateMachine`, and returns a `StateMachine`
-- updated with those transitions.
addTransitions :: (StateLike s, Ord l, Semigroup e) => [Transition l e] -> StateMachine l s e -> Error (StateMachine l s e)
addTransitions ts sm@StateMachine {..} = do
  tups <- mapM (`addTransitions'` sm) ts
  let v = transitions V.// M.toList (foldr hFunc M.empty tups)
  return $ updateTransitions v sm
  where
    hFunc (sid, (l, (s, e))) m = M.insert sid (M.alter hFunc' l (findWithDefault M.empty sid m)) m
      where
        hFunc' (Just (s', e')) = Just (combineStateLike s s', e <> e')
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
constructStateMachine :: (Ord l, StateLike s, Semigroup e) => String -> Set l -> Set State -> [Transition l e] -> State -> Set State -> StepFunction l s e -> Error (StateMachine l s e)
constructStateMachine name' language' states' transitions' startState' acceptStates' step'' = do
  acceptStatesList <- mapM (\s -> lookupEither' ("Could not find accept state " ++ show s) s namesToNumbers') (S.toList (S.delete Dead acceptStates'))
  startStateID' <- lookupEither' ("Could not find start state " ++ show startState') startState' namesToNumbers'
  addTransitions transitions' $
    updateStartStateID startStateID' $
      updateAcceptStateIDs (S.fromList acceptStatesList) sm
  where
    states'' = S.delete Dead states'
    namesToNumbers' = M.insert Dead (-1) $ M.fromList $ zip (S.toAscList states'') [0 ..]
    sm = StateMachine name' language' (V.replicate (size states'') M.empty) (-2) S.empty step'' namesToNumbers'

-- | Infers a `StateMachine` from the transitions, using @getStatesAndLang@. Gets all the
-- states from the transitions, and gets the language from the transitions too. Assumes
-- the constructor has already added the `step` function to the state machine.
--
-- Check `constructStateMachine` to see how this works under the hood.
inferStateMachine' :: (Ord l) => (String -> Set l -> Set State -> [Transition l e] -> State -> Set State -> Error (StateMachine l s e)) -> String -> [Transition l e] -> State -> Set State -> Error (StateMachine l s e)
inferStateMachine' conSM name' transitions' = conSM name' language' states' transitions'
  where
    (states', language') = getStatesAndLang transitions'

-- | Infers a `StateMachine` from the transitions, using @getStatesAndLang@. Gets all the
-- states from the transitions, and gets the language from the transitions too.
--
-- Check `constructStateMachine` to see how this works under the hood.
inferStateMachine :: (Ord l, StateLike s, Semigroup e) => String -> [Transition l e] -> State -> Set State -> StepFunction l s e -> Error (StateMachine l s e)
inferStateMachine n ts ss as sf = inferStateMachine' (\name' language' states' transitions' startState' acceptStates' -> constructStateMachine name' language' states' transitions' startState' acceptStates' sf) n ts ss as

-- | Type class to define the three functions needed to construct a `StateMachine`.
-- Minimal definition requires `stepFunction`
class ConsSM l s e where
  -- | The function that moves the state machine between states.
  stepFunction :: (Ord l, Semigroup e) => StepFunction l s e

  -- | How to construct this state machine from scratch
  consSM :: (Ord l, StateLike s, Semigroup e) => String -> Set l -> Set State -> [Transition l e] -> State -> Set State -> Error (StateMachine l s e)
  consSM name' language' states' transitions' startState' acceptStates' = constructStateMachine name' language' states' transitions' startState' acceptStates' stepFunction

  -- | How to infer this state machine from scratch
  inferSM :: (Ord l, StateLike s, Semigroup e) => String -> [Transition l e] -> State -> Set State -> Error (StateMachine l s e)
  inferSM = inferStateMachine' consSM

-- | How to create a deterministic `StateMachine` such as `Data.StateMachines.DFA.DFA`s or
-- `Data.StateMachines.TuringMachine.TuringMachine`s.
instance ConsSM l Identity e where
  stepFunction (Identity s) = step' s
