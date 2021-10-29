module StateMachine
  ( StateLike (..),
    Transition (..),
    State (..),
    StateID,
    StateMachine (..),
    updateName,
    updateLanguage,
    updateTransitions,
    updateStartStateID,
    updateAcceptStateIDs,
    updateAddOutput,
    updateNamesToNumbers,
    runStep,
    tupleToSimpleTransition,
    inferStateMachine,
    constructStateMachine,
  )
where

import Control.Monad (when)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Map as M (Map, empty, fromList, insert, lookup, member, (!))
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
  )
import Data.Vector as V (Vector, replicate, (!?))
import Lib
  ( Error,
    Single (..),
    lookupEither',
    maybeToEither,
    updateVector,
  )

type StateID = Int

data State
  = Dead
  | State String
  deriving (Show, Eq, Ord)

class StateLike s where
  fromStateID :: StateID -> s StateID
  combineStates :: StateID -> s StateID -> s StateID

instance StateLike Set where
  fromStateID = singleton
  combineStates s ss = S.insert s ss

instance StateLike Single where
  fromStateID = Single
  combineStates s _ = Single s

data StateMachine l s e = StateMachine
  { name :: !String,
    language :: !(Set l),
    transitions :: !(StateLike s => Vector (Map l (s StateID, e))),
    startStateID :: !StateID,
    acceptStateIDs :: !(Set StateID),
    addOutput :: e -> e -> e,
    namesToNumbers :: !(Map State StateID)
  }

updateName :: String -> StateMachine l s e -> StateMachine l s e
updateName name' StateMachine {..} = StateMachine name' language transitions startStateID acceptStateIDs addOutput namesToNumbers

updateLanguage :: Set l -> StateMachine l s e -> StateMachine l s e
updateLanguage language' StateMachine {..} = StateMachine name language' transitions startStateID acceptStateIDs addOutput namesToNumbers

updateTransitions :: (StateLike s) => Vector (Map l (s StateID, e)) -> StateMachine l s e -> StateMachine l s e
updateTransitions transitions' StateMachine {..} = StateMachine name language transitions' startStateID acceptStateIDs addOutput namesToNumbers

updateStartStateID :: StateID -> StateMachine l s e -> StateMachine l s e
updateStartStateID startStateID' StateMachine {..} = StateMachine name language transitions startStateID' acceptStateIDs addOutput namesToNumbers

updateAcceptStateIDs :: Set StateID -> StateMachine l s e -> StateMachine l s e
updateAcceptStateIDs acceptStateIDs' StateMachine {..} = StateMachine name language transitions startStateID acceptStateIDs' addOutput namesToNumbers

updateAddOutput :: (e -> e -> e) -> StateMachine l s e -> StateMachine l s e
updateAddOutput addOutput' StateMachine {..} = StateMachine name language transitions startStateID acceptStateIDs addOutput' namesToNumbers

updateNamesToNumbers :: Map State StateID -> StateMachine l s e -> StateMachine l s e
updateNamesToNumbers namesToNumbers' StateMachine {..} = StateMachine name language transitions startStateID acceptStateIDs addOutput namesToNumbers'

instance (Show l, Show (s StateID), Show e, StateLike s) => Show (StateMachine l s e) where
  show sm =
    "StateMachine "
      ++ contained name
      ++ contained language
      ++ contained transitions
      ++ contained startStateID
      ++ contained acceptStateIDs
      --  ++ "(" ++ show (typeRep (Proxy :: Proxy s)) ++ ")"
      ++ contained namesToNumbers
    where
      contained f = "(" ++ show (f sm) ++ ") "

data Transition a b = Transition
  { startStateT :: State,
    endStateT :: State,
    characterT :: a,
    outputT :: b
  }
  deriving (Show, Eq)

tupleToSimpleTransition :: (State, State, a) -> Transition a ()
tupleToSimpleTransition (s, s', a) = Transition s s' a ()

getStatesAndLang :: (Ord a) => [Transition a b] -> (Set State, Set a)
getStatesAndLang = foldr combine (S.empty, S.empty)
  where
    combine (Transition s s' a _) (ss, as) = (S.insert s (S.insert s' ss), S.insert a as)

runStep :: (Ord l, (StateLike s)) => StateMachine l s e -> StateID -> l -> Error (s StateID, Maybe e)
runStep sm sid l
  | sid >= 0 = do
    m <- maybeToEither "Could not find state (runStep)" (t !? sid)
    interpretNothing $ M.lookup l m
  | otherwise = interpretNothing Nothing
  where
    t = transitions sm
    interpretNothing Nothing = return (fromStateID (-1), Nothing)
    interpretNothing (Just (s, e)) = return (s, Just e)

addTransition :: (StateLike s, Ord l) => Transition l e -> StateMachine l s e -> Error (StateMachine l s e)
addTransition Transition {..} sm@StateMachine {..} = do
  (ss, es, l, e) <- do
    ss <- lookupEither' ("Could not locate startStateT " ++ show startStateT ++ " (addTransition)") startStateT namesToNumbers
    es <- lookupEither' ("Could not locate endStateT " ++ show endStateT ++ " (addTransition)") endStateT namesToNumbers
    return (ss, es, characterT, outputT)
  when (l `S.notMember` language) $ Left "Character not in language"
  m <- maybeToEither "Could not find start state (addTransition)" $ transitions !? ss
  let combined
        | l `M.member` m = bimap (combineStates es) (addOutput e) (m M.! l)
        | otherwise = (fromStateID es, e)
  v <- maybeToEither "Could not update vector (addTransition)" $ updateVector ss (M.insert l combined m) transitions
  return $ updateTransitions v sm

addTransitions :: (StateLike s, Ord l) => [Transition l e] -> StateMachine l s e -> Error (StateMachine l s e)
addTransitions ts sm = foldr foldFunc (return sm) ts
  where
    foldFunc _ (Left s) = Left s
    foldFunc t (Right statemachine) = addTransition t statemachine

-- | @constructStateMachine@ takes the machine name, the language, all the states in the
-- machine, all the transitions between states using the language producing side effects
-- of type e, the start state, the accept states, a way to combine side effects, and will
-- return either an error from the construction of the state machine or return a state
-- machine
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

inferStateMachine :: (Ord l, StateLike s) => String -> [Transition l e] -> State -> Set State -> (e -> e -> e) -> Error (StateMachine l s e)
inferStateMachine = inferStateMachine' constructStateMachine
