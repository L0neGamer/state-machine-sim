module StateMachine where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Map as M (Map, empty, fromList, insert, lookup, member, (!))
import Data.Set as S (Set, delete, empty, fromList, insert, notMember, singleton, size, toAscList, toList)
import Data.Vector as V (Vector, replicate, (!?))
import Lib
  ( Error,
    Single (..),
    lookupEither,
    lookupError,
    maybeToError,
    updateVector,
  )

type StateID = Int

data State
  = Dead
  | State String
  deriving (Show, Eq, Ord)

numberStates :: [State]
numberStates = fmap (State . show) ([0 ..] :: [Int])

q0, q1, q2, q3, q4, q5 :: State
q0 : q1 : q2 : q3 : q4 : q5 : _ = numberStates

getState :: State -> Error String
getState Dead = Left "Tried to unwrap Dead State"
getState (State s) = Right s

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

updatenamesToNumbers :: Map State StateID -> StateMachine l s e -> StateMachine l s e
updatenamesToNumbers namesToNumbers' StateMachine {..} = StateMachine name language transitions startStateID acceptStateIDs addOutput namesToNumbers'

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

simpleTransition :: State -> State -> a -> Transition a ()
simpleTransition s s' a = Transition s s' a ()

tupleToTransition :: (State, State, a, b) -> Transition a b
tupleToTransition (s, s', a, b) = Transition s s' a b

tupleToSimpleTransition :: (State, State, a) -> Transition a ()
tupleToSimpleTransition (s, s', a) = simpleTransition s s' a

getStatesAndLang :: (Ord a) => [Transition a b] -> (Set State, Set a)
getStatesAndLang = Prelude.foldr combine (S.empty, S.empty)
  where
    combine (Transition s s' a _) (ss, as) = (S.insert s (S.insert s' ss), S.insert a as)

runStep :: (Ord l, (StateLike s)) => StateMachine l s e -> StateID -> l -> Error (s StateID, Maybe e)
runStep sm sid l
  | sid >= 0 = do
    m <- maybeToError "Could not find state (runStep)" (t !? sid)
    interpretNothing $ M.lookup l m
  | otherwise = interpretNothing Nothing
  where
    t = transitions sm
    interpretNothing Nothing = Right (fromStateID (-1), Nothing)
    interpretNothing (Just (s, e)) = Right (s, Just e)

mapStates :: Map State StateID -> [Transition a b] -> ([(StateID, StateID, a, b)], [State])
mapStates m = foldr (combiner . mpFunc) ([], [])
  where
    mpFunc Transition {..} = do
      ss <- lookupEither startStateT m
      es <- lookupEither endStateT m
      return (ss, es, characterT, outputT)
    combiner (Left b) (ls, bs) = (ls, b : bs)
    combiner (Right l) (ls, bs) = (l : ls, bs)

addTransition :: (StateLike s, Ord l) => Transition l e -> StateMachine l s e -> Error (StateMachine l s e)
addTransition Transition {..} sm@StateMachine {..} = do
  (ss, es, l, e) <- do
    ss <- lookupError ("Could not locate startStateT " ++ show startStateT ++ " (addTransition)") startStateT namesToNumbers
    es <- lookupError ("Could not locate endStateT " ++ show endStateT ++ " (addTransition)") endStateT namesToNumbers
    return (ss, es, characterT, outputT)
  _ <- if l `S.notMember` language then Left "Character not in language" else Right ()
  m <- maybeToError "Could not find start state (addTransition)" $ transitions !? ss
  let combined
        | l `M.member` m = bimap (combineStates es) (addOutput e) (m M.! l)
        | otherwise = (fromStateID es, e)
  v <- maybeToError "Could not update vector (addTransition)" $ updateVector ss (M.insert l combined m) transitions
  Right $ updateTransitions v sm

addTransitions :: (StateLike s, Ord l) => [Transition l e] -> StateMachine l s e -> Error (StateMachine l s e)
addTransitions ts sm = foldr foldFunc (Right sm) ts
  where
    foldFunc _ (Left s) = Left s
    foldFunc t (Right statemachine) = addTransition t statemachine

constructStateMachine :: (Ord l, StateLike s) => String -> Set l -> Set State -> [Transition l e] -> State -> Set State -> (e -> e -> e) -> Error (StateMachine l s e)
constructStateMachine name' language' states' transitions' startState' acceptStates' addOutput' = do
  acceptStatesList <- mapM (\s -> lookupError ("Could not find accept state " ++ show s) s namesToNumbers') (S.toList (S.delete Dead acceptStates'))
  startStateID' <- lookupError ("Could not find start state " ++ show startState') startState' namesToNumbers'
  let smASI = updateAcceptStateIDs (S.fromList acceptStatesList) sm
      smSSID = updateStartStateID startStateID' smASI
  addTransitions transitions' smSSID
  where
    states'' = S.delete Dead states'
    namesToNumbers' = M.insert Dead (-1) $ M.fromList $ zip (S.toAscList states'') [0 ..]
    sm = StateMachine name' language' (V.replicate (size states'') M.empty) (-2) S.empty addOutput' namesToNumbers'

inferStateMachine :: (Ord l, StateLike s) => String -> [Transition l e] -> State -> Set State -> (e -> e -> e) -> Error (StateMachine l s e)
inferStateMachine name' transitions' = constructStateMachine name' language' states' transitions'
  where
    (states', language') = getStatesAndLang transitions'
