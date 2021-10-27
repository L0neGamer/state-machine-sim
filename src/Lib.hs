module Lib where

-- import qualified Data.Map as M
-- import qualified Data.Set as S

-- import qualified Data.Map as M
-- import qualified Data.Set as S
import Data.Map (Map, lookup)
import Data.Set (Set, empty, insert)
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))
import Data.Vector (Vector, (!?))

-- https://softwareengineering.stackexchange.com/questions/242795/what-is-the-free-monad-interpreter-pattern

data TapeDir
  = L
  | R
  deriving (Show, Eq)

data StateMachine l s e = StateMachine
  { name :: String,
    language :: Set l,
    transitions :: Vector (Map l (s, e)),
    startStateID :: StateID,
    acceptStateIDs :: Set StateID,
    getNext :: s -> If (s == StateID) StateID (Set StateID),
    namesToNumbers :: Map State StateID
  }

instance (Show l, Show s, Show e) => Show (StateMachine l s e) where
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

type StateID = Int

type DFA = StateMachine Char StateID ()

type NFA = StateMachine (Maybe Char) (Set StateID) ()

type TuringMachine = StateMachine Char StateID (Char, TapeDir)

data State
  = Dead
  | State String
  deriving (Show, Eq, Ord)

data Transition a b = Transition
  { startStateT :: State,
    endStateT :: State,
    characterT :: a,
    outputT :: b
  }
  deriving (Show, Eq)

type DFATransition = Transition Char ()

type NFATransition = Transition (Maybe Char) ()

type TuringMachineTransition = Transition Char (Char, TapeDir)

data Clock
  = Countdown {time :: Integer, limit :: Integer}
  | Infinite {count :: Integer}
  deriving (Show, Ord, Eq)

tickClock :: Clock -> Clock
tickClock (Countdown i j) = Countdown (i -1) j
tickClock (Infinite i) = Infinite (i + 1)

clock :: Integer -> Clock
clock i
  | i >= 0 = Countdown i i
  | otherwise = Infinite 0

getTime :: Clock -> Integer
getTime (Countdown i j) = j - i
getTime (Infinite i) = i

data ReturnValue = Running | Timeout | Term Bool deriving (Eq, Show)

getStatesAndLang :: (Ord a) => [Transition a b] -> (Set State, Set a)
getStatesAndLang = Prelude.foldr combine (empty, empty)
  where
    combine (Transition s s' a _) (ss, as) = (insert s (insert s' ss), insert a as)

step :: (Ord l) => StateMachine l s e -> StateID -> l -> Maybe (s, e)
step sm sid l = (t !? sid) >>= Data.Map.lookup l
  where
    t = transitions sm

mapStates' :: Map State StateID -> [Transition a b] -> [Either State (StateID, StateID, a, b)]
mapStates' m = fmap mpFunc
  where
    lookupEither s Nothing = Left s
    lookupEither _ (Just r) = Right r
    mpFunc Transition {..} = do
      ss <- lookupEither startStateT $ Data.Map.lookup startStateT m
      es <- lookupEither endStateT $ Data.Map.lookup endStateT m
      return (ss, es, characterT, outputT)

-- a is the language, b is the output from one step
-- class (HasState b Int) => StateMachine sm a b where
--     states :: sm a b -> InternalStates
--     language :: sm a b -> Language a
--     acceptStates :: sm a b -> AcceptStates
--     startState :: sm a b -> InternalState
--     transitionTable :: sm a b -> Vector (Map a b)
--     stepMachine :: InternalState -> a -> sm a b -> b
--     constructStateMachine :: InternalStates -> Language a -> Transitions Int b a -> InternalState -> AcceptStates -> sm a b
--     inferStateMachine :: (Ord a) => Transitions Int b a -> InternalState -> AcceptStates -> sm a b
--     inferStateMachine transitions = constructStateMachine stats lang transitions
--       where
--         (stats, lang) = getStatesAndLang transitions
-- run :: (Ord a) => [a] -> Clock -> sm a b -> ReturnValue

-- constructStateMachine'' :: (Ord a, Ord b) => (Language a -> Language b) -> (Transitions a -> transitionsMap) -> (States -> Language b -> transitionsMap -> State -> AcceptStates -> sm a) -> States -> Language a -> Transitions a -> State -> AcceptStates -> sm a
-- constructStateMachine'' langConv transitionsConv stateMachineCons states language transitions startState acceptStates
--   | startState `S.notMember` states = error "start state not in set of states"
--   | not (acceptStates `S.isSubsetOf` states) = error "accept states not subset of states"
--   | not (transitionStates `S.isSubsetOf` states) = error "transition states not subset of states"
--   | not (langConv transitionLanguage `S.isSubsetOf` language') = error "transition language not subset of language"
--   | otherwise = stateMachineCons states language' (transitionsConv transitions) startState acceptStates
--   where
--     (transitionStates, transitionLanguage) = getStatesAndLang transitions
--     language' = langConv language

-- constructStateMachine' :: (Ord a) => (Transitions a -> transitionsMap) -> (States -> Language a -> transitionsMap -> State -> AcceptStates -> sm a) -> States -> Language a -> Transitions a -> State -> AcceptStates -> sm a
-- constructStateMachine' = constructStateMachine'' id

-- -- TODO: https://archives.haskell.org/projects.haskell.org/diagrams/tutorials.html
-- -- or https://discordapp.com/channels/195989586260918272/222003210670440451/709816458921640057

-- class StateMachine sm where
--   constructStateMachine :: (Ord a) => States -> Language a -> Transitions a -> State -> AcceptStates -> sm a

--   inferStateMachine :: (Ord a) => Transitions a -> State -> AcceptStates -> sm a
--   inferStateMachine transitions = constructStateMachine states language transitions
--     where
--       (states, language) = getStatesAndLang transitions

--   addTransition :: (Ord a) => Transition a -> sm a -> sm a
--   removeTransition :: (Ord a) => Transition a -> sm a -> sm a

--   addTransitions :: (Ord a) => Transitions a -> sm a -> sm a
--   addTransitions ts sm = foldr addTransition sm ts

--   stepMachine :: (Ord a) => State -> a -> sm a -> States
--   stepMachine Dead _ _ = S.singleton Dead
--   stepMachine _ _ _ = error "no step found from state - incorrect statemachine setup"

--   run :: (Ord a) => [a] -> Clock -> sm a -> ReturnValue

--   smAcceptStates :: (Ord a) => sm a -> AcceptStates

--   nextStates :: (Ord a) => sm a -> State -> States
--   reachableStates' :: (Ord a) => sm a -> States -> States -> States
--   reachableStates' sm toCheck visited
--     | next == S.singleton Dead || null next = S.delete Dead $ S.union toCheck visited
--     | otherwise = reachableStates' sm next (S.union next visited)
--     where
--       next = S.difference (S.unions $ S.map (nextStates sm) toCheck) visited

--   reachableStates :: (Ord a) => sm a -> State -> States

--   isAcceptable :: (Ord a) => sm a -> State -> Bool
--   isAcceptable sm s = or $ S.map (`S.member` smAcceptStates sm) (reachableStates sm s)

-- class RunningStateMachine rsm where
--   getReturnValue :: (Ord a) => rsm a -> ReturnValue
--   step :: (Ord a) => rsm a -> rsm a
--   runSM :: (Ord a) => rsm a -> rsm a
--   runSM running
--     | getReturnValue running' == Running = runSM running'
--     | otherwise = running'
--     where
--       running' = step running

-- fromTuplesToMap :: (Ord a, Ord c) => (b -> b -> b) -> [(a, b, c)] -> M.Map a (M.Map c b)
-- fromTuplesToMap _ [] = M.empty
-- fromTuplesToMap f ((a, b, c) : xs) = M.unionWith (M.unionWith f) (M.singleton a (M.singleton c b)) mp
--   where
--     mp = fromTuplesToMap f xs

-- fromSingleton :: S.Set a -> a
-- fromSingleton (S.toList -> [x]) = x
-- fromSingleton _ = error "tried to get single item from non-singleton set"
