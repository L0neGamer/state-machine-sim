module Lib where

-- import qualified Data.Map as M
-- import qualified Data.Set as S

import Data.Map (Map)
import Data.Set
import Data.Vector (Vector)

-- https://softwareengineering.stackexchange.com/questions/242795/what-is-the-free-monad-interpreter-pattern

data DSL next
  = Get String (String -> next)
  | Set String String next
  | End

instance Functor DSL where
  fmap f (Get name k) = Get name (f . k)
  fmap f (Set name value next) = Set name value (f next)
  fmap _ End = End

data Free f a = Free (f (Free f a)) | Return a 

-- instance (Show a, Show (f (Free f a))) => Show (Free f a) where
--   show (Return a) = "Return "++show a
--   show (Free a) = "Free "++show a
-- deriving instance (Show a) => Show (Free f a)

instance Functor f => Functor (Free f) where
  fmap f (Return a) = Return (f a)
  fmap f (Free a) = Free $ fmap (fmap f) a

instance (Functor f) => Applicative (Free f) where
  pure = Return
  (Return f) <*> a = fmap f a
  (Free f) <*> a = Free $ fmap (<*> a) f

instance Functor f => Monad (Free f) where
  return = Return
  Free a >>= f = Free (fmap (>>= f) a)
  Return a >>= f = f a

-- data ExStateMachine a b = ExStateMachine {
--   states :: InternalStates,
--   language :: Language a,
--   transitions :: Vector (Map a (InternalStates, b)),
--   startState :: InternalState,
--   accepStates :: AcceptStates
-- }

-- import Debug.Trace (trace)

-- data State = IdI Integer | IdS String | Dead deriving (Eq, Ord, Show)

-- type State a = Maybe a

-- type InternalState = State Int

-- data ReturnValue = Running | Timeout | Term Bool deriving (Eq, Show)

-- data Clock = I {time :: Integer, limit :: Integer} | Infinite {count :: Integer} deriving (Show, Ord, Eq)

-- type States a = Set (State a)

-- type InternalStates = Set InternalState

-- type AcceptStates = InternalStates

-- type Language a = Set a

-- data Transition a b c = Transition {startStateT :: State a, resultT :: b, characterT :: c}

-- -- type Transition a b c = (State a, b, c)

-- type Transitions a b c = [Transition a b c]

-- toState :: a -> State a
-- toState = Just

-- tickClock :: Clock -> Clock
-- tickClock (I i j) = I (i -1) j
-- tickClock (Infinite i) = Infinite (i + 1)

-- clock :: Integer -> Clock
-- clock i
--   | i >= 0 = I i i
--   | otherwise = Infinite 0

-- getTime :: Clock -> Integer
-- getTime (I i j) = j - i
-- getTime (Infinite i) = i

-- allIntStates :: [InternalState]
-- allIntStates = fmap Just [0, 1 ..]

-- type DFATransition a = M.Map a State

-- type DFATransitions a = M.Map State (DFATransition a)
-- data NFATransitionType a = Epsilon | Val a deriving (Show, Eq, Ord)

-- type NFATransition a = M.Map (NFATransitionType a) States

-- type NFATransitions a = M.Map State (NFATransition a)

-- data DFAStateMachine a = DFAStatMac
--   { states :: States,
--     language :: Language a,
--     transitions :: DFATransitions a,
--     startState :: State,
--     acceptStates :: AcceptStates
--   }
--   deriving (Show, Eq)

-- data NFAStateMachine a = NFAStatMac
--   { states :: States,
--     language :: Language (NFATransitionType a),
--     transitions :: NFATransitions a,
--     startState :: State,
--     acceptStates :: AcceptStates
--   }
--   deriving (Show)


-- getStatesAndLang :: (Ord a, Ord c, HasState b a)=> Transitions a b c -> (States a, Language c)
-- getStatesAndLang = Prelude.foldr combine (empty, empty)
--   where
--     combine (Transition a b c) (as, cs) = (insert a (getNextStates b `union` as), insert c cs)

-- class HasState a b where
--     getNextStates :: a -> States b

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
