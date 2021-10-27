module Lib where

import Data.Map as M (Map, lookup, member, (!))
import Data.Vector as V (Vector, (//))

type Error a = Either String a

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither s Nothing = Left s
maybeToEither _ (Just a) = Right a

maybeToError :: String -> Maybe a -> Error a
maybeToError = maybeToEither

lookupEither :: (Ord k) => k -> Map k v -> Either k v
lookupEither k m
  | k `M.member` m = Right $ m M.! k
  | otherwise = Left k

lookupError :: (Ord k) => String -> k -> Map k v -> Error v
lookupError s k = maybeToError s . M.lookup k

updateVector :: Int -> a -> Vector a -> Maybe (Vector a)
updateVector i a v
  | i < 0 || i >= length v = Nothing
  | otherwise = Just $ v // [(i, a)]

if' :: Bool -> a -> a -> a
if' True a _ = a
if' False _ a = a

newtype Single a = Single a deriving (Show, Eq, Ord)

class Peekable a where
  peek :: a b -> Error b

instance Peekable [] where
  peek [] = Left "Empty List when peeking"
  peek (x : _) = Right x

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
