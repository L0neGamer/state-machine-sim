-- |
-- Module      :  Data.StateMachines.Convert
-- License     :  BSD3
--
-- Maintainer  :  L0neGamer
-- Stability   :  experimental
--
-- Functions for converting state machines.
module Data.StateMachines.Convert (convertDFAToNFA, convertNFAToDFA, reverseNFA) where

import Data.Bifunctor (Bifunctor (first))
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.StateMachines.DFA (DFA)
import Data.StateMachines.Internal (Error)
import Data.StateMachines.NFA (NFA, NFAData (Epsilon, Val), expandEpsilon)
import Data.StateMachines.StateMachine
  ( ConsSM (inferSM, stepFunction),
    State (Dead, State),
    StateID,
    StateLike (toSet),
    StateMachine (..),
    Transition (Transition),
    getState,
    getTransitions,
    stateName,
  )

-- | Converts the given DFA into an NFA of the same type.
convertDFAToNFA :: Ord a => DFA a -> NFA a
convertDFAToNFA StateMachine {..} =
  StateMachine
    ("NFA of '" ++ name ++ "'")
    (S.map Val language)
    transitions'
    startStateID
    acceptStateIDs
    stepFunction
    namesToNumbers
  where
    transitions' = fmap (M.map (Data.Bifunctor.first toSet) . M.mapKeys Val) transitions

convertNFAToDFA :: (Ord a, Show a) => NFA a -> Error (DFA a)
convertNFAToDFA nfa@StateMachine {..} = do
  startStates <- expandEpsilon (S.singleton startStateID) nfa
  rs <- reachableStates startStates
  ts <- foldr foldF (return []) rs
  inferSM ("DFA of '" ++ name ++ "'") ts (toState startStates) (S.map toState $ S.filter (not . S.disjoint acceptStateIDs) rs)
  where
    language' = S.toAscList $ S.map (\(Val a) -> a) $ S.delete Epsilon language
    getReachableStateFunction ss l = S.delete (namesToNumbers M.! Dead) . fst <$> step ss (Val l) nfa
    adjustStates ss = S.delete S.empty . S.insert ss . S.fromList
    reachableStates ss = getReachableStates (adjustStates ss) language' (S.singleton ss) getReachableStateFunction
    toState s = State $ intercalate ", " $ stateName . getState nfa <$> S.toAscList s
    foldF startStates errTransitions = do
      transitions' <- errTransitions
      resultant <- mapM (\l -> (l,) <$> getReachableStateFunction startStates l) language'
      return (fmap (\(l, d) -> Transition (toState startStates) (toState d) l ()) (filter (not . S.null . snd) resultant) ++ transitions')

getReachableStates :: ([Set StateID] -> Set (Set StateID)) -> [l] -> Set (Set StateID) -> (Set StateID -> l -> Error (Set StateID)) -> Error (Set (Set StateID))
getReachableStates adjustStates lang curr stepF = do
  next' <- sequence ((stepF <$> S.toList curr) <*> lang)
  let next'' = adjustStates next'
  if next'' == curr
    then return next''
    else getReachableStates adjustStates lang next'' stepF

reverseNFA :: (Ord a, Show a) => NFA a -> Error (NFA a)
reverseNFA nfa@StateMachine {..} = inferSM ("reverse of " ++ name) ts' newStartState (S.singleton (markOld $ getState nfa startStateID))
  where
    ts = getTransitions nfa
    markOld Dead = Dead
    markOld (State s) = State ("." ++ s)
    newStartState = State "start"
    revts = fmap (\(Transition s s' l e) -> Transition (markOld s') (markOld s) l e) ts
    morets = fmap ((\as -> Transition newStartState (markOld as) Epsilon ()) . getState nfa) (S.toList acceptStateIDs)
    ts' = morets ++ revts
