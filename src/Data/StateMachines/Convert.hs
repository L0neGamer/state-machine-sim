-- |
-- Module      :  Data.StateMachines.Convert
-- License     :  BSD3
--
-- Maintainer  :  L0neGamer
-- Stability   :  experimental
--
-- Functions for converting state machines.
module Data.StateMachines.Convert (convertDFAToNFA, convertNFAToDFA) where

import Data.Bifunctor (Bifunctor (first))
import Data.Functor.Identity (Identity)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.StateMachines.DFA (DFA)
import Data.StateMachines.Internal (Error)
import Data.StateMachines.NFA (NFA, NFAData (Epsilon, Val))
import Data.StateMachines.StateMachine (ConsSM (inferSM, stepFunction), State (Dead, State), StateID, StateLike (toSet), StateMachine (..), Transition (Transition), stateName')
import Data.Tuple (swap)
import qualified Data.Vector as V

-- | Converts the given DFA into an NFA of the same type.
convertDFAToNFA :: Ord a => DFA a -> NFA a
convertDFAToNFA StateMachine {..} =
  StateMachine
    ("NFA of `" ++ name ++ "`")
    (S.map Val language)
    transitions'
    startStateID
    acceptStateIDs
    stepFunction
    namesToNumbers
  where
    transitions' = fmap (M.map (Data.Bifunctor.first toSet) . M.mapKeys Val) transitions

convertNFAToDFA :: (Ord a) => NFA a -> Error (DFA a)
convertNFAToDFA nfa@StateMachine {..} = do
  ds <- destinationStates
  rs <- reachableStates ds
  ts <- foldr foldF (return []) rs
  (inferSM :: Ord a => String -> [Transition a ()] -> State -> Set State -> Error (StateMachine a Identity ())) ("DFA of `" ++ name ++ "`") ts (State $ toNewName (S.singleton startStateID)) (S.map (State . toNewName) $ S.filter (not . S.disjoint acceptStateIDs) rs)
  where
    dead = namesToNumbers M.! Dead
    numbersToNames = M.fromList $ swap <$> M.assocs namesToNumbers
    language' = S.map (\(Val a) -> a) $ S.delete Epsilon language
    destinationStates = S.delete S.empty <$> (S.fromList . fmap (S.delete dead . fst) <$> mapM (\l -> step (S.fromList [0 .. V.length transitions - 1]) (Val l) nfa) (S.toList language'))
    getReachableStateFunction ss l = S.delete dead . fst <$> step ss (Val l) nfa
    reachableStates ds = getReachableStates startStateID dead language' (S.insert (S.singleton startStateID) ds) getReachableStateFunction
    toNewName s = intercalate ", " $ stateName' . (numbersToNames M.!) <$> S.toAscList s
    foldF startStates errTransitions = do
      transitions' <- errTransitions
      resultant <- mapM (\l -> (l,) <$> getReachableStateFunction startStates l) (S.toAscList language')
      return (fmap (\(l, d) -> Transition (State $ toNewName startStates) (State $ toNewName d) l ()) (filter (not . S.null . snd) resultant) ++ transitions')

getReachableStates :: StateID -> StateID -> Set l -> Set (Set StateID) -> (Set StateID -> l -> Error (Set StateID)) -> Error (S.Set (S.Set StateID))
getReachableStates start dead lang curr stepF = do
  next' <- next
  let next'' = S.delete S.empty $ S.insert (S.singleton start) next'
  if next'' == curr
    then return next''
    else getReachableStates start dead lang next'' stepF
  where
    next = S.fromList <$> sequence ((stepF <$> S.toList curr) <*> S.toList lang)
