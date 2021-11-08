-- |
-- Module      :  Data.StateMachines.Convert
-- License     :  BSD3
--
-- Maintainer  :  L0neGamer
-- Stability   :  experimental
--
-- Functions for converting state machines.
module Data.StateMachines.Convert (convertDFAToNFA) where

import Data.Bifunctor (Bifunctor (first))
import Data.Map as M (map, mapKeys)
import Data.Set as S (map)
import Data.StateMachines.DFA (DFA)
import Data.StateMachines.NFA (NFA, NFAData (Val))
import Data.StateMachines.StateMachine (ConsSM (stepFunction), StateLike (toSet), StateMachine (..))

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
