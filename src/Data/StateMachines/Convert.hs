module Data.StateMachines.Convert (convertDFAToNFA) where

import qualified Data.Bifunctor
import Data.Map as M (map, mapKeys)
import Data.Set as S (map)
import Data.StateMachines.DFA (DFA)
import Data.StateMachines.NFA (NFA, NFAData (Val))
import Data.StateMachines.StateMachine (StateLike (toSet), StateMachine (..))

-- | @convertDFAToNFA@ converts the given DFA into an NFA of the same type
convertDFAToNFA :: (Ord a) => DFA a -> NFA a
convertDFAToNFA StateMachine {..} =
  StateMachine
    ("NFA of `" ++ name ++ "`")
    (S.map Val language)
    transitions'
    startStateID
    acceptStateIDs
    addOutput
    namesToNumbers
  where
    transitions' = fmap (M.map (Data.Bifunctor.first toSet) . M.mapKeys Val) transitions
