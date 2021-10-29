module StateMachineSim.StateMachines.Convert (convertDFAToNFA) where

import qualified Data.Bifunctor
import Data.Map as M (map, mapKeys)
import Data.Set as S (map)
import StateMachineSim.Lib.Lib (Error)
import StateMachineSim.Lib.StateMachine
  ( StateLike (toSet),
    StateMachine
      ( StateMachine,
        acceptStateIDs,
        addOutput,
        language,
        name,
        namesToNumbers,
        startStateID,
        transitions
      ),
  )
import StateMachineSim.StateMachines.DFA (DFA)
import StateMachineSim.StateMachines.NFA (NFA, NFAData (Val))

-- | @convertDFAToNFA@ converts the given DFA into an NFA of the same type
convertDFAToNFA :: (Ord a) => DFA a -> Error (NFA a)
convertDFAToNFA StateMachine {..} =
  return $
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
