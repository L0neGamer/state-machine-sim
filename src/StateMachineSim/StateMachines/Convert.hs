module StateMachineSim.StateMachines.Convert (convertDFAToNFA) where

import qualified Data.Bifunctor
import Data.Map as M (map, mapKeys)
import Data.Set as S (map, singleton)
import StateMachineSim.Lib.Lib (Error, Single (Single))
import StateMachineSim.Lib.StateMachine
  ( StateMachine
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
    conv (Single s) = S.singleton s
    transitions' = fmap (M.map (Data.Bifunctor.first conv) . M.mapKeys Val) transitions
