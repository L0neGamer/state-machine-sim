module Convert (convertDFAToNFA) where

import DFA (DFA)
import qualified Data.Bifunctor
import Data.Map as M (map, mapKeys)
import Data.Set as S (map, singleton)
import Lib (Error, Single (Single))
import NFA (NFA, NFAData (Val))
import StateMachine (StateMachine (StateMachine, acceptStateIDs, addOutput, language, name, namesToNumbers, startStateID, transitions))

-- | @convertDFAToNFA@ converts the given DFA into an NFA of the same type
convertDFAToNFA :: (Ord a) => DFA a -> Error (NFA a)
convertDFAToNFA StateMachine {..} = return $ StateMachine ("NFA of `" ++ name ++ "`") (S.map Val language) transitions' startStateID acceptStateIDs addOutput namesToNumbers
  where
    conv (Single s) = S.singleton s
    transitions' = fmap (M.map (Data.Bifunctor.first conv) . M.mapKeys Val) transitions
