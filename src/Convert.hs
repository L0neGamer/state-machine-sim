{-# LANGUAGE OverloadedStrings, RecordWildCards, InstanceSigs #-}
module Convert
 where

-- import Lib
import DFA (DFAStateMachine, states, language, transitions, startState, acceptStates)
import NFA (NFAStateMachine(NFAStatMac), NFATransitionType(Val))

import qualified Data.Map as M
import qualified Data.Set as S

convertDFAToNFA :: (Ord a) => DFAStateMachine a -> NFAStateMachine a
convertDFAToNFA dfa = NFAStatMac (states dfa) (S.map Val (language dfa)) (M.map (M.map S.singleton . M.mapKeys Val) (transitions dfa)) (startState dfa) (acceptStates dfa)
