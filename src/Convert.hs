{-# LANGUAGE OverloadedStrings, RecordWildCards, InstanceSigs #-}
module Convert
 where

import Lib
import DFA (DFAStateMachine(DFAStatMac), startState, acceptStates, states,language, mapping)
import NFA (NFAStateMachine(NFAStatMac), NFATransitionType(Val))

import qualified Data.Map as M
import qualified Data.Set as S

convertDFAToNFA :: (Ord a) => DFAStateMachine a -> NFAStateMachine a
convertDFAToNFA dfa = NFAStatMac (startState dfa) (acceptStates dfa) (states dfa) (language dfa) (M.map (M.map S.singleton . M.mapKeys Val) (mapping dfa))

