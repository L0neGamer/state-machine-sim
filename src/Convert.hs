module Convert where

-- import Lib
import DFA (DFAStateMachine, acceptStates, language, startState, states, transitions)
import qualified Data.Map as M
import qualified Data.Set as S
import NFA (NFAStateMachine (NFAStatMac), NFATransitionType (Val))

convertDFAToNFA :: (Ord a) => DFAStateMachine a -> NFAStateMachine a
convertDFAToNFA dfa = NFAStatMac (states dfa) (S.map Val (language dfa)) (M.map (M.map S.singleton . M.mapKeys Val) (transitions dfa)) (startState dfa) (acceptStates dfa)
