module Regex where

-- thanks to this page for help in this file: http://matt.might.net/articles/parsing-regex-with-recursive-descent/

import Data.Set as S (singleton)
import Lib (Error)
import NFA (NFA, NFATransition, runNFA)
import RunStateMachine (ReturnValue, clock, extractResult)
import StateMachine (State (State), inferStateMachine, tupleToSimpleTransition)

type Err = String

data RegexToken
  = LParen
  | RParen
  | Chr Char
  | KleeneStar
  | Alternation
  deriving (Show, Eq)

type TokenStream = [RegexToken]

data Base
  = BaseChar Char
  | BaseGroup Regex
  deriving (Show, Eq)

data Factor
  = FactorSingle Base
  | FactorStar Factor
  deriving (Show, Eq)

data Term
  = TermNil
  | TermFact Factor Term
  deriving (Show, Eq)

data Regex
  = RegexSingle Term
  | RegexAlternation Term Regex
  deriving (Show, Eq)

isChr :: RegexToken -> Bool
isChr (Chr _) = True
isChr _ = False

charToToken :: Char -> RegexToken
charToToken '*' = KleeneStar
charToToken '(' = LParen
charToToken ')' = RParen
charToToken '|' = Alternation
charToToken x = Chr x

toTokens :: String -> TokenStream
toTokens [] = []
toTokens ('\\' : x : xs) = Chr x : toTokens xs
toTokens (x : xs) = charToToken x : toTokens xs

parseRegex :: TokenStream -> (TokenStream, Regex)
parseRegex toks
  | checkNext toks' Alternation = (toks'', RegexAlternation term regex)
  | otherwise = (toks', RegexSingle term)
  where
    (toks', term) = parseTerm toks
    (toks'', regex) = parseRegex (tail toks')

parseTerm :: TokenStream -> (TokenStream, Term)
parseTerm [] = ([], TermNil)
parseTerm toks@(tok : _)
  | tok `isIn` baseChecks = (toks'', TermFact fact fact')
  | otherwise = (toks, TermNil)
  where
    baseChecks = [isChr, (== LParen)]
    (toks', fact) = parseFactor toks
    (toks'', fact') = parseTerm toks'

parseFactor :: TokenStream -> (TokenStream, Factor)
parseFactor toks = (toks'', fact)
  where
    (toks', base) = parseBase toks
    (toks'', fact) = parseFactor' toks' (FactorSingle base)

parseFactor' :: TokenStream -> Factor -> (TokenStream, Factor)
parseFactor' (KleeneStar : toks) fact = parseFactor' toks (FactorStar fact)
parseFactor' toks fact = (toks, fact)

parseBase :: TokenStream -> (TokenStream, Base)
parseBase (Chr c : toks) = (toks, BaseChar c)
parseBase (LParen : toks)
  | checkNext toks' RParen = (tail toks', BaseGroup regex)
  | otherwise = error $ "regex parentheses could not be parsed:\n\t" ++ show (LParen : toks)
  where
    (toks', regex) = parseRegex toks
parseBase t = error $ "unexpected non-base token:" ++ show t

checkNext :: TokenStream -> RegexToken -> Bool
checkNext [] _ = False
checkNext (x : _) tok = x == tok

isIn :: RegexToken -> [RegexToken -> Bool] -> Bool
isIn tok = any ($ tok)

strToParsedRegex :: String -> Regex
strToParsedRegex str
  | null toks = regex
  | otherwise = error $ "regex was not parsed correctly:\n\tLeftover:" ++ show toks ++ "\n\tOriginal:" ++ show str
  where
    (toks, regex) = parseRegex . toTokens $ str

stateFromInteger :: Integer -> State
stateFromInteger = State . show

tupleIntegerState :: (Integer, Integer, a) -> (State, State, a)
tupleIntegerState (i, i', a) = (stateFromInteger i, stateFromInteger i', a)

convertList :: [(Integer, Integer, Maybe a)] -> [NFATransition a]
convertList = (tupleToSimpleTransition . tupleIntegerState <$>)

regexToNFA :: Regex -> Integer -> (Integer, Integer, [NFATransition Char])
regexToNFA (RegexSingle term) i = termToNFA term i
regexToNFA (RegexAlternation term regex) i = (q0, q3, ts)
  where
    q0 = i
    (q1, q2, xs) = termToNFA term (q0 + 1)
    (q3, q4, xs') = regexToNFA regex (q2 + 1)
    q5 = q4 + 1
    ts = xs ++ xs' ++ convertList [(q0, q1, Nothing), (q0, q3, Nothing), (q2, q5, Nothing), (q4, q5, Nothing)]

termToNFA :: Term -> Integer -> (Integer, Integer, [NFATransition Char])
termToNFA TermNil i = (i, i, [])
termToNFA (TermFact fact term) i = (q0, q2, ts)
  where
    q0 = i
    (_, q1, xs) = factToNFA fact i
    (_, q2, xs') = termToNFA term q1
    ts = xs ++ xs'

factToNFA :: Factor -> Integer -> (Integer, Integer, [NFATransition Char])
factToNFA (FactorStar fact) i = (q0, q1, ts)
  where
    (q0, q1, xs) = factToNFA fact i
    ts = xs ++ convertList [(q0, q1, Nothing), (q1, q0, Nothing)]
factToNFA (FactorSingle base) i = baseToNFA base i

baseToNFA :: Base -> Integer -> (Integer, Integer, [NFATransition Char])
baseToNFA (BaseChar c) i = (i, i + 1, [tupleToSimpleTransition (stateFromInteger i, stateFromInteger (i + 1), Just c)])
baseToNFA (BaseGroup regex) s = regexToNFA regex s

-- regexToNFA :: Regex -> Integer -> (State, State, [(State, State, NFA.NFATransitionType Char)])
-- regexToNFA (RegexSingle term) i = termToNFA term i
-- regexToNFA (RegexAlternation term regex) i = (q0, q3, transitions)
--   where
--     q0 = IdI i
--     (q1, IdI i', xs) = termToNFA term (i + 1)
--     (q2, IdI i'', xs') = regexToNFA regex (i' + 1)
--     q3 = IdI (i'' + 1)
--     transitions = xs ++ xs' ++ [(q0, q1, NFA.Epsilon), (q0, q2, NFA.Epsilon), (IdI i', q3, NFA.Epsilon), (IdI i'', q3, NFA.Epsilon)]

-- termToNFA TermNil i = (IdI i, IdI i, [])
-- termToNFA (TermFact fact term) i = (q0, q2, transitions)
--   where
--     (q0, IdI i', xs) = factToNFA fact i
--     (_, q2, xs') = termToNFA term i'
--     transitions = xs ++ xs'

-- factToNFA :: Factor -> Integer -> (State, State, [(State, State, NFA.NFATransitionType Char)])
-- factToNFA (FactorStar fact) i = (q0, q1, transitions)
--   where
--     (q0, q1, xs) = factToNFA fact i
--     transitions = xs ++ [(q0, q1, NFA.Epsilon), (q1, q0, NFA.Epsilon)]
-- factToNFA (FactorSingle base) i = baseToNFA base i

regexStrToNFA :: String -> Error (NFA Char)
regexStrToNFA str = inferStateMachine ("regex NFA `" ++ str ++ "`") xs (stateFromInteger start) (S.singleton $ stateFromInteger end) const
  where
    (start, end, xs) = regexToNFA (strToParsedRegex str) 0

-- emptyMachine = NFA.NFAStatMac states lang M.empty start (S.singleton end)

-- testRegexStr :: String
-- testRegexStr = "ab(aab)*bb"

checkString :: String -> String -> Error ReturnValue
checkString inpStr regex = do
  nfa <- regexStrToNFA regex
  extractResult $ runNFA (map Just inpStr) (clock 0) nfa
