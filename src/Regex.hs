module Regex (checkString, regexStrToNFA) where

-- thanks to this page for help in this file:
-- http://matt.might.net/articles/parsing-regex-with-recursive-descent/

import Data.Set as S (singleton)
import Lib (Error)
import NFA (NFA, NFAData (Epsilon, Val), NFATransition, runNFA)
import RunStateMachine (ReturnValue, clock, extractResult)
import StateMachine (State (State), inferStateMachine, tupleToSimpleTransition)

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

parseRegex :: TokenStream -> Error (TokenStream, Regex)
parseRegex toks = do
  (toks', term) <- parseTerm toks
  if checkNext toks' Alternation
    then
      ( do
          (toks'', regex) <- parseRegex (tail toks')
          return (toks'', RegexAlternation term regex)
      )
    else return (toks', RegexSingle term)

parseTerm :: TokenStream -> Error (TokenStream, Term)
parseTerm [] = return ([], TermNil)
parseTerm toks@(tok : _) = do
  if tok `isIn` baseChecks
    then
      ( do
          (toks', fact) <- parseFactor toks
          (toks'', fact') <- parseTerm toks'
          return (toks'', TermFact fact fact')
      )
    else return (toks, TermNil)
  where
    baseChecks = [isChr, (== LParen)]

parseFactor :: TokenStream -> Error (TokenStream, Factor)
parseFactor toks = do
  (toks', base) <- parseBase toks
  (toks'', fact) <- parseFactor' toks' (FactorSingle base)
  return (toks'', fact)

parseFactor' :: TokenStream -> Factor -> Error (TokenStream, Factor)
parseFactor' (KleeneStar : toks) fact = parseFactor' toks (FactorStar fact)
parseFactor' toks fact = return (toks, fact)

parseBase :: TokenStream -> Error (TokenStream, Base)
parseBase (Chr c : toks) = return (toks, BaseChar c)
parseBase (LParen : toks) = do
  (toks', regex) <- parseRegex toks
  if checkNext toks' RParen
    then return (tail toks', BaseGroup regex)
    else Left $ "regex parentheses could not be parsed:\n\t" ++ show (LParen : toks)
parseBase t = Left $ "unexpected non-base token:" ++ show t

checkNext :: TokenStream -> RegexToken -> Bool
checkNext [] _ = False
checkNext (x : _) tok = x == tok

isIn :: RegexToken -> [RegexToken -> Bool] -> Bool
isIn tok = any ($ tok)

strToParsedRegex :: String -> Error Regex
strToParsedRegex str = do
  (toks, regex) <- parseRegex . toTokens $ str
  if null toks
    then return regex
    else
      Left $
        "regex was not parsed correctly. Leftover:"
          ++ show toks
          ++ " Original:"
          ++ show str

stateFromInteger :: Integer -> State
stateFromInteger = State . show

tupleIntegerState :: (Integer, Integer, a) -> (State, State, a)
tupleIntegerState (i, i', a) = (stateFromInteger i, stateFromInteger i', a)

convertList :: [(Integer, Integer, NFAData a)] -> [NFATransition a]
convertList = (tupleToSimpleTransition . tupleIntegerState <$>)

regexToNFA :: Regex -> Integer -> (Integer, Integer, [NFATransition Char])
regexToNFA (RegexSingle term) i = termToNFA term i
regexToNFA (RegexAlternation term regex) i = (q0, q5, ts)
  where
    q0 = i
    (q1, q2, xs) = termToNFA term (q0 + 1)
    (q3, q4, xs') = regexToNFA regex (q2 + 1)
    q5 = q4 + 1
    ts =
      xs
        ++ xs'
        ++ convertList
          [ (q0, q1, Epsilon),
            (q0, q3, Epsilon),
            (q2, q5, Epsilon),
            (q4, q5, Epsilon)
          ]

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
    ts = xs ++ convertList [(q0, q1, Epsilon), (q1, q0, Epsilon)]
factToNFA (FactorSingle base) i = baseToNFA base i

baseToNFA :: Base -> Integer -> (Integer, Integer, [NFATransition Char])
baseToNFA (BaseChar c) i = (i, i + 1, convertList [(i, i + 1, Val c)])
baseToNFA (BaseGroup regex) s = regexToNFA regex s

regexStrToNFA :: String -> Error (NFA Char)
regexStrToNFA str = do
  regex <- strToParsedRegex str
  let (start, end, xs) = regexToNFA regex 0
  inferStateMachine
    ("regex NFA `" ++ str ++ "`")
    xs
    (stateFromInteger start)
    (S.singleton $ stateFromInteger end)
    const

checkString :: String -> String -> Error ReturnValue
checkString inpStr regex = do
  nfa <- regexStrToNFA regex
  extractResult $ runNFA inpStr (clock 0) nfa
