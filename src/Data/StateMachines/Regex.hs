-- |
-- Module      :  Data.StateMachines.Regex
-- License     :  BSD3
--
-- Maintainer  :  L0neGamer
-- Stability   :  experimental
--
-- Functions and data types for processing regex strings and for matching strings with
-- regex, using NFAs under the hood.
--
-- Currently, only alternation, kleene star, and capturing groups, as well as multiple
-- characters in a row.
module Data.StateMachines.Regex
  ( regexStrToNFA,
    checkString,
  )
where

-- thanks to this page for help in this file:
-- http://matt.might.net/articles/parsing-regex-with-recursive-descent/

import Data.Set as S (singleton)
import Data.StateMachines.Internal (Error)
import Data.StateMachines.NFA (NFA, NFAData (Epsilon, Val), NFATransition, runNFA)
import Data.StateMachines.RunStateMachine (ReturnValue, clock, extractResult)
import Data.StateMachines.StateMachine (State (State), inferStateMachine, tupleToSimpleTransition)

-- | Represents all the tokens that can be expected from an input string.
data RegexToken
  = LParen
  | RParen
  | Chr Char
  | KleeneStar
  | Alternation
  deriving (Show, Eq)

-- | Represents the base elements of the regex AST (single characters, groups).
data Base
  = BaseChar Char
  | BaseGroup Regex
  deriving (Show, Eq)

-- | Represents either a `Base` element, or one or more repetitions.
data Factor
  = FactorSingle Base
  | FactorStar Factor
  deriving (Show, Eq)

-- | `Term` represents either nothing, or one or more `Factor`s.
data Term
  = TermNil
  | TermFact Factor Term
  deriving (Show, Eq)

-- | Represents either a single `Term`, or an alternation between a `Term` and
-- another `Regex`.
data Regex
  = RegexSingle Term
  | RegexAlternation Term Regex
  deriving (Show, Eq)

-- | Checks if a `RegexToken` is just a character or not.
isChr :: RegexToken -> Bool
isChr (Chr _) = True
isChr _ = False

-- | Converts a character into its `RegexToken`.
charToToken :: Char -> RegexToken
charToToken '*' = KleeneStar
charToToken '(' = LParen
charToToken ')' = RParen
charToToken '|' = Alternation
charToToken x = Chr x

-- | Takes a `String` and returns a list of `RegexToken`s, maintaining escaped characters.
toTokens :: String -> [RegexToken]
toTokens [] = []
toTokens ('\\' : x : xs) = Chr x : toTokens xs
toTokens (x : xs) = charToToken x : toTokens xs

-- | Takes a stream of `RegexToken`s, and returns a parsed `Regex` and the rest of the
-- stream.
parseRegex :: [RegexToken] -> Error ([RegexToken], Regex)
parseRegex toks = do
  (toks', term) <- parseTerm toks
  if checkNext toks' Alternation
    then
      ( do
          (toks'', regex) <- parseRegex (tail toks')
          return (toks'', RegexAlternation term regex)
      )
    else return (toks', RegexSingle term)

-- | Takes a stream of `RegexToken`s, and returns a parsed `Term` and the
-- rest of the stream.
parseTerm :: [RegexToken] -> Error ([RegexToken], Term)
parseTerm [] = return ([], TermNil)
parseTerm toks@(tok : _) = do
  if tok `isIn` [isChr, (== LParen)]
    then
      ( do
          (toks', fact) <- parseFactor toks
          (toks'', fact') <- parseTerm toks'
          return (toks'', TermFact fact fact')
      )
    else return (toks, TermNil)

-- | Takes a stream of `RegexToken`s, and returns a parsed `Factor` and the
-- rest of the stream.
parseFactor :: [RegexToken] -> Error ([RegexToken], Factor)
parseFactor toks = do
  (toks', base) <- parseBase toks
  (toks'', fact) <- parseFactor' toks' (FactorSingle base)
  return (toks'', fact)

-- | A helper function that takes a stream of `RegexToken`s and an already parsed Factor
-- and returns a parsed `Factor` and the rest of the stream (for multiple `KleeneStar`s).
parseFactor' :: [RegexToken] -> Factor -> Error ([RegexToken], Factor)
parseFactor' (KleeneStar : toks) fact = parseFactor' toks (FactorStar fact)
parseFactor' toks fact = return (toks, fact)

-- | Takes a stream of `RegexToken`s, and returns a parsed `Base` and the rest of the
-- stream.
parseBase :: [RegexToken] -> Error ([RegexToken], Base)
parseBase (Chr c : toks) = return (toks, BaseChar c)
parseBase (LParen : toks) = do
  (toks', regex) <- parseRegex toks
  if checkNext toks' RParen
    then return (tail toks', BaseGroup regex)
    else Left $ "regex parentheses could not be parsed:\n\t" ++ show (LParen : toks)
parseBase t = Left $ "unexpected non-base token:" ++ show t

-- | Checks whether the next token in a stream of `RegexToken`s is the given `RegexToken`,
-- returning a boolean.
checkNext :: [RegexToken] -> RegexToken -> Bool
checkNext [] _ = False
checkNext (x : _) tok = x == tok

-- | Checks whether a `RegexToken` fulfills any of the given boolean functions.
isIn :: RegexToken -> [RegexToken -> Bool] -> Bool
isIn tok = any ($ tok)

-- | Takes a string and returns the complete `Regex` AST from it.
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

-- | Returns a state that represents the given integer.
stateFromInteger :: Integer -> State
stateFromInteger = State . show

-- | Converts a list of (Integer, Integer, `NFAData`) triplets into a list of
-- `NFATransition`s.
convertList :: [(Integer, Integer, NFAData a)] -> [NFATransition a]
convertList = (tupleToSimpleTransition . (\(i, i', a) -> (stateFromInteger i, stateFromInteger i', a)) <$>)

-- | Converts a `Regex` AST, when given a start state integer, into a start and end state
-- ID, and a list of `NFATransition`s.
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

-- | Converts a `Term` AST, when given a start state integer, into a start and end state
-- ID, and a list of `NFATransition`s.
termToNFA :: Term -> Integer -> (Integer, Integer, [NFATransition Char])
termToNFA TermNil i = (i, i, [])
termToNFA (TermFact fact term) i = (q0, q2, ts)
  where
    q0 = i
    (_, q1, xs) = factToNFA fact i
    (_, q2, xs') = termToNFA term q1
    ts = xs ++ xs'

-- | Converts a `Factor` AST, when given a start state integer, into a start and end state
-- ID, and a list of `NFATransition`s.
factToNFA :: Factor -> Integer -> (Integer, Integer, [NFATransition Char])
factToNFA (FactorStar fact) i = (q0, q1, ts)
  where
    (q0, q1, xs) = factToNFA fact i
    ts = xs ++ convertList [(q0, q1, Epsilon), (q1, q0, Epsilon)]
factToNFA (FactorSingle base) i = baseToNFA base i

-- | Converts a `Base` AST, when given a start state integer, into a start and end state
-- ID, and a list of `NFATransition`s.
baseToNFA :: Base -> Integer -> (Integer, Integer, [NFATransition Char])
baseToNFA (BaseChar c) i = (i, i + 1, convertList [(i, i + 1, Val c)])
baseToNFA (BaseGroup regex) s = regexToNFA regex s

-- | Converts a string into an `NFA` representing the regex in the string.
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

-- | Takes an input string and a regex string, and returns whether the input (exactly)
-- matches the regex.
checkString :: String -> String -> Error ReturnValue
checkString inpStr regex = do
  nfa <- regexStrToNFA regex
  extractResult $ runNFA inpStr (clock 0) nfa
