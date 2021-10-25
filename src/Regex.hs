module Regex where

-- thanks to this page for help in this file: http://matt.might.net/articles/parsing-regex-with-recursive-descent/

import qualified Data.Map as M
import qualified Data.Set as S
import Lib
import qualified NFA

type Err = String

data RegexToken = LParen | RParen | Chr Char | KleeneStar | Alternation deriving (Show, Eq)

type TokenStream = [RegexToken]

data Base = BaseChar Char | BaseGroup Regex deriving (Show, Eq)

data Factor = FactorSingle Base | FactorStar Factor deriving (Show, Eq)

data Term = TermNil | TermFact Factor Term deriving (Show, Eq)

data Regex = RegexSingle Term | RegexAlternation Term Regex deriving (Show, Eq)

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
parseTerm (tok : toks)
  | tok `isIn` baseChecks = (toks'', TermFact fact fact')
  | otherwise = (tok : toks, TermNil)
  where
    baseChecks = [isChr, (== LParen)]
    (toks', fact) = parseFactor (tok : toks)
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

regexToNFA :: Regex -> Integer -> (State, State, [(State, State, NFA.NFATransitionType Char)])
regexToNFA (RegexSingle term) i = termToNFA term i
regexToNFA (RegexAlternation term regex) i = (q0, q3, transitions)
  where
    q0 = IdI i
    (q1, IdI i', xs) = termToNFA term (i + 1)
    (q2, IdI i'', xs') = regexToNFA regex (i' + 1)
    q3 = IdI (i'' + 1)
    transitions = xs ++ xs' ++ [(q0, q1, NFA.Epsilon), (q0, q2, NFA.Epsilon), (IdI i', q3, NFA.Epsilon), (IdI i'', q3, NFA.Epsilon)]

termToNFA :: Term -> Integer -> (State, State, [(State, State, NFA.NFATransitionType Char)])
termToNFA TermNil i = (IdI i, IdI i, [])
termToNFA (TermFact fact term) i = (q0, q2, transitions)
  where
    (q0, IdI i', xs) = factToNFA fact i
    (_, q2, xs') = termToNFA term i'
    transitions = xs ++ xs'

factToNFA :: Factor -> Integer -> (State, State, [(State, State, NFA.NFATransitionType Char)])
factToNFA (FactorStar fact) i = (q0, q1, transitions)
  where
    (q0, q1, xs) = factToNFA fact i
    transitions = xs ++ [(q0, q1, NFA.Epsilon), (q1, q0, NFA.Epsilon)]
factToNFA (FactorSingle base) i = baseToNFA base i

baseToNFA :: Base -> Integer -> (State, State, [(State, State, NFA.NFATransitionType Char)])
baseToNFA (BaseChar c) i = (IdI i, IdI (i + 1), [(IdI i, IdI (i + 1), NFA.Val c)])
baseToNFA (BaseGroup regex) i = regexToNFA regex i

regexStrToNFA :: String -> NFA.NFAStateMachine Char
regexStrToNFA str = foldr (NFA.addNFATransition . (\(a, b, c) -> (a, S.singleton b, c))) emptyMachine xs
  where
    (start, end, xs) = regexToNFA (strToParsedRegex str) 0
    (states, lang) = getStatesAndLang xs
    emptyMachine = NFA.NFAStatMac states lang M.empty start (S.singleton end)

testRegexStr :: String
testRegexStr = "ab(aab)*bb"

checkString :: String -> String -> ReturnValue
checkString inpStr regex = run inpStr (Infinite 0) (regexStrToNFA regex)
