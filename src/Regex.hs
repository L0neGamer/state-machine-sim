{-# LANGUAGE OverloadedStrings, RecordWildCards, InstanceSigs #-}
module Regex
 where

-- thanks to this page for help in this file: http://matt.might.net/articles/parsing-regex-with-recursive-descent/

import Lib

import qualified Data.Map as M
import qualified Data.Set as S
import qualified NFA
import qualified Lib (ReturnState)

import Debug.Trace (trace)

type Err = String

data RegexToken = LParen | RParen | Chr Char | KleeneStar | Alternation deriving (Show, Eq)

type TokenStream = [RegexToken]

-- data RegexFormat = Val Char | Group [RegexFormat] | KleeneVal RegexFormat | AlternationVal RegexFormat RegexFormat

data Base = BaseChar Char | BaseGroup Regex deriving (Show, Eq)
data Factor = FactorSingle Base | FactorStar Factor deriving (Show, Eq)
data Term = TermNil | TermFact Factor Term deriving (Show, Eq)
data Regex = RegexSingle Term | RegexAlternation Term Regex deriving (Show, Eq)

isChr (Chr _) = True
isChr _ = False

baseSet = [isChr, (==LParen)]
factorSet = baseSet
termSet = factorSet
regexSet = (==Alternation) : termSet

charToToken :: Char -> RegexToken
charToToken '*' = KleeneStar
charToToken '(' = LParen
charToToken ')' = RParen
charToToken '|' = Alternation
charToToken x = Chr x

toTokens :: String -> TokenStream
toTokens [] = []
toTokens ('\\':x:xs) = Chr x : toTokens xs
toTokens (x:xs) = charToToken x : toTokens xs

parseRegex :: TokenStream -> (TokenStream, Regex)
parseRegex toks
    | checkNext toks' Alternation = (toks'', RegexAlternation term regex)
    | otherwise = (toks', RegexSingle term)
    where (toks', term) = parseTerm toks
          (toks'', regex) = parseRegex (tail toks')

parseTerm :: TokenStream -> (TokenStream, Term)
parseTerm [] = ([], TermNil)
parseTerm (tok:toks)
    | tok `isIn` factorSet = (toks'', TermFact fact fact')
    | otherwise = (tok:toks, TermNil)
    where (toks', fact) = parseFactor (tok:toks)
          (toks'', fact') = parseTerm toks'

parseFactor :: TokenStream -> (TokenStream, Factor)
parseFactor toks = (toks'', fact)
    where (toks', base) = parseBase toks
          (toks'', fact) = parseFactor' toks' (FactorSingle base)

parseFactor' :: TokenStream -> Factor -> (TokenStream, Factor)
parseFactor' (KleeneStar:toks) fact = parseFactor' toks (FactorStar fact)
parseFactor' toks fact = (toks, fact)

parseBase :: TokenStream -> (TokenStream, Base)
parseBase (Chr c:toks) = (toks, BaseChar c) 
parseBase (LParen:toks)
    | checkNext toks' RParen = (tail toks', BaseGroup regex)
    where (toks', regex) = parseRegex toks

checkNext :: TokenStream -> RegexToken -> Bool
checkNext [] _ = False
checkNext (x:_) tok = x == tok

isIn :: RegexToken -> [RegexToken -> Bool] -> Bool
isIn tok = any ($ tok)

strToParsedRegex :: String -> Regex
strToParsedRegex str
    | null toks = regex
    | otherwise = error $ "regex was not parsed correctly:\n\tLeftover:"  ++ show toks ++ "\n\tOriginal:" ++ show str
    where (toks, regex) = parseRegex . toTokens $ str

regexToNFA :: Regex -> Integer -> (State, State, [(State, NFA.NFATransitionType Char, State)])
regexToNFA (RegexSingle term) i = termToNFA term i
regexToNFA (RegexAlternation term regex) i = (q0, q3, transitions)
    where q0 = IdI i
          (q1, IdI i', xs) = termToNFA term (i+1)
          (q2, IdI i'', xs') = regexToNFA regex (i'+1)
          q3 = IdI (i''+1)
          transitions = xs ++ xs' ++ [(q0, NFA.Epsilon, q1), (q0, NFA.Epsilon, q2), (IdI i', NFA.Epsilon, q3), (IdI i'', NFA.Epsilon, q3)]

termToNFA :: Term -> Integer -> (State, State, [(State, NFA.NFATransitionType Char, State)])
termToNFA TermNil i = (IdI i, IdI i, [])
termToNFA (TermFact fact term) i = (q0, q2, transitions)
    where (q0, IdI i', xs) = factToNFA fact i
          (q1, q2, xs') = termToNFA term i'
          transitions = xs ++ xs'

factToNFA :: Factor -> Integer -> (State, State, [(State, NFA.NFATransitionType Char, State)])
factToNFA (FactorStar fact) i = (q0, q1, transitions)
    where (q0, q1, xs) = factToNFA fact i
          transitions = xs ++ [(q0, NFA.Epsilon, q1), (q1, NFA.Epsilon, q0)]
factToNFA (FactorSingle base) i = baseToNFA base i

baseToNFA :: Base -> Integer -> (State, State, [(State, NFA.NFATransitionType Char, State)])
baseToNFA (BaseChar c) i = (IdI i, IdI (i+1), [(IdI i, NFA.Val c, IdI (i+1))])
baseToNFA (BaseGroup regex) i = regexToNFA regex i

getMetaData :: [(State, NFA.NFATransitionType Char, State)] -> (S.Set State, S.Set Char)
getMetaData [] = (S.empty, S.empty)
getMetaData ((q0, NFA.Epsilon, q1):xs) = (S.insert q1 (S.insert q0 states), lang)
    where (states, lang) = getMetaData xs
getMetaData ((q0, NFA.Val c, q1):xs) = (S.insert q1 (S.insert q0 states), S.insert c lang)
    where (states, lang) = getMetaData xs

regexStrToNFA :: String -> NFA.NFAStateMachine Char
regexStrToNFA str = NFA.simpleAddNFATransitions xs emptyMachine
    where (start, end, xs) = regexToNFA (strToParsedRegex str) 0
          (states, lang) = getMetaData xs
          emptyMachine = NFA.NFAStatMac start (S.singleton end) states lang M.empty

testRegexStr = "ab(aab)*bb"

parsedRegex = strToParsedRegex testRegexStr

checkString :: String -> String -> ReturnState
checkString inpStr regex = Lib.run inpStr Infinite (regexStrToNFA regex)