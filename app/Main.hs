module Main where

import Lib
import DFA
import NFA
import Convert
import Regex

main :: IO ()
main = do
    print exampleDFA
    print $ convertDFAToNFA exampleDFA
    print $ run [1,0,1,1,1,0] (Infinite 0) (convertDFAToNFA exampleDFA)
    print $ regexStrToNFA testRegexStr
    print "main_end"
