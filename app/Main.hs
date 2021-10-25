module Main where

-- import NFA
import Convert
import DFA
import Lib
import Regex
import TuringMachine

main :: IO ()
main = do
  print exampleDFA
  print $ convertDFAToNFA exampleDFA
  print $ run [1, 0, 1, 1, 1, 0] (Infinite 0) (convertDFAToNFA exampleDFA)
  print $ regexStrToNFA testRegexStr
  -- print $ busyBeaverCheck busyBeaver4State
  print $ busyBeaverCheck busyBeaver5State
  print "main_end"
