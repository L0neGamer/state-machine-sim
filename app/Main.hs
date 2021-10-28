module Main where

-- import NFA
import DFA (DFA)
import Examples (busyBeaver5State, busyBeaverCheck, emptyDFA, exampleDFA, runExampleDFA)
import Lib (Error)

main :: IO ()
main = do
  print exampleDFA
  print (emptyDFA :: Error (DFA Int))
  print $ runExampleDFA [1, 0, 1, 1, 1, 1]
  print $ runExampleDFA [1, 0, 1, 1, 1, 1, 0]
  print $ busyBeaver5State >>= busyBeaverCheck
  -- print $ convertDFAToNFA exampleDFA
  -- print $ runSM
  -- print $ run [1, 0, 1, 1, 1, 0] (Infinite 0) (convertDFAToNFA exampleDFA)
  -- print $ regexStrToNFA testRegexStr
  -- print $ busyBeaverCheck busyBeaver4State
  -- print $ busyBeaverCheck busyBeaver5State
  print "main_end"
