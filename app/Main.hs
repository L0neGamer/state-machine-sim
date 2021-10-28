module Main where

import DFA (DFA)
import Examples (busyBeaver3State, busyBeaver4State, busyBeaver5State, busyBeaverCheck, emptyDFA, exampleDFA, runExampleDFA)
import Lib (Error)
import Regex (checkString, regexStrToNFA)
import RunStateMachine (extractResult)

main :: IO ()
main = do
  print exampleDFA
  print (emptyDFA :: Error (DFA Int))
  print $ extractResult $ runExampleDFA [1, 0, 1, 1, 1, 1] -- fails
  print $ extractResult $ runExampleDFA [1, 0, 1, 1, 1, 1, 0] -- succeeds
  print $ regexStrToNFA "hel*o the(re|ba)*"
  print $ checkString "hello therebarereba" "hel*o the(re|ba)*" -- succeeds
  -- the first two values for all of these should be 0
  print $ busyBeaver3State >>= busyBeaverCheck
  print $ busyBeaver4State >>= busyBeaverCheck
  -- print $ busyBeaver5State >>= busyBeaverCheck
  -- print $ convertDFAToNFA exampleDFA
  -- print $ runSM
  -- print $ run [1, 0, 1, 1, 1, 0] (Infinite 0) (convertDFAToNFA exampleDFA)
  -- print $ regexStrToNFA testRegexStr
  -- print $ busyBeaverCheck busyBeaver4State
  -- print $ busyBeaverCheck busyBeaver5State
  print "main_end"
