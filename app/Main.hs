module Main where

import Convert (convertDFAToNFA)
import DFA (DFA)
import Data.Functor ((<&>))
import Examples
  ( busyBeaver3State,
    busyBeaver4State,
    busyBeaverCheck,
    emptyDFA,
    exampleDFA,
    runExampleDFA,
  )
import Lib (Error)
import Regex (checkString, regexStrToNFA)
import RunStateMachine (extractResult)

main :: IO ()
main = do
  print exampleDFA
  print (emptyDFA :: Error (DFA Int))
  print $ extractResult <$> runExampleDFA [1, 0, 1, 1, 1, 1] -- fails
  print $ extractResult <$> runExampleDFA [1, 0, 1, 1, 1, 1, 0] -- succeeds
  print $ regexStrToNFA "hel*o the(re|ba)*"
  print $ checkString "hello therebarereba" "hel*o the(re|ba)*" -- succeeds
  -- the first two values for all of these should be 0
  print $ busyBeaver3State <&> busyBeaverCheck
  print $ busyBeaver4State <&> busyBeaverCheck
  -- print $ busyBeaver5State <&> busyBeaverCheck -- WARNING - takes a long time!!
  print $ exampleDFA >>= convertDFAToNFA
  print "main_end"
