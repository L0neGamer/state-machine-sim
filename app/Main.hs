module Main where

import Data.Functor ((<&>))
import Data.StateMachines.Convert (convertDFAToNFA)
import Data.StateMachines.DFA (DFA)
import Data.StateMachines.Diagrams (drawStateMachineTo)
import Data.StateMachines.Examples
  ( busyBeaver3State,
    busyBeaver4State,
    busyBeaver5State,
    busyBeaverCheck,
    emptyDFA,
    exampleDFA,
    runExampleDFA,
  )
import Data.StateMachines.Internal (Error)
import Data.StateMachines.Regex (checkString, regexStrToNFA)
import Data.StateMachines.RunStateMachine (extractResult)
import Data.StateMachines.StateMachine (name)
import System.Directory.Extra (getCurrentDirectory)

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
  print $ exampleDFA <&> convertDFAToNFA
  mapM_ test exampleDFA
  mapM_ test (regexStrToNFA "hel*o the(re|ba)*")
  mapM_ (\(_, _, sm) -> test sm) busyBeaver3State
  mapM_ (\(_, _, sm) -> test sm) busyBeaver5State
  print "main_end"
  where
    test sm = do
      curDir <- getCurrentDirectory
      drawStateMachineTo (curDir ++ "/" ++ name sm ++ ".svg") 500 sm
