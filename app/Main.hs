{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Main where

import Data.Functor ((<&>))
import Data.StateMachines.Convert (convertDFAToNFA, convertNFAToDFA, reverseNFA)
import Data.StateMachines.DFA (DFA, runDFA)
import Data.StateMachines.Diagrams (drawStateMachineTo)
import Data.StateMachines.Examples
  ( busyBeaver3State,
    busyBeaver4State,
    busyBeaver5State,
    busyBeaverCheck,
    emptyDFA,
    exampleDFA,
    helloThereRegexNFA,
    runExampleDFA,
  )
import Data.StateMachines.Internal (Const (..), Error)
import Data.StateMachines.NFA (runNFA)
import Data.StateMachines.Regex (checkString, regexStrToNFA)
import Data.StateMachines.RunStateMachine (clock, extractResult)
import Data.StateMachines.StateMachine (name)
import Data.StateMachines.TuringMachine (blankTape, runTuringMachine)
import System.Directory (getCurrentDirectory)

main :: IO ()
main = do
  print exampleDFA
  print (emptyDFA :: Error (DFA Int))
  print $ extractResult <$> runExampleDFA [1, 0, 1, 1, 1, 1] -- fails
  print $ extractResult <$> runExampleDFA [1, 0, 1, 1, 1, 1, 0] -- succeeds
  print helloThereRegexNFA
  print $ checkString "hello therebarereba" "hel*o the(re|ba)*" -- succeeds
  print $ (helloThereRegexNFA >>= reverseNFA) <&> runNFA "ababereht ollleh" (clock 100)
  print $ (helloThereRegexNFA >>= reverseNFA >>= convertNFAToDFA) <&> runDFA "ababereht ollleh" (clock 100)
  -- the first two values for all of these should be 0
  print $ busyBeaver3State <&> busyBeaverCheck
  print $ busyBeaver4State <&> busyBeaverCheck
  -- print $ busyBeaver5State <&> busyBeaverCheck -- WARNING - takes a long time!!
  -- print $ busyBeaver5State >>= \bb -> return $ runTuringMachine (blankTape (Const 0)) (clock 0) (thd bb)
  print $ exampleDFA <&> convertDFAToNFA
  mapM_ test exampleDFA
  mapM_ test (regexStrToNFA "hel*o the(re|ba)*")
  mapM_ (\(_, _, sm) -> test sm) busyBeaver3State
  mapM_ (\(_, _, sm) -> test sm) busyBeaver4State
  mapM_ (\(_, _, sm) -> test sm) busyBeaver5State
  mapM_ test helloThereRegexNFA
  mapM_ test (helloThereRegexNFA >>= convertNFAToDFA)
  mapM_ test (helloThereRegexNFA >>= reverseNFA)
  mapM_ test (helloThereRegexNFA >>= reverseNFA >>= convertNFAToDFA)
  print "main_end"
  where
    adjustFilename = fmap (\c -> if c `notElem` (['a' .. 'z'] ++ ['A' .. 'Z'] ++ "0123456789 ") then '~' else c)
    test sm = do
      curDir <- getCurrentDirectory
      drawStateMachineTo (curDir ++ "/svgs/" ++ adjustFilename (name sm) ++ ".svg") 500 sm
    thd (_, _, c) = c
