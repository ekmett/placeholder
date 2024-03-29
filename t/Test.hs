{-# OPTIONS_GHC -Wno-deprecations #-}

import Test.Tasty
import Test.Tasty.HUnit

import Control.Exception
import Control.Placeholder

import Data.List

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup " All tests" [
    basic
  ]

basic :: TestTree
basic = testGroup "Basic"  [
    testCase "todo" $ todoTest
  ]

todoTest :: IO ()
todoTest = do
  result <- try (evaluate $ todo) :: IO (Either TodoException Int)
  case result of
    Left ex -> do
        let msg = show ex
        assertBool ("unexpected exception message format: " ++ msg) $
           all (flip isInfixOf msg) [ -- --enable-profiling disabled
               "Control.Placeholder.todo: not yet implemented"
             , "test/Test.hs:26:29"
             ] ||
           all (flip isInfixOf msg) [ -- --enable-profiling enabled
               "Control.Placeholder.todo: not yet implemented",
               "CallStack (from HasCallStack):",
                  "todo, called at t/Test.hs:26:29 in main:Main",
               "CallStack (from -prof):",
                  "Control.Placeholder.todo (src/Control/Placeholder.hs:116:1-66)",
                  "Main.todoTest (t/Test.hs:(25,1)-(45,36))",
                  "Main.CAF (<entire-module>)"
             ]
    _ -> assertBool "todo (1)" False
