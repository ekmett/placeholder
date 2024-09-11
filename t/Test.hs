{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

import Control.Exception
import Control.Monad (when)
import Control.Placeholder
import Data.List
import System.FilePath((</>))
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests" [
    basic
  ]

basic :: TestTree
basic = testGroup "Basic"  [
    testCase "todo" todoTest
  , testCase "unimplemented" unimplementedTest
  ]

todoTest :: IO ()
todoTest = do
  Left (ex :: TodoException) <- try (evaluate todo)
  let msg = show ex
  assertBool ("unexpected message: " ++ msg)
     $ "Control.Placeholder.todo: not yet implemented" `isPrefixOf` msg
  let has x = x `isInfixOf` msg
  assertBool ("unexpected HasCallStack format: " ++ msg) $
    all has [
      "CallStack (from HasCallStack):"
    , "todo, called at t" </> "Test.hs:28:47 in"
    ]
  when (has "CallStack (from -prof)") $ --enable-profiling enabled
    assertBool ("unexpected -prof stack format: " ++ msg) $
      all has [
        "Control.Placeholder.todo (src" </> "Control" </> "Placeholder.hs:120:1-66)"
      , "Main.todoTest (t" </> "Test.hs:(27,1)-(44,7))"
      , "Main.CAF (<entire-module>)"
      ]

unimplementedTest :: IO ()
unimplementedTest = do
  Left (ex :: UnimplementedException) <- try (evaluate unimplemented)
  let msg = show ex
  assertBool ("unexpected message: " ++ msg)
    $ "Control.Placeholder.unimplemented: unimplemented" `isPrefixOf` msg
