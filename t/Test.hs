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
        assertBool "todo(1)" $ all (flip isInfixOf msg) [
               "Control.Placeholder.todo: not yet implemented"
             , "test/Test.hs:26:29"
             ]
    _ -> assertBool "todo (1)" False
