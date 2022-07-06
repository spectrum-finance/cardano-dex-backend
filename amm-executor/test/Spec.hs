module Main(main) where

import Test.Tasty
import Test.Tasty.HUnit
import Tests.Backlog.ServiceTest (checkBacklogService)

main :: IO ()
main = do
  defaultMain tests

tests = testGroup "ExecutorTests"
  [checkBacklogService]