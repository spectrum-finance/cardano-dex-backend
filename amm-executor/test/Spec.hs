module Main(main) where

import Test.Tasty 
  ( defaultMain, testGroup )
import Test.Tasty.HUnit
import Tests.Backlog.ServiceTest 
  ( checkBacklogService )

main :: IO ()
main = do
  defaultMain tests

tests = testGroup "ExecutorTests"
  [checkBacklogService]