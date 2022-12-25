module Main(main) where

import Test.Tasty 
  ( defaultMain, testGroup )
import Test.Tasty.HUnit
import Tests.Backlog.ServiceTest 
  ( checkBacklogService )
import Tests.OrdersExecutor.ProcessTest

main :: IO ()
main = do
  defaultMain tests

tests = testGroup "ExecutorTests"
  [ checkOrdersExecutor
  , checkBacklogService
  ]
