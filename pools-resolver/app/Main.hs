module Main where

import Resolver
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  runApp args
