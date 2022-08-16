module Main where

import Spectrum.Executor
  ( runApp )
import System.Environment
  ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  runApp args
