module Main where

import Wallet.Helper
  ( runApp )
import System.Environment
  ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  runApp args