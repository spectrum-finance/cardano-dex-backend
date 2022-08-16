module Main where

import Tracker.App
import RIO

main :: IO ()
main = do
  app <- mkApp
  runApp app