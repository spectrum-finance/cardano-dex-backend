module Main where

import RIO
import Executor.Wirings.WiringApp
import Executor.Services.BashService

main :: IO ()
main = mkScriptAddress'
