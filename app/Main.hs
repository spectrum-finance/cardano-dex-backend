module Main where

import Dex.GetBlock
import Dex.Models.Rosetta.Response.BlockResponse
import RIO
import RIO.Text as T

main :: IO ()
main = do
    res <- getBlock 1 -- 2646191
    print res
