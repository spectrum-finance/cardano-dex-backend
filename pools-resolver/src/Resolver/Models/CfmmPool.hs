module Resolver.Models.CfmmPool
    ( ConfirmedPool(..)
    , PredictedPool(..)
    ) where

import RIO

newtype ConfirmedPool a = ConfirmedPool 
    { confirmed :: a } deriving (Show)

newtype PredictedPool a = PredictedPool 
    { predicted :: a } deriving (Show)