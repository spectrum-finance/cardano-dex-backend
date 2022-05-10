module Resolver
  ( runApp
  ) where

import RIO.List
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Resource (runResourceT)

import           Resolver.Settings  (loadAppSettings)
import qualified Resolver.AppWiring as AppWiring

runApp :: [String] -> IO ()
runApp args = do
  settings <- loadAppSettings $ headMaybe args
  app      <- runResourceT $ AppWiring.mkApp (UnliftIO id) settings
  AppWiring.runApp app
