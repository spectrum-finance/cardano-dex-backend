module Resolver
  ( runApp
  ) where

import RIO.List (headMaybe)

import Control.Monad.IO.Unlift      (UnliftIO(..))
import Control.Monad.Trans.Resource (runResourceT)

import           Resolver.Settings  (loadAppSettings)
import qualified Resolver.AppWiring as AppWiring

runApp :: [String] -> IO ()
runApp args = do
  settings <- loadAppSettings $ headMaybe args
  runResourceT $ AppWiring.mkApp (UnliftIO id) settings
