module Tracker.Programs.TrackerProgram where

import System.IO
import Tracker.Modules.Grabber
import Control.Concurrent.ParallelIO.Global
import qualified Streamly.Prelude as S
import Tracker.Modules.Publisher
import RIO
import Prelude

data TrackerProgram = TrackerProgram { run :: IO () }

mkTrackerProgram :: Grabber -> Publisher -> IO (TrackerProgram)
mkTrackerProgram grabber publisher = pure $ TrackerProgram (run' grabber publisher)

run' :: Grabber -> Publisher -> IO ()
run' grabber publisher = parallel_ [publishStream grabber publisher, startGrabber grabber]

publishStream :: Grabber -> Publisher -> IO ()
publishStream grabber publisher =
  liftIO $ S.repeatM (threadDelay 1000000) >> (liftIO $ getTxOutsAndPublish grabber publisher) & S.drain

getTxOutsAndPublish :: Grabber -> Publisher -> IO ()
getTxOutsAndPublish Grabber{..} Publisher{..} = do
  newPools <- getPools
  newOps <- getParsedOperation
  publishPool newPools
  publishParsedOp newOps