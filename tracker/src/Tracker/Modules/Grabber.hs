module Tracker.Modules.Grabber

import Dex.Models
im
import RIO

data Grabber env = Grabber {
	getParsedOperation :: RIO env [ParsedOperation],
	getPools :: RIO env [Pool]
}

getParsedOperation' ExplorerClient -> TVar Int -> RIO env [ParsedOperation]