module Dex.Contract.Integration ( proxyDatumBS ) where

import Data.ByteString.Char8
import qualified Data.ByteString.Char8  as C

proxyDatumBS :: ByteString
proxyDatumBS = C.pack "lazyByteStrin"