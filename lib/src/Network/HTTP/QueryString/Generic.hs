-- Module      : Network.HTTP.QueryString.Generic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               Berkeley Software Distribution License, v. 3.0.
--               You can obtain it at
--               http://http://opensource.org/licenses/BSD-3-Clause.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.HTTP.QueryString.Generic where

import           Data.Default
import           Data.Monoid
import           Data.Text          (Text)
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import           Data.Text.Helpers

data Query
    = List [Query]
    | Pair Text Query
    | Value Text
      deriving (Eq, Show)

data QueryOptions = QueryOptions
    { ctorMod  :: String -> Text
    , fieldMod :: String -> Text
    }

instance Default QueryOptions

decode

class FromQuery a where

encode

class ToQuery a where


