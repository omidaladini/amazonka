-- Module      : Network.AWS.Core
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Core
    ( module Internal

    , FromJSON (..)
    , ToJSON   (..)
    , Options  (..)
    , defaultOptions

    , ByteString
    , HashMap
    , NonEmpty (..)
    , Tagged   (..)
    , Text
    , UTCTime
    , Generic
    , Service  (..)
    ) where

import Data.Aeson                 (FromJSON(..), ToJSON(..))
import Data.Aeson.Types           (Options(..), defaultOptions)
import Data.ByteString            (ByteString)
import Data.Default               (def)
import Data.HashMap.Strict        (HashMap)
import Data.Hashable              (Hashable(..))
import Data.List.NonEmpty         (NonEmpty(..))
import Data.Tagged                (Tagged(..))
import Data.Text                  (Text)
import Data.Time                  (UTCTime)
import GHC.Generics               (Generic)
import Network.AWS.Internal       as Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.Internal.Types (Service(..))

