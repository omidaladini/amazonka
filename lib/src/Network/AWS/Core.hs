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
    ( FromJSON (..)
    , ToJSON   (..)
    , Options  (..)
    , ByteString
    , ResumableSource
    , Hashable (..)
    , HashMap
    , NonEmpty (..)
    , Tagged   (..)
    , Text
    , UTCTime
    , Generic
    , ToQuery  (..)
    , RequestBody

    , defaultOptions
    , def
    , queryFromList

    , module Export
    ) where

import Data.Aeson                 (FromJSON(..), ToJSON(..))
import Data.Aeson.Types           (Options(..), defaultOptions)
import Data.ByteString            (ByteString)
import Data.Conduit               (ResumableSource)
import Data.Default               (def)
import Data.HashMap.Strict        (HashMap)
import Data.Hashable              (Hashable(..))
import Data.List.NonEmpty         (NonEmpty(..))
import Data.Tagged                (Tagged(..))
import Data.Text                  (Text)
import Data.Time                  (UTCTime)
import GHC.Generics               (Generic)
import Network.AWS.Generics.Query (ToQuery(..), queryFromList)
import Network.HTTP.Conduit       (RequestBody)

import Control.Monad              as Export
import Data.Monoid                as Export
import Network.AWS.Generics.XML   as Export
import Network.AWS.Internal       as Export hiding (AvailabilityZone, Region)
import Network.AWS.Text           as Export

