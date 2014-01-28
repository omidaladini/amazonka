-- Module      : Network.AWS.Internal
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal
    (
    -- * Internal modules
      module Internal

    -- * Convenience exports
    , module Common

    , ToQuery(..)
    , FromQuery(..)
    , ToXML(..)
    , FromXML(..)
    ) where

import Network.AWS.Internal.Request       as Internal
import Network.AWS.Internal.Serialisation as Internal
import Network.AWS.Internal.Signing       as Internal
import Network.AWS.Internal.Types         as Internal
import Network.AWS.Internal.Types.Common  as Internal

import Data.Text.Helpers                  as Common
import Data.Time.Formatters               as Common

import Network.HTTP.QueryString.Generic
import Text.XML.Generic

-- FIXME:
-- remove no-warn-unused-imports from operations/types templates
-- and then re-export common types from here

