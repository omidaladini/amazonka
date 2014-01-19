-- Module      : Network.AWS.Internal
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal
    (
    -- * Internal Modules
      module Internal

    -- * Convenience Modules
    , module Common
    ) where

-- import Network.AWS.Internal.Request    as Internal
import Network.AWS.Internal.Signing       as Internal
import Network.AWS.Internal.Types         as Internal
import Network.AWS.Internal.Types.Common  as Internal

import Data.Default                       as Common
import Data.Text.Helpers                  as Common
import Data.Time.Formatters               as Common
import GHC.Generics                       as Common
import Network.HTTP.QueryString.Instances as Common
import Network.HTTP.QueryString.Pickle    as Common
import Text.XML.Generic                   as Common
