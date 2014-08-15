-- Module      : Network.AWS.Request.RestS3
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Request.RestS3
    ( get
    , Network.AWS.Request.Common.head
    , delete
    , put
    , post
    ) where

import Control.Lens
import Network.AWS.Data
import Network.AWS.Request.Common
import Network.AWS.Types
import Network.HTTP.Types.Method

put :: (ToPath a, ToQuery a, ToHeaders a, ToBody a) => a -> Request a
put x = get x & rqMethod .~ PUT & rqBody .~ toBody x

post :: (ToPath a, ToQuery a, ToHeaders a, ToBody a) => a -> Request a
post = put