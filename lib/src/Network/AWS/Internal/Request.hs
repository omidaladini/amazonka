{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Network.AWS.Internal.Request
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.Request where

import Data.ByteString                  (ByteString)
import Network.AWS.Types
import Network.HTTP.Conduit
import Network.HTTP.QueryString.Generic
import Network.HTTP.Types               (StdMethod)
import Text.XML.Generic

v2Query :: ToQuery a => Service -> StdMethod -> ByteString -> a -> RawRequest
v2Query s@Service{..} m p x =
    RawRequest s m p (encodeQuery x) [] (RequestBodyBS "")

v4Query :: ToQuery a => Service -> StdMethod -> ByteString -> a -> RawRequest
v4Query s m a q = v2Query s m "/" q .?. [("Action",  a)]

-- xml :: ToXML a => Service -> StdMethod -> ByteString -> a -> RawRequest
-- xml s@Service{..} m p = RawRequest s m p [] [] . RequestBodyBS . toXML
-- --     , rqHeaders = [hdr (Content :: XML)]

(.?.) :: RawRequest -> [(ByteString, ByteString)] -> RawRequest
(.?.) r q = r { rawQuery = rawQuery r ++ q }

-- (.:.) :: RawRequest -> [Header] -> RawRequest
-- (.:.) r hs = r { rqHeaders = rqHeaders r ++ hs }
