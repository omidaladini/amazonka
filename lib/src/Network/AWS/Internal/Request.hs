{-# LANGUAGE FlexibleContexts  #-}
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

import Control.Applicative
import Data.Aeson
import Data.Foldable              (Foldable, foldl')
import Data.Text                  (Text)
import Network.AWS.Generics.Query
import Network.AWS.Headers
import Network.AWS.Internal.Types
import Network.HTTP.Conduit       hiding (rawBody)
import Network.HTTP.Types

getQuery :: (ToQuery a, AWSRequest a)
         => Service
         -> Text
         -> a
         -> RawRequest
getQuery s a x = setMethod GET
   . addQuery "Action" a
   $ setMany (uncurry addQuery) (encodeQuery x) (mk s)

postJSON :: (ToJSON a, AWSRequest a)
         => Service
         -> Text
         -> a
         -> RawRequest
postJSON s a = setMethod POST
    . addQuery "Action" a
    . setBody (mk s)
    . RequestBodyLBS
    . encode

addHeader :: Header -> RawRequest -> RawRequest
addHeader h rq = rq { rawHeaders = h : rawHeaders rq }

addQuery :: Text -> Text -> RawRequest -> RawRequest
addQuery k v rq = rq { rawQuery = (k, Just v) : rawQuery rq }

setMethod :: StdMethod -> RawRequest -> RawRequest
setMethod m rq = rq { rawMethod = m }

setBody :: RawRequest -> RequestBody -> RawRequest
setBody rq b = rq { rawBody = b }

setMany :: Foldable t => (b -> a -> a) -> t b -> a -> a
setMany f xs rq = foldl' (\x y -> f y x) rq xs

mk :: Service -> RawRequest
mk s@Service{..} = RawRequest s GET "/" [] hs (RequestBodyBS "")
  where
    hs = maybe [] (:[]) $ hAMZTarget <$> svcTarget

-- xml :: ToXML a => Service -> StdMethod -> ByteString -> a -> RawRequest
-- xml s@Service{..} m p = RawRequest s s m p [] [] . RequestBodyBS . toXML
-- --     , rqHeaders = [hdr (Content :: XML)]

-- xml :: ToXML a => a -> RequestBody
-- xml = RequestBodyLBS . encodeXML

-- getRestXML :: (ToHeaders a, ToPath a, ToQuery a, AWSRequest a)
--            => Service
--            -> a
--            -> RawRequest
-- getRestXML = undefined

-- postRestXML :: (ToHeaders a, ToPath a, ToQuery a, ToXML a, AWSRequest a)
--             => Service
--             -> a
--             -> RawRequest
-- postRestXML = undefined

-- putRestXML :: (ToHeaders a, ToPath a, ToQuery a, ToXML a, AWSRequest a)
--            => Service
--            -> a
--            -> RawRequest
-- putRestXML = undefined

-- deleteRestXML :: (ToHeaders a, ToPath a, ToQuery a, ToXML a, AWSRequest a)
--             => Service
--             -> a
--             -> RawRequest
-- deleteRestXML = undefined

-- getRestJSON :: (ToPath a, ToJSON a, AWSRequest a)
--             => Service
--             -> a
--             -> RawRequest
-- getRestJSON = undefined

-- postRestJSON :: (ToPath a, ToJSON a, AWSRequest a)
--              => Service
--              -> a
--              -> RawRequest
-- postRestJSON = undefined

-- deleteRestJSON :: (ToPath a, ToJSON a, AWSRequest a)
--                => Service
--                -> a
--                -> RawRequest
-- deleteRestJSON = undefined

-- headS3 :: (ToHeaders a, ToPath a, ToQuery a, AWSRequest a)
--        => (ByteString -> Service)
--        -> a
--        -> RawRequest
-- headS3 = undefined

-- getS3 :: (ToHeaders a, ToPath a, ToQuery a, AWSRequest a)
--       => (ByteString -> Service)
--       -> a
--       -> RawRequest
-- getS3 = undefined

-- postS3 :: (ToHeaders a, ToPath a, ToQuery a, AWSRequest a)
--        => (ByteString -> Service)
--        -> a
--        -> RawRequest
-- postS3 = undefined

-- putS3 :: (ToHeaders a, ToPath a, ToQuery a, AWSRequest a)
--       => (ByteString -> Service)
--       -> a
--       -> RawRequest
-- putS3 = undefined

-- deleteS3 :: (ToHeaders a, ToPath a, ToQuery a, AWSRequest a)
--          => (ByteString -> Service)
--          -> a
--          -> RawRequest
-- deleteS3 = undefined

-- responseJSON :: (FromJSON (Er a), FromJSON (Rs a))
--              => a
--              -> Response (ResumableSource AWS ByteString)
--              -> AWS (Either (Er a) (Rs a))
-- responseJSON = undefined
