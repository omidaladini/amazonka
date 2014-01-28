{-# LANGUAGE FlexibleContexts  #-}

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

import Data.Aeson
import Data.ByteString                    (ByteString)
import Data.Conduit
import Network.AWS.Internal.Serialisation
import Network.AWS.Internal.Types
import Network.HTTP.Conduit
import Network.HTTP.QueryString.Generic
import Text.XML.Generic

getRestXML :: (ToHeaders a, ToPath a, ToQuery a, AWSRequest a)
           => Service
           -> a
           -> RawRequest
getRestXML = undefined

postRestXML :: (ToHeaders a, ToPath a, ToQuery a, ToXML a, AWSRequest a)
            => Service
            -> a
            -> RawRequest
postRestXML = undefined

putRestXML :: (ToHeaders a, ToPath a, ToQuery a, ToXML a, AWSRequest a)
           => Service
           -> a
           -> RawRequest
putRestXML = undefined

deleteRestXML :: (ToHeaders a, ToPath a, ToQuery a, ToXML a, AWSRequest a)
            => Service
            -> a
            -> RawRequest
deleteRestXML = undefined

getQuery :: (ToQuery a, AWSRequest a)
         => Service
         -> ByteString
         -> a
         -> RawRequest
getQuery = undefined

getJSON :: (ToJSON a, AWSRequest a)
        => Service
        -> a
        -> RawRequest
getJSON = undefined

responseJSON :: (FromJSON (Er a), FromJSON (Rs a))
             => a
             -> Response (ResumableSource AWS ByteString)
             -> AWS (Either (Er a) (Rs a))
responseJSON = undefined


-- v2Query :: ToQuery a => Service -> StdMethod -> ByteString -> a -> RawRequest
-- v2Query s@Service{..} m p x = RawRequest s m p q [] (RequestBodyBS "")
--   where
--     q = map (second Just) $ encodeQuery x

-- v4Query :: ToQuery a => Service -> StdMethod -> ByteString -> a -> RawRequest
-- v4Query s m a q = v2Query s m "/" q .?. [("Action", Just a)]

-- v3httpsQuery :: AWSRequest a => Service -> StdMethod -> ByteString -> a -> RawRequest
-- v3httpsQuery = undefined

-- xml :: ToXML a => Service -> StdMethod -> ByteString -> a -> RawRequest
-- xml s@Service{..} m p = RawRequest s m p [] [] . RequestBodyBS . toXML
-- --     , rqHeaders = [hdr (Content :: XML)]

-- (.?.) :: RawRequest -> [QueryItem] -> RawRequest
-- (.?.) r q = r { rawQuery = rawQuery r ++ q }

-- (.:.) :: RawRequest -> [Header] -> RawRequest
-- (.:.) r hs = r { rqHeaders = rqHeaders r ++ hs }

-- s3GET :: (ByteString -> Service)
--       -> Text
--       -> Text
--       -> [AnyQuery]
--       -> [AnyHeader]
--       -> RawRequest
-- s3GET = undefined

-- xml :: ToXML a => a -> RequestBody
-- xml = RequestBodyLBS . encodeXML

-- xmlRs :: (FromXML (Er a), FromXML (Rs a))
--       => a
--       -> Response (ResumableSource AWS ByteString)
--       -> AWS (Either (Er a) (Rs a))
--     response _ rs = (responseBody rs $$+- Conduit.sinkLbs)
--         >>= f (statusIsSuccessful $ responseStatus rs)
--       where
--         f True  = fmap Right . awsEither . decodeXML
--         f False = fmap Left  . awsEither . decodeXML
