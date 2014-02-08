{-# LANGUAGE FlexibleContexts #-}

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
import Network.AWS.Generics.Query
import Network.AWS.Generics.XML
import Network.AWS.Internal.Serialisation
import Network.AWS.Internal.Types
import Network.HTTP.Conduit

-- v2Query :: ToQuery a => Service -> StdMethod -> ByteString -> a -> RawRequest
-- v2Query s@Service{..} m p x = RawRequest s s m p q [] (RequestBodyBS "")
--   where
--     q = map (second Just) $ encodeQuery x

-- v4Query :: ToQuery a => Service -> StdMethod -> ByteString -> a -> RawRequest
-- v4Query s m a q = v2Query s m "/" q .?. [("Action", Just a)]

-- v3httpsQuery :: AWSRequest a => Service -> StdMethod -> ByteString -> a -> RawRequest
-- v3httpsQuery = undefined

-- xml :: ToXML a => Service -> StdMethod -> ByteString -> a -> RawRequest
-- xml s@Service{..} m p = RawRequest s s m p [] [] . RequestBodyBS . toXML
-- --     , rqHeaders = [hdr (Content :: XML)]

-- (.?.) :: RawRequest s -> [QueryItem] -> RawRequest
-- (.?.) r q = r { rawQuery = rawQuery r ++ q }

-- (.:.) :: RawRequest s -> [Header] -> RawRequest
-- (.:.) r hs = r { rqHeaders = rqHeaders r ++ hs }

-- xml :: ToXML a => a -> RequestBody
-- xml = RequestBodyLBS . encodeXML

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

getRestJSON :: (ToPath a, ToJSON a, AWSRequest a)
            => Service
            -> a
            -> RawRequest
getRestJSON = undefined

postRestJSON :: (ToPath a, ToJSON a, AWSRequest a)
             => Service
             -> a
             -> RawRequest
postRestJSON = undefined

deleteRestJSON :: (ToPath a, ToJSON a, AWSRequest a)
               => Service
               -> a
               -> RawRequest
deleteRestJSON = undefined

headS3 :: (ToHeaders a, ToPath a, ToQuery a, AWSRequest a)
       => (ByteString -> Service)
       -> a
       -> RawRequest
headS3 = undefined

getS3 :: (ToHeaders a, ToPath a, ToQuery a, AWSRequest a)
      => (ByteString -> Service)
      -> a
      -> RawRequest
getS3 = undefined

postS3 :: (ToHeaders a, ToPath a, ToQuery a, AWSRequest a)
       => (ByteString -> Service)
       -> a
       -> RawRequest
postS3 = undefined

putS3 :: (ToHeaders a, ToPath a, ToQuery a, AWSRequest a)
      => (ByteString -> Service)
      -> a
      -> RawRequest
putS3 = undefined

deleteS3 :: (ToHeaders a, ToPath a, ToQuery a, AWSRequest a)
         => (ByteString -> Service)
         -> a
         -> RawRequest
deleteS3 = undefined

responseJSON :: (FromJSON (Er a), FromJSON (Rs a))
             => a
             -> Response (ResumableSource AWS ByteString)
             -> AWS (Either (Er a) (Rs a))
responseJSON = undefined
