{-# LANGUAGE GADTs                #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE UndecidableInstances #-}

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

import           Control.Arrow
import           Control.Error
import           Control.Monad
import           Data.ByteString                  (ByteString)
import qualified Data.CaseInsensitive             as CI
import           Data.Text                        (Text)
import qualified Data.Text.Encoding               as Text
import           Data.Text.Helpers
import           Data.Time
import           Data.Time.Formatters
import           Network.AWS.Internal.Types
import           Network.HTTP.Conduit
import           Network.HTTP.QueryString.Generic
import           Network.HTTP.Types               hiding (Query, toQuery)
import           Text.XML.Generic

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

(=?) :: ToQuery a => Text -> a -> Query
(=?) k = Pair k . toQuery

(=:) :: ToHeader a => ByteString -> a -> (HeaderName, Maybe ByteString)
(=:) = toHeader

hdr :: FromText a => HeaderName -> Response b -> Maybe a
hdr k = join
    . fmap (hush . fromText . Text.decodeUtf8)
    . lookup k
    . responseHeaders

class ToPath a where
    toPath :: a -> Text
    toPath = const ""

class ToHeaders a where
    toHeaders :: a -> [(HeaderName, Maybe ByteString)]
    toHeaders = const []

class ToHeader a where
    toHeader :: ByteString -> a -> (HeaderName, Maybe ByteString)

instance ToHeader ByteString where
    toHeader k = (CI.mk k,) . Just

instance ToText a => ToHeader a where
    toHeader k = (CI.mk k,) . Just . Text.encodeUtf8 . toText

instance ToHeader a => ToHeader (Maybe a) where
    toHeader k (Just x) = toHeader k x
    toHeader k Nothing  = (CI.mk k, Nothing)
