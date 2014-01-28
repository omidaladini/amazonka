{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TupleSections        #-}

-- FIXME: review
{-# LANGUAGE UndecidableInstances #-}

-- Module      : Network.AWS.Internal.Serialisation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               Berkeley Software Distribution License, v. 3.0.
--               You can obtain it at
--               http://http://opensource.org/licenses/BSD-3-Clause.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.Serialisation where

import           Control.Error
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types                 (Parser)
import           Data.ByteString                  (ByteString)
import qualified Data.CaseInsensitive             as CI
import           Data.Tagged
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding               as Text
import           Data.Text.Helpers
import           Network.HTTP.Conduit
import           Network.HTTP.QueryString.Generic
import           Network.HTTP.Types               hiding (Query, toQuery)
import           Text.XML                         (Node)
import           Text.XML.Generic

(=?) :: ToQuery a => Text -> a -> Query
(=?) k = Pair k . toQuery

(=:) :: ToHeader a => ByteString -> a -> (HeaderName, Maybe ByteString)
(=:) = toHeader

hdr :: FromText a => HeaderName -> Response b -> Maybe a
hdr k = join
    . fmap (hush . fromText . Text.decodeUtf8)
    . lookup k
    . responseHeaders

fromTextQuery :: FromText a => Query -> Either String a
fromTextQuery = join . fmap fromText . fromQuery

toTextQuery :: ToText a => a -> Query
toTextQuery = toQuery . toText

fromTextXML :: FromText a => Tagged a XMLOptions -> [Node] -> Either String a
fromTextXML o = join . fmap fromText . fromXML (retag o)

toTextXML :: ToText a => Tagged a XMLOptions -> a -> [Node]
toTextXML o = toXML (retag o) . toText

fromTextJSON :: FromText a => String -> Value -> Parser a
fromTextJSON n = withText n (either fail return . fromText)

toTextJSON :: ToText a => a -> Value
toTextJSON = String . toText

class ToPath a where
    toPath :: a -> Text
    toPath = const Text.empty

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