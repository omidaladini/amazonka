{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TupleSections        #-}

-- FIXME: review
{-# LANGUAGE OverlappingInstances #-}
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

import           Control.Applicative
import           Control.Arrow
import           Control.Error
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types           (Parser)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.CaseInsensitive       as CI
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.Hashable
import           Data.Tagged
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           Network.AWS.Generics.Query
import           Network.AWS.Generics.XML
import           Network.AWS.Text
import           Network.HTTP.Types         hiding (Query, toQuery)
import           Text.XML                   (Node)
import           Text.XML.Cursor

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

(=?) :: ToQuery a => Text -> a -> Query
(=?) k = Pair k . toQuery

(=:) :: ToHeader a => ByteString -> a -> (HeaderName, Maybe ByteString)
(=:) = toHeader

hdr :: (Applicative f, FromText a)
    => HeaderName
    -> HashMap HeaderName ByteString
    -> f (Maybe a)
hdr k = pure
    . join
    . fmap (hush . fromText . Text.decodeUtf8)
    . Map.lookup k

hdrs :: Applicative f
     => ByteString
     -> HashMap HeaderName ByteString
     -> f (HashMap Text Text)
hdrs pre hs = pure $
    Map.fromList [f (CI.original k, v) | (k, v) <- Map.toList hs, p k]
  where
    f = join (***) Text.decodeUtf8
    p = BS.isPrefixOf pre . CI.foldedCase

xml :: FromXML a => Text -> Cursor -> Either String a
xml = undefined

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

toTextHashJSON :: (ToText k, ToJSON v)
               => HashMap k v
               -> Value
toTextHashJSON = toJSON . map (first (String . toText)) . Map.toList

fromTextHashJSON :: (Eq a, Hashable a, ToText a, FromText a, FromJSON v)
                 => Value
                 -> Parser (HashMap a v)
fromTextHashJSON = withObject "HashMap EnumKey v" f
  where
    f = fmap Map.fromList . mapM g . Map.toList

    g (k, v) =
        (,) <$> either fail return (fromText k)
            <*> parseJSON v
