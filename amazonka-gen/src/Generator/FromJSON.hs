{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Generator.FromJSON
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Generator.FromJSON where

import           Control.Applicative
import           Control.Arrow
import           Control.Error
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as LBS
import           Data.Default
import qualified Data.HashMap.Strict        as Map
import           Data.Monoid
import           Data.String.CaseConversion
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.Unsafe           as Text
import           GHC.Generics
import           Generator.AST
import           Generator.Log
import           Generator.Models
import           Network.HTTP.Types.Method

parseModel :: Model -> Script Service
parseModel Model{..} = do
    r <- scriptIO $ say "Parse Service" modPath >> LBS.readFile modPath
    hoistEither (eitherDecode r)

instance FromJSON Abbrev where
    parseJSON = withText "abbrev" (return . abbrev)

instance FromJSON Version where
    parseJSON = withText "version" (return . version)

instance FromJSON Doc where
    parseJSON = withText "documentation" (return . documentation)

instance FromJSON Time where
    parseJSON = fromCtor lowered

instance FromJSON Checksum where
    parseJSON = fromCtor lowered

instance FromJSON ServiceType where
    parseJSON = fromCtor (recase Camel Hyphen)

instance FromJSON Signature where
    parseJSON = withText "signature" $ \t ->
        return $ case t of
            "v2"      -> V2
            "v3"      -> V3
            "v3https" -> V3
            _         -> V4

instance FromJSON JSONV where
    parseJSON (String t) = return (JSONV t)
    parseJSON (Number n) = return . JSONV . Text.pack $ show n
    parseJSON e          = fail $ "Unrecognised JSONV field: " ++ show e

instance FromJSON Service where
    parseJSON = withObject "service" $ \o -> do
        n  <- o .: "service_full_name"
        a  <- o .: "service_abbreviation" .!= abbrev n
        v  <- o .: "api_version"
        os <- o .: "operations"

        let vNS    = namespace a v
            opNS x = x { opNamespace = vNS <> NS [opName x] }

        Service a n (rootNS vNS) vNS (typeNS vNS) v
            <$> o .:! "type"
            <*> o .:? "result_wrapped"   .!= False
            <*> o .:  "signature_version"
            <*> o .:! "documentation"
            <*> o .:  "endpoint_prefix"
            <*> o .:? "global_endpoint"
            <*> o .:? "xmlnamespace"
            <*> o .:! "timestamp_format"
            <*> o .:! "checksum_format"
            <*> o .:! "json_version"
            <*> o .:? "target_prefix"
            <*> pure (map opNS os)

instance FromJSON [Operation] where
    parseJSON = withObject "operations" (mapM parseJSON . Map.elems)

instance FromJSON Operation where
    parseJSON = withObject "operation" $ \o -> Operation
        <$> o .:  "name"
        <*> o .:? "alias"
        <*> pure def
        <*> o .:  "documentation"
        <*> o .:? "documentation_url"
        <*> parseJSON (Object o)
        <*> parseJSON (Object o)
        <*> pure []
        <*> o .:? "pagination"

instance FromJSON Request where
    parseJSON = withObject "request" $ \o -> Request
        <$> o .:! "input"
        <*> o .:! "http"

instance FromJSON Response where
    parseJSON = withObject "response" $ \o -> Response
        <$> o .:! "output"

instance FromJSON Location where
    parseJSON = fromCtor (lowered . drop 1)

instance FromJSON Common where
    parseJSON = withObject "common" $ \o -> do
        n <- o .:  "shape_name" <|> o .: "alias" <|> o .: "name" <|> return Nothing
        x <- o .:? "xmlname" .!= n
        l <- o .:! "location"
        Common n x l
            <$> o .:? "location_name" .!= n
            <*> o .:? "required"      .!= (if l == LBody then True else False)
            <*> o .:! "documentation"
            <*> o .:? "streaming"     .!= False

instance FromJSON Shape where
    parseJSON o = do
        f <-  parseJSON o :: Parser (Common -> Shape)
        f <$> parseJSON o

instance FromJSON (Common -> Shape) where
    parseJSON = withObject "shape" $ \o -> o .: "type" >>= f o
      where
        f o "structure" = do
            xs <- o .:? "members"      .!= mempty
            ys <- o .:? "member_order" .!= Map.keys xs :: Parser [Text]
            return . SStruct $ mapMaybe (\y -> Map.lookup y xs) ys

        f o "list" = SList
            <$> o .:  "members"
            <*> o .:? "flattened"  .!=  False
            <*> o .:? "min_length" .!= 0
            <*> o .:? "max_length" .!= 0

        f o "map" = SMap
            <$> o .: "keys"
            <*> o .: "members"

        f o typ = do
            ms <- o .:? "enum"

            let str  = Text.pack . recase Under Camel . Text.unpack
                enum = Map.fromList . map (first str . join (,)) <$> ms

            case enum of
                Just vs -> return (SEnum vs)
                Nothing -> SPrim
                    <$> parseJSON (String typ)
                    <*> o .:? "min_length" .!= 0
                    <*> o .:? "max_length" .!= 0
                    <*> o .:? "pattern"

instance FromJSON Prim where
    parseJSON = withText "type" $ \t ->
        case t of
            "string"    -> return PText
            "integer"   -> return PInteger
            "long"      -> return PInteger
            "double"    -> return PDouble
            "float"     -> return PDouble
            "boolean"   -> return PBool
            "blob"      -> return PByteString
            "timestamp" -> return PUTCTime
            _           -> fail $ "Unable to parse Prim from: " ++ Text.unpack t

instance FromJSON Pagination where
    parseJSON = withObject "pagination" $ \o ->
        let f k = o .: k <|> (head <$> o .: k)
         in Pagination
                <$> o .:? "more_key"
                <*> o .:? "limit_key"
                <*> f "input_token"
                <*> f "output_token"
                <*> f "result_key"

instance FromJSON HTTP where
    parseJSON = withObject "http" $ \o -> HTTP
        <$> (o .: "method" >>= method)
        <*> o .: "uri"
        <*> o .: "uri"
      where
        method = either (fail . BS.unpack) return
            . parseMethod
            . Text.encodeUtf8

instance FromJSON [PathPart] where
    parseJSON = withText "uri" $ return . path
       where
        path = filter (/= PConst "") . go . Text.takeWhile (/= '?')

        go x | Text.null s = [PConst l]
             | otherwise   = PConst l : PVar m : go (Text.unsafeTail t)
          where
            (m, t) = Text.span (/= '}') $ Text.unsafeTail s
            (l, s) = Text.span (/= '{') x

instance FromJSON [QueryPart] where
    parseJSON = withText "uri" $ return . query
      where
        query = map (uncurry QueryPart) . go . Text.dropWhile (/= '?')

        go x | Text.null s
             , Text.null l = []
             | Text.null s = [(Text.tail l, Nothing)]
             | otherwise   = brk : go (Text.unsafeTail t)
          where
            (m, t) = Text.span (/= '}') $ Text.unsafeTail s
            (l, s) = Text.span (/= '{') x

            brk | '=' <- Text.last l = (Text.init $ Text.tail l, Just m)
                | otherwise          = (Text.tail l, Nothing)

(.:!) :: (Default a, FromJSON a) => Object -> Text -> Parser a
(.:!) o k = o .:? k .!= def

fromField :: (Generic a, GFromJSON (Rep a))
          => (String -> String)
          -> Value
          -> Parser a
fromField f = genericParseJSON $ defaultOptions { fieldLabelModifier = f }

fromCtor :: (Generic a, GFromJSON (Rep a))
         => (String -> String)
         -> Value
         -> Parser a
fromCtor f = genericParseJSON $ defaultOptions
    { constructorTagModifier = f
    , allNullaryToStringTag  = True
    }