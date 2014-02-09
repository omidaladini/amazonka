{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- Module      : Generator.Model
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Generator.Model where

import           Control.Applicative
import           Control.Arrow
import           Control.Error
import           Control.Monad
import qualified Data.Aeson           as Aeson
import           Data.Aeson           hiding (String)
import           Data.Aeson.Types     hiding (String)
import qualified Data.ByteString.Lazy as LBS
import           Data.Char
import           Data.Foldable        (any)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.Monoid
import           Data.Ord
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.Unsafe     as Text
import qualified Data.Vector          as Vector
import           GHC.Generics         (Generic)
import           Generator.Helpers
import           Prelude              hiding (any)
import           Text.EDE.Filters

loadModel :: FilePath -> Script Model
loadModel path = scriptIO (LBS.readFile path) >>= hoistEither . eitherDecode

data Model = Model
    { mApiVersion       :: !Text
    , mServiceType      :: !ServiceType
    , mResultWrapped    :: !Bool
    , mSignatureVersion :: !SignatureVersion
    , mName             :: !Text
    , mServiceFullName  :: !Text
    , mEndpointPrefix   :: !Text
    , mGlobalEndpoint   :: Maybe Text
    , mXmlNamespace     :: Maybe Text
    , mTimestamp        :: Maybe Text
    , mChecksum         :: Maybe Text
    , mDocumentation    :: [Text]
    , mOperations       :: [Operation]
    , mJSONVersion      :: !Text
    , mTargetPrefix     :: Maybe Text
    } deriving (Show, Generic)

instance FromJSON Model where
    parseJSON (Object o) = do
        Object m <- o .:  "operations"
        full     <- o .:  "service_full_name"
        a        <- o .:? "service_abbreviation" .!= full

        let abbrev = mconcat . Text.words . strip "AWS" $ strip "Amazon" a
            ops    = Array . Vector.fromList $ Map.elems m

        Model <$> o .:  "api_version"
              <*> o .:? "type" .!= Query
              <*> o .:? "result_wrapped" .!= False
              <*> o .:  "signature_version"
              <*> pure abbrev
              <*> pure full
              <*> o .:  "endpoint_prefix"
              <*> o .:? "global_endpoint"
              <*> o .:? "xmlnamespace"
              <*> o .:? "timestamp_format"
              <*> o .:? "checksum_format"
              <*> documentation o
              <*> (update abbrev <$> parseJSON ops)
              <*> version
              <*> o .:? "target_prefix"
      where
        version = do
            v <- o .:? "json_version" .!= Aeson.String "1.0"
            case v of
                Aeson.String t -> return t
                Aeson.Number n -> return . Text.pack $ show n
                _              -> fail "Unrecognised json_version field."

        -- CloudFront + ElasticTranscoder fuckery
        update n ops
            | "ElasticTranscoder" == n = map f ops
            | "CloudFront"        == n = map f ops
            | otherwise                = ops
          where
            f o'@Operation{..}
                | Just x <- oInput = o'
                    { oInput = Just $ x
                        { sFields = Map.map (\v -> v { sRequired = True }) $ sFields x
                        }
                    }
                | otherwise = o'

    parseJSON _ =
        fail "Unable to parse Model."

instance ToJSON Model where
    toJSON Model{..} = object
        [ "api_version"           .= mApiVersion
        , "service_type"          .= mServiceType
        , "result_wrapped"        .= mResultWrapped
        , "signature_version"     .= mSignatureVersion
        , "service_abbreviation"  .= mName
        , "service_full_name"     .= mServiceFullName
        , "endpoint_prefix"       .= mEndpointPrefix
        , "global_endpoint"       .= mGlobalEndpoint
        , "xmlnamespace"          .= mXmlNamespace
        , "timestamp_format"      .= mTimestamp
        , "checksum_format"       .= mChecksum
        , "service_documentation" .= mDocumentation
        , "operations"            .= map oName mOperations
        ]

data ServiceType = RestXML | RestJSON | JSON | Query
    deriving (Show)

instance FromJSON ServiceType where
    parseJSON (Aeson.String "rest-xml")  = return RestXML
    parseJSON (Aeson.String "rest-json") = return RestJSON
    parseJSON (Aeson.String "json")      = return JSON
    parseJSON (Aeson.String "query")     = return Query

    parseJSON _ =
        fail "Unable to parse ServiceType."

instance ToJSON ServiceType where
    toJSON = toJSON . Text.pack . show

data SignatureVersion = V2 | V3 | V3HTTPS | V4 | S3
    deriving (Show, Generic)

instance FromJSON SignatureVersion where
    parseJSON = genericParseJSON $ options
        { constructorTagModifier = map toLower
        }

instance ToJSON SignatureVersion where
    toJSON = toJSON . f
      where
        f :: SignatureVersion -> Text
        f V2      = "v2"
        f V3      = "v3"
        f V3HTTPS = "v3"
        f V4      = "v4"
        f S3      = "vS3"

data Operation = Operation
    { oName             :: !Text
    , oAlias            :: Maybe Text
    , oDocumentation    :: [Text]
    , oDocumentationUrl :: Maybe Text
    , oHttp             :: HTTP
    , oInput            :: Maybe Shape
    , oOutput           :: Maybe Shape
    , oErrors           :: [Shape]
    , oPagination       :: Maybe Pagination
    } deriving (Show, Generic)

instance FromJSON Operation where
    parseJSON (Object o) = Operation
        <$> (clean <$> (o .: "name" <|> o .: "alias"))
        <*> o .:? "alias"
        <*> documentation o
        <*> o .:? "documentation_url"
        <*> o .:? "http" .!= HTTP "POST" [] mempty
        <*> (fmap streaming <$> o .:? "input")
        <*> (fmap streaming <$> o .:? "output")
        <*> o .: "errors"
        <*> o .:? "pagination"
      where
        clean x
            | "2013_11_11" `Text.isSuffixOf` x = Text.take (Text.length x - 10) x
            | otherwise                        = x

        streaming s
            | SStruct{} <- s
            , any sStreaming $ sFields s = s { sStreaming = True }
            | otherwise                  = s

    parseJSON _ =
        fail "Unable to parse Operation."

instance ToJSON Operation where
    toJSON = genericToJSON options

data Part
    = T !Text
    | I !Text
      deriving (Eq, Show)

instance ToJSON Part where
    toJSON p =
        case p of
            T t -> f "T" t
            I i -> f "I" i
      where
        f k v = object
            [ "type"  .= (k :: Text)
            , "value" .= v
            ]

data HTTP = HTTP
    { hMethod :: !Text
    , hUri    :: [Part]
    , hQuery  :: HashMap Text Text
    } deriving (Show, Generic)

instance FromJSON HTTP where
    parseJSON (Object o) = do
        u <- o .: "uri"
        HTTP <$> o .: "method" <*> pure (uri u) <*> pure (query u)
      where
        uri = filter (/= T "") . go . Text.takeWhile (/= '?')
          where
            go x | Text.null s = [T l]
                 | otherwise   = T l : I m : go (Text.unsafeTail t)
              where
                (m, t) = Text.span (/= '}') $ Text.unsafeTail s
                (l, s) = Text.span (/= '{') x

        query = Map.fromList . go . Text.dropWhile (/= '?')
          where
            go x | Text.null s
                 , Text.null l = []
                 | Text.null s = [(Text.tail l, "")]
                 | otherwise   = brk : go (Text.unsafeTail t)
              where
                (m, t) = Text.span (/= '}') $ Text.unsafeTail s
                (l, s) = Text.span (/= '{') x

                brk | '=' <- Text.last l = (Text.init $ Text.tail l, m)
                    | otherwise          = (Text.tail l, "")

    parseJSON _ =
        fail "Unable to parse Operation."

instance ToJSON HTTP where
    toJSON = genericToJSON options

data Shape
    = SStruct
      { sFields        :: HashMap Text Shape
      , sOrder         :: Maybe [Text]

      , sShapeName     :: Maybe Text
      , sRequired      :: !Bool
      , sDocumentation :: [Text]
      , sXmlname       :: Maybe Text
      , sPrefix        :: !Text
      , sPayload       :: !Bool
      , sStreaming     :: !Bool
      }

    | SList
      { sItem          :: !Shape
      , sFlattened     :: !Bool
      , sLength        :: !Int

      , sShapeName     :: Maybe Text
      , sRequired      :: !Bool
      , sDocumentation :: [Text]
      , sXmlname       :: Maybe Text
      , sPrefix        :: !Text
      , sPayload       :: !Bool
      , sStreaming     :: !Bool
      }

    | SMap
      { sKey           :: !Shape
      , sValue         :: !Shape

      , sShapeName     :: Maybe Text
      , sRequired      :: !Bool
      , sDocumentation :: [Text]
      , sXmlname       :: Maybe Text
      , sPrefix        :: !Text
      , sPayload       :: !Bool
      , sStreaming     :: !Bool
      }

    | SPrim
      { sType          :: !Prim
      , sLocation      :: Maybe Text
      , sLocationName  :: Maybe Text
      , sMinLength     :: Maybe Int
      , sMaxLength     :: Maybe Int
      , sPattern       :: Maybe Text
      , sEnum          :: Maybe (HashMap Text Text)
      , sStrict        :: !Bool

      , sShapeName     :: Maybe Text
      , sRequired      :: !Bool
      , sDocumentation :: [Text]
      , sXmlname       :: Maybe Text
      , sPrefix        :: !Text
      , sPayload       :: !Bool
      , sStreaming     :: !Bool
      }

      deriving (Eq, Show, Generic)

instance Ord Shape where
    a `compare` b =
        compare (Down $ ctor a, sShapeName a)
                (Down $ ctor b, sShapeName b)
      where
        ctor SStruct {..} = Structure
        ctor SList   {..} = List
        ctor SMap    {..} = Map
        ctor SPrim   {..} = String

instance FromJSON Shape where
    parseJSON (Object o) = (o .: "type" >>= f)
        <*> ((Just <$> o .: "shape_name") <|> (Just <$> o .: "alias") <|> o .:? "name")
        <*> o .:? "required" .!= False
        <*> documentation o
        <*> o .:? "xmlname"
        <*> pure ""
        <*> o .:? "payload" .!= False
        <*> o .:? "streaming" .!= False
      where
        f Structure = SStruct
            <$> (names <$> o .:? "members" .!= mempty)
            <*> o .:? "member_order"

        f List = SList
            <$> o .:  "members"
            <*> o .:? "flattened"  .!= False
            <*> o .:? "min_length" .!= 0

        f Map = SMap
            <$> o .: "keys"
            <*> o .: "members"

        f String    = prim PString
        f Integer   = prim PInteger
        f Long      = prim PLong
        f Double    = prim PDouble
        f Float     = prim PDouble
        f Boolean   = prim PBoolean
        f Blob      = prim PBlob
        f Timestamp = prim PTimestamp

        prim t = do
            ms <- o .:? "enum"
            let enum = Map.fromList . map (first pascalize . join (,)) <$> ms
            SPrim (maybe t (const PEnum) enum)
                <$> o .:? "location"
                <*> o .:? "location_name"
                <*> o .:? "min_length"
                <*> o .:? "max_length"
                <*> o .:? "pattern"
                <*> pure enum
                <*> pure True

        names = Map.foldlWithKey' g mempty
          where
            g m k v = Map.insert (h k) v m

            h s | "VPC" `Text.isPrefixOf` s = ("vpc" <>) $ Text.drop 3 s
                | otherwise                 = s

    parseJSON x =
        fail $ "Unable to parse Shape:\n" ++ show x

instance ToJSON Shape where
    toJSON = genericToJSON options
        { constructorTagModifier = map toLower . drop 1
        , sumEncoding            = defaultTaggedObject { tagFieldName = "type" }
        }

data Type
    = Structure
    | List
    | Map
    | String
    | Integer
    | Double
    | Float
    | Boolean
    | Blob
    | Timestamp
    | Long
      deriving (Eq, Ord, Show, Generic)

instance FromJSON Type where
    parseJSON = genericParseJSON options

data Prim
    = PString
    | PEnum
    | PInteger
    | PDouble
    | PBoolean
    | PBlob
    | PTimestamp
    | PLong
      deriving (Eq, Show, Generic)

instance ToJSON Prim where
    toJSON = genericToJSON $ options
        { constructorTagModifier = map toLower . drop 1
        }

data Pagination = Pagination
    { pMoreKey     :: Maybe Text
    , pLimitKey    :: Maybe Text
    , pInputToken  :: !Text
    , pOutputToken :: !Text
    , pResultKeys  :: !Text
    } deriving (Show, Generic)

instance FromJSON Pagination where
    parseJSON (Object o) = Pagination
        <$> o .:? "more_key"
        <*> o .:? "limit_key"
        <*> f "input_token"
        <*> f "output_token"
        <*> f "result_key"
      where
        f k = o .: k <|> (head <$> o .: k)

    parseJSON _ =
        fail "Unable to parse Pagination."

instance ToJSON Pagination where
    toJSON = genericToJSON options

options :: Options
options = defaultOptions
    { fieldLabelModifier     = lowerWith '_' . dropWhile isLower
    , constructorTagModifier = lowerWith '-'
    , allNullaryToStringTag  = True
    }

shapeName :: Shape -> Text
shapeName = fromMaybe "?" . sShapeName

documentation :: Object -> Parser [Text]
documentation o = fmap normalise (o .:? "documentation" .!= "")
