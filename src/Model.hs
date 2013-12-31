{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Model
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Model where

import           Control.Applicative
import           Control.Error
import           Data.Aeson           hiding (String)
import           Data.Aeson.Types     hiding (String)
import qualified Data.ByteString.Lazy as LBS
import           Data.Char
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.Monoid
import           Data.Ord
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Vector          as Vector
import           GHC.Generics         (Generic)
import           Helpers

loadModel :: FilePath -> Script Model
loadModel path = scriptIO (LBS.readFile path) >>= hoistEither . eitherDecode

data Model = Model
    { mApiVersion       :: !Text
    , mType             :: !ServiceType
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
              <*> fmap normalise (o .:? "documentation" .!= "")
              <*> parseJSON ops

    parseJSON _ =
        fail "Unable to parse Model."

instance ToJSON Model where
    toJSON Model{..} = object
        [ "api_version"           .= mApiVersion
        , "type"                  .= mType
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

data ServiceType = RestXml | RestJson | Json | Query
    deriving (Show, Generic)

instance FromJSON ServiceType where
    parseJSON = genericParseJSON options

instance ToJSON ServiceType where
    toJSON = genericToJSON options

data SignatureVersion = V2 | V3 | V3HTTPS | V4 | S3
    deriving (Show, Generic)

instance FromJSON SignatureVersion where
    parseJSON = genericParseJSON $ options
        { constructorTagModifier = map toLower
        }

instance ToJSON SignatureVersion where
    toJSON = genericToJSON $ options
        { constructorTagModifier = map toLower
        }

data Operation = Operation
    { oName             :: !Text
    , oAlias            :: Maybe Text
    , oDocumentation    :: [Text]
    , oDocumentationUrl :: !Text
    , oHttp             :: Maybe HTTP
    , oInput            :: Maybe Shape
    , oOutput           :: Maybe Shape
    , oErrors           :: [Shape]
    , oPagination       :: Maybe Pagination
    } deriving (Show, Generic)

instance FromJSON Operation where
    parseJSON (Object o) = Operation
        <$> o .:  "name"
        <*> o .:? "alias"
        <*> fmap normalise (o .:? "documentation" .!= "")
        <*> o .:? "documentation_url" .!= ""
        <*> o .:? "http"
        <*> o .:? "input"
        <*> o .:? "output"
        <*> o .:  "errors"
        <*> o .:? "pagination"

    parseJSON _ =
        fail "Unable to parse Operation."

instance ToJSON Operation where
    toJSON = genericToJSON options

data HTTP = HTTP
    { hMethod :: !Text
    , hUri    :: !Text
    } deriving (Show, Generic)

instance FromJSON HTTP where
    parseJSON = genericParseJSON options

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
      }

    | SList
      { sItem          :: !Shape
      , sFlattened     :: !Bool
      , sLength        :: !Int

      , sShapeName     :: Maybe Text
      , sRequired      :: !Bool
      , sDocumentation :: [Text]
      , sXmlname       :: Maybe Text
      }

    | SMap
      { sKey           :: !Shape
      , sValue         :: !Shape

      , sShapeName     :: Maybe Text
      , sRequired      :: !Bool
      , sDocumentation :: [Text]
      , sXmlname       :: Maybe Text
      }

    | SPrim
      { sType          :: !Prim
      , sLocation      :: Maybe Text
      , sMinLength     :: Maybe Int
      , sMaxLength     :: Maybe Int
      , sPattern       :: Maybe Text

      , sShapeName     :: Maybe Text
      , sRequired      :: !Bool
      , sDocumentation :: [Text]
      , sXmlname       :: Maybe Text
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
        <*> o .:? "shape_name"
        <*> o .:? "required" .!= False
        <*> fmap normalise (o .:? "documentation" .!= "")
        <*> o .:? "xmlname"
      where
        f Structure = SStruct
            <$> o .: "members"
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
        f Boolean   = prim PBoolean
        f Blob      = prim PBlob
        f Timestamp = prim PTimestamp
        f Long      = prim PLong

        prim t = SPrim t
            <$> o .:? "location"
            <*> o .:? "min_length"
            <*> o .:? "max_length"
            <*> o .:? "pattern"

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
    | Boolean
    | Blob
    | Timestamp
    | Long
      deriving (Eq, Ord, Show, Generic)

instance FromJSON Type where
    parseJSON = genericParseJSON options

data Prim
    = PString
    | PInteger
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
    { pLimitKey    :: Maybe Text
    , pInputToken  :: !Text
    , pOutputToken :: !Text
    , pResultKey   :: !Text
    } deriving (Show, Generic)

instance FromJSON Pagination where
    parseJSON = genericParseJSON options

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
