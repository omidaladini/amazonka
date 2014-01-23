{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- Module      : Model
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Model where

import           Control.Applicative
import           Control.Arrow
import           Control.Error
import           Control.Monad
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
    , mPolicies         :: [Policy]
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
              <*> ((o .: "retry") >>= (.: "__default__") >>= (.: "policies"))

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
        , "policies"              .= mPolicies
        ]

data Policy = Policy
    { pName             :: !Text
    , pServiceErrorCode :: Maybe Text
    , pHttpStatusCode   :: Maybe Int
    } deriving (Show, Generic)

instance FromJSON [Policy] where
    parseJSON (Object o) = fmap catMaybes . mapM f $ Map.toList o
      where
        f (k, Object v) = do
            mr <- v .: "applies_when" >>= (.:? "response")
            maybe (return Nothing) (fmap Just . g k) mr

        f (k, _) =
            fail $ "Unable to parse Policy from " ++ Text.unpack k

        g k v = Policy (pascalise k)
            <$> v .:? "service_error_code"
            <*> v .:  "http_status_code"

    parseJSON _ =
        fail "Unable to parse Policy."

instance ToJSON Policy where
    toJSON = genericToJSON options

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
    , oDocumentationUrl :: Maybe Text
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
        <*> o .:? "documentation_url"
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
      , sEnum          :: Maybe (HashMap Text Text)

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
            <$> (names <$> o .: "members")
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
            let enum = Map.fromList . map (first pascalise . join (,)) <$> ms
            SPrim (maybe t (const PEnum) enum)
                <$> o .:? "location"
                <*> o .:? "min_length"
                <*> o .:? "max_length"
                <*> o .:? "pattern"
                <*> pure enum

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
