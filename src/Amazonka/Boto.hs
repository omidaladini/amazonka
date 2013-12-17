{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Amazonka.Boto
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Amazonka.Boto where

import           Amazonka.Log
import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Data.Aeson              hiding (String)
import           Data.Aeson.Types        hiding (String)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as BS
import qualified Data.ByteString.Lazy    as LBS
import           Data.Char
import           Data.Foldable           (foldl', foldMap)
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as Map
import qualified Data.List               as List
import           Data.Maybe
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import qualified Data.Text.Lazy.Builder  as LText
import qualified Data.Text.Lazy.Encoding as LText
import qualified Data.Text.Lazy.IO       as LText
import qualified Data.Vector             as Vector
import           GHC.Generics            (Generic)
import           System.Directory
import qualified Text.EDE                as EDE

loadModel :: FilePath -> Script Model
loadModel path = msg ("Loading Model: " ++ path)
    >>  scriptIO (LBS.readFile path)
    >>= hoistEither . eitherDecode

data Model = Model
    { mApiVersion       :: !Text
    , mType             :: !ServiceType
    , mResultWrapped    :: !Bool
    , mSignatureVersion :: !SignatureVersion
    , mServiceFullName  :: !Text
    , mEndpointPrefix   :: !Text
--    , mDocumentation    :: !Text
    , mOperations       :: [Operation]
    } deriving (Show, Generic)

instance FromJSON Model where
    parseJSON (Object o) = Model
        <$> o .:  "api_version"
        <*> o .:? "type" .!= Query
        <*> o .:? "result_wrapped" .!= False
        <*> o .:  "signature_version"
        <*> o .:  "service_full_name"
        <*> o .:  "endpoint_prefix"
--        <*> o .:? "documentation" .!= ""
        <*> ops
      where
        ops = do
            Object m <- o .: "operations"
            parseJSON . Array . Vector.fromList $ Map.elems m

    parseJSON _ =
        fail "Unable to parse Model."

data ServiceType = RestXml | RestJson | Json | Query
    deriving (Show, Generic)

instance FromJSON ServiceType where
    parseJSON = genericParseJSON options

data SignatureVersion = V2 | V3 | V3HTTPS | V4 | S3
    deriving (Show, Generic)

instance FromJSON SignatureVersion where
    parseJSON = genericParseJSON $ options
        { constructorTagModifier = map toLower
        }

data Operation = Operation
    { oName             :: !Text
    , oAlias            :: Maybe Text
    -- , oDocumentation    :: !Text
    , oDocumentationUrl :: !Text
    , oHttp             :: Maybe HTTP
    , oInput            :: Maybe Shape
    , oOutput           :: Maybe Shape
    , oErrors           :: [Shape]
    } deriving (Show)

instance FromJSON Operation where
    parseJSON (Object o) = do
        Operation <$> o .:  "name"
              <*> o .:? "alias"
--              <*> o .:? "documentation" .!= ""
              <*> o .:? "documentation_url" .!= ""
              <*> o .:? "http"
              <*> o .:? "input"
              <*> o .:? "output"
              <*> o .:  "errors"

    parseJSON _ =
        fail "Unable to parse Operation."

data HTTP = HTTP
    { hMethod :: !Text
    , hUri    :: !Text
    } deriving (Show, Generic)

instance FromJSON HTTP where
    parseJSON = genericParseJSON options

data Type
    = Structure
    | List
    | Map
    | String
    | Integer
    | Boolean
    | Blob
    | Timestamp
    deriving (Show, Generic)

instance FromJSON Type where
    parseJSON = genericParseJSON options

data Prim
    = PString
    | PInteger
    | PBoolean
    | PBlob
    | PTimestamp
      deriving (Show)

data Shape
    = SStruct
      { sShapeName :: Maybe Text
      , sMembers   :: HashMap Text Shape
      }

    | SList
      { sShapeName :: Maybe Text
      , sMember    :: Shape
      }

    | SMap
      { sShapeName :: Maybe Text
      }

    | SPrim
      { sType      :: !Prim
      , sShapeName :: Maybe Text
      , sRequired  :: Maybe Bool
      , sLocation  :: Maybe Text
      , sMinLength :: Maybe Int
      , sMaxLength :: Maybe Int
      , sPattern   :: Maybe Text
      }

      deriving (Show)

instance FromJSON Shape where
    parseJSON (Object o) = o .: "type" >>= f
      where
        f Structure = SStruct
            <$> o .:? "shape_name"
            <*> o .:  "members"

        f List = SList
            <$> o .:? "shape_name"
            <*> o .:  "members"

        f Map = SMap
            <$> o .:? "shape_name"

        f String    = prim PString
        f Integer   = prim PInteger
        f Boolean   = prim PBoolean
        f Blob      = prim PBlob
        f Timestamp = prim PTimestamp

        prim t = SPrim t
            <$> o .:? "shape_name"
            <*> o .:? "required"
            <*> o .:? "location"
            <*> o .:? "min_length"
            <*> o .:? "max_length"
            <*> o .:? "pattern"

    parseJSON x =
        fail $ "Unable to parse Shape:\n" ++ show x



options :: Options
options = defaultOptions
    { fieldLabelModifier     = lowerWith '_' . dropWhile isLower
    , constructorTagModifier = lowerWith '-'
    , allNullaryToStringTag  = True
    }

lowerWith :: Char -> String -> String
lowerWith x = map toLower . tail . snd . break (== x) . concatMap f
  where
    f c | isUpper c = [x, toLower c]
        | otherwise = [c]
