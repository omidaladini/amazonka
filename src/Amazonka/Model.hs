{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Amazonka.Model
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Amazonka.Model where

import           Amazonka.Log
import           Control.Applicative
import           Control.Error
import           Data.Aeson           hiding (String)
import           Data.Aeson.Types     hiding (String)
import qualified Data.ByteString.Lazy as LBS
import           Data.Char
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Vector          as Vector
import           GHC.Generics         (Generic)

loadModel :: FilePath -> Script Model
loadModel path = msg ("Loading " ++ path)
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
    , mName             :: !Text
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
        <*> name
      where
        ops = do
            Object m <- o .: "operations"
            parseJSON . Array . Vector.fromList $ Map.elems m

        name = do
            n <- o .: "service_full_name"
            return . mconcat . Text.words . strip "AWS" $ strip "Amazon" n

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
    , oPagination       :: Maybe Pagination
    } deriving (Show)

instance FromJSON Operation where
    parseJSON (Object o) = Operation
        <$> o .:  "name"
        <*> o .:? "alias"
--        <*> o .:? "documentation" .!= ""
        <*> o .:? "documentation_url" .!= ""
        <*> o .:? "http"
        <*> o .:? "input"
        <*> o .:? "output"
        <*> o .:  "errors"
        <*> o .:? "pagination"

    parseJSON _ =
        fail "Unable to parse Operation."

data HTTP = HTTP
    { hMethod :: !Text
    , hUri    :: !Text
    } deriving (Show, Generic)

instance FromJSON HTTP where
    parseJSON = genericParseJSON options

data Shape
    = SStruct
      { sShapeName :: Maybe Text
      , sRequired  :: Bool
      , sFields    :: HashMap Text Shape
      , sOrder     :: Maybe [Text]
      }

    | SList
      { sShapeName :: Maybe Text
      , sRequired  :: Bool
      , sItem      :: Shape
      }

    | SMap
      { sShapeName :: Maybe Text
      , sRequired  :: Bool
      , sKey       :: Shape
      , sValue     :: Shape
      }

    | SPrim
      { sType      :: !Prim
      , sShapeName :: Maybe Text
      , sRequired  :: Bool
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
            <*> o .:? "required" .!= False
            <*> o .:  "members"
            <*> o .:? "member_order"

        f List = SList
            <$> o .:? "shape_name"
            <*> o .:? "required" .!= False
            <*> o .:  "members"

        f Map = SMap
            <$> o .:? "shape_name"
            <*> o .:? "required" .!= False
            <*> o .:  "keys"
            <*> o .:  "members"

        f String    = prim PString
        f Integer   = prim PInteger
        f Boolean   = prim PBoolean
        f Blob      = prim PBlob
        f Timestamp = prim PTimestamp
        f Long      = prim PLong

        prim t = SPrim t
            <$> o .:? "shape_name"
            <*> o .:? "required" .!= False
            <*> o .:? "location"
            <*> o .:? "min_length"
            <*> o .:? "max_length"
            <*> o .:? "pattern"

    parseJSON x =
        fail $ "Unable to parse Shape:\n" ++ show x

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
      deriving (Show, Generic)

instance FromJSON Type where
    parseJSON = genericParseJSON options

data Prim
    = PString
    | PInteger
    | PBoolean
    | PBlob
    | PTimestamp
    | PLong
      deriving (Show)

data Pagination = Pagination
    { pLimitKey    :: Maybe Text
    , pInputToken  :: !Text
    , pOutputToken :: !Text
    , pResultKey   :: !Text
    } deriving (Show, Generic)

instance FromJSON Pagination where
    parseJSON = genericParseJSON options


options :: Options
options = defaultOptions
    { fieldLabelModifier     = lowerWith '_' . dropWhile isLower
    , constructorTagModifier = lowerWith '-'
    , allNullaryToStringTag  = True
    }

lowerWith :: Char -> String -> String
lowerWith x = map toLower
    . tail
    . dropWhile (not . (x ==))
    . concatMap f
  where
    f c | isUpper c = [x, toLower c]
        | otherwise = [c]

strip :: Text -> Text -> Text
strip delim = f Text.stripSuffix . f Text.stripPrefix
  where
    f g x = fromMaybe x $ g delim x

