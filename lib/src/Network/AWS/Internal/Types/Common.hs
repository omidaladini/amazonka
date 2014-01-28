{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- Module      : Network.AWS.Internal.Types.Common
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.Types.Common where

import           Control.Applicative
import           Data.Aeson
import           Data.ByteString.Char8              (ByteString)
import qualified Data.ByteString.Char8              as BS
import           Data.Default
import           Data.Monoid
import           Data.String
import           Data.Tagged
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import qualified Data.Text.Encoding                 as Text
import           Data.Text.Helpers
import           Data.Time
import           GHC.Generics
import           Network.AWS.Internal.Serialisation
import           Network.HTTP.QueryString.Generic
import           Text.XML.Generic

newtype Key = Key { unKey :: Text }
    deriving (Eq, Ord, Show)

instance FromText Key where
    fromText = Right . Key

instance ToText Key where
    toText = unKey

instance FromJSON Key where
    parseJSON = fromTextJSON "Key"

instance ToJSON Key where
    toJSON = toTextJSON

newtype Blob = Blob { unBlob :: ByteString }
    deriving (Eq, Ord, Show, Generic)

instance IsString Blob where
    fromString = Blob . BS.pack

instance ToQuery Blob where
    toQuery = toQuery . Text.decodeUtf8 . unBlob

instance FromXML Blob where
    fromXML o = fmap Blob . fromXML (retag o)

instance ToXML Blob where
    toXML o = toXML (retag o) . unBlob

instance FromJSON Blob where
    parseJSON = withText "Blob" $ pure . Blob . Text.encodeUtf8

instance ToJSON Blob where
    toJSON = String . Text.decodeUtf8 . unBlob

newtype ResourceName = ResourceName { unResourceName :: Text }
    deriving (Show, Eq, Ord, Generic)

instance IsString ResourceName where
    fromString = ResourceName . Text.pack

instance FromText ResourceName where
    fromText = Right . ResourceName

instance ToText ResourceName where
    toText = unResourceName

instance ToQuery ResourceName where
    toQuery = toQuery . unResourceName

instance FromXML ResourceName where
    fromXML = fromTextXML

instance ToXML ResourceName where
    toXML = toTextXML

data Auth = Auth
    { authAccessKeyId     :: !Text
    , authSecretAccessKey :: !Text
    , authSecurityToken   :: Maybe Text
    , expiration          :: Maybe UTCTime
    }

accessKeyId :: Auth -> ByteString
accessKeyId = Text.encodeUtf8 . authAccessKeyId

secretAccessKey :: Auth -> ByteString
secretAccessKey = Text.encodeUtf8 . authSecretAccessKey

securityToken :: Auth -> Maybe ByteString
securityToken = fmap Text.encodeUtf8 . authSecurityToken

instance FromJSON Auth where
    parseJSON = withObject "Auth" $ \o -> Auth
        <$> o .:  "AccessKeyId"
        <*> o .:  "SecretAccessKey"
        <*> o .:? "Token"
        <*> o .:? "Expiration"

data Region
    = NorthVirginia
    | NorthCalifornia
    | Oregon
    | Ireland
    | Singapore
    | Tokyo
    | Sydney
    | SaoPaulo
      deriving (Eq, Ord, Generic)

instance FromText Region where
    fromText "us-east-1"      = Right NorthVirginia
    fromText "us-west-1"      = Right NorthCalifornia
    fromText "us-west-2"      = Right Oregon
    fromText "eu-west-1"      = Right Ireland
    fromText "ap-southeast-1" = Right Singapore
    fromText "ap-northeast-1" = Right Tokyo
    fromText "ap-southeast-2" = Right Sydney
    fromText "sa-east-1"      = Right SaoPaulo
    fromText e                = fromTextFail $ "Unrecognised region: " <> e

instance Read Region where
    readsPrec _ = fromTextRead

instance ToText Region where
    toText NorthVirginia   = "us-east-1"
    toText NorthCalifornia = "us-west-1"
    toText Oregon          = "us-west-2"
    toText Ireland         = "eu-west-1"
    toText Singapore       = "ap-southeast-1"
    toText Tokyo           = "ap-northeast-1"
    toText Sydney          = "ap-southeast-2"
    toText SaoPaulo        = "sa-east-1"

instance Show Region where
    show = toTextShow

instance ToQuery Region where
    toQuery = toTextQuery

instance FromXML Region where
    fromXML = fromTextXML

instance ToXML Region where
    toXML = toTextXML

instance Default Region where
    def = NorthVirginia

data AvailabilityZone = AZ
    { azRegion :: !Region
    , azSuffix :: !Char
    } deriving (Eq, Ord, Generic)

instance FromText AvailabilityZone where
    fromText txt
        | Text.length txt < 2 =
            fromTextFail $ "Unable to parse AvailabilityZone: " <> txt
        | otherwise =
            (`AZ` Text.last txt) `fmap` fromText (Text.init txt)

instance Read AvailabilityZone where
    readsPrec _ = fromTextRead

instance ToText AvailabilityZone where
    toText AZ{..} = toText azRegion `Text.snoc` azSuffix

instance Show AvailabilityZone where
    show = toTextShow

instance ToQuery AvailabilityZone where
    toQuery = toTextQuery

instance FromXML AvailabilityZone where
    fromXML = fromTextXML

instance ToXML AvailabilityZone where
    toXML = toTextXML
