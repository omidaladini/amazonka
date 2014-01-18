{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Network.AWS.Internal.Types.Common
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.Types.Common where

import           Control.Applicative
import           Control.Error
import           Control.Exception
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Aeson                      hiding (Error)
import qualified Data.Attoparsec.Text            as AText
import           Data.ByteString.Char8           (ByteString)
import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy.Char8      as LBS
import           Data.Conduit
import qualified Data.Conduit.Binary             as Conduit
import           Data.Default
import           Data.Foldable                   (Foldable)
import           Data.IORef
import           Data.Monoid
import           Data.String
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.Encoding              as Text
import           Data.Time
import           GHC.Generics
import           Network.HTTP.Conduit
import           Network.HTTP.QueryString.Pickle
import           Network.HTTP.Types
import           Text.Class
import qualified Text.ParserCombinators.ReadP    as ReadP
import qualified Text.Read                       as Read
import           Text.XML.Generic

data Region
    = NorthVirginia
    | NorthCalifornia
    | Oregon
    | Ireland
    | Singapore
    | Tokyo
    | Sydney
    | SaoPaulo
      deriving (Eq, Ord)

instance FromText Region where
    fromText = AText.parseOnly (AText.takeText >>= f)
      where
        f "us-east-1"      = return NorthVirginia
        f "us-west-1"      = return NorthCalifornia
        f "us-west-2"      = return Oregon
        f "eu-west-1"      = return Ireland
        f "ap-southeast-1" = return Singapore
        f "ap-northeast-1" = return Tokyo
        f "ap-southeast-2" = return Sydney
        f "sa-east-1"      = return SaoPaulo
        f e                = fail $ "Unrecognised region " ++ Text.unpack e

instance Read Region where
    readsPrec _ = readFromText

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
    show = showToText

instance IsQuery Region where
    queryPickler = primQuery

instance FromXML Region where
    fromXML = primFromXML

instance ToXML Region where
    toXML = primToXML

instance Default Region where
    def = NorthVirginia

data AvailabilityZone = AZ
    { azRegion :: !Region
    , azSuffix :: !Char
    } deriving (Eq, Ord)

instance FromText AvailabilityZone where
    fromText = AText.parseOnly p
      where
        p = do
            txt <- AText.takeText
            if Text.null txt
                then fail "Unable to parse AZ from zero length string."
                else f (fromText $ Text.init txt) (Text.last txt)

        f (Left  e) _ = fail e
        f (Right r) c = return $ AZ r c

instance Read AvailabilityZone where
    readsPrec _ = readFromText

instance ToText AvailabilityZone where
    toText AZ{..} = toText azRegion `Text.snoc` azSuffix

instance Show AvailabilityZone where
    show = showToText

instance IsQuery AvailabilityZone where
    queryPickler = primQuery

instance FromXML AvailabilityZone where
    fromXML = primFromXML

instance ToXML AvailabilityZone where
    toXML = primToXML

data InstanceType
    = T1_Micro
    | M1_Small
    | M1_Medium
    | M1_Large
    | M1_XLarge
    | M2_XLarge
    | M2_2XLarge
    | M2_4XLarge
    | M3_XLarge
    | M3_2XLarge
    | C1_Medium
    | C1_XLarge
    | C3_Large
    | C3_XLarge
    | C3_2XLarge
    | C3_4XLarge
    | C3_8XLarge
    | CC2_8XLarge
    | G2_2XLarge
    | CG1_4XLarge
    | CR1_8XLarge
    | I2_XLarge
    | I2_2XLarge
    | I2_4XLarge
    | I2_8XLarge
    | HS1_8XLarge
    | HI1_4XLarge
      deriving (Eq, Ord, Generic)

instance FromText InstanceType where
    fromText = AText.parseOnly (AText.takeText >>= f)
      where
        f "t1.micro"    = return T1_Micro
        f "m1.small"    = return M1_Small
        f "m1.medium"   = return M1_Medium
        f "m1.large"    = return M1_Large
        f "m1.xlarge"   = return M1_XLarge
        f "m2.xlarge"   = return M2_XLarge
        f "m2.2xlarge"  = return M2_2XLarge
        f "m2.4xlarge"  = return M2_4XLarge
        f "m3.xlarge"   = return M3_XLarge
        f "m3.2xlarge"  = return M3_2XLarge
        f "c1.medium"   = return C1_Medium
        f "c1.xlarge"   = return C1_XLarge
        f "c3.large"    = return C3_Large
        f "c3.xlarge"   = return C3_XLarge
        f "c3.2xlarge"  = return C3_2XLarge
        f "c3.4xlarge"  = return C3_4XLarge
        f "c3.8xlarge"  = return C3_8XLarge
        f "cc2.8xlarge" = return CC2_8XLarge
        f "g2.2xlarge"  = return G2_2XLarge
        f "cg1.4xlarge" = return CG1_4XLarge
        f "cr1.8xlarge" = return CR1_8XLarge
        f "i2.xlarge"   = return I2_XLarge
        f "i2.2xlarge"  = return I2_2XLarge
        f "i2.4xlarge"  = return I2_4XLarge
        f "i2.8xlarge"  = return I2_8XLarge
        f "hs1.8xlarge" = return HS1_8XLarge
        f "hi1.4xlarge" = return HI1_4XLarge
        f e             = fail $ "Unrecognised instance type " ++ Text.unpack e

instance Read InstanceType where
    readsPrec _ = readFromText

instance ToText InstanceType where
    toText T1_Micro    = "t1.micro"
    toText M1_Small    = "m1.small"
    toText M1_Medium   = "m1.medium"
    toText M1_Large    = "m1.large"
    toText M1_XLarge   = "m1.xlarge"
    toText M2_XLarge   = "m2.xlarge"
    toText M2_2XLarge  = "m2.2xlarge"
    toText M2_4XLarge  = "m2.4xlarge"
    toText M3_XLarge   = "m3.xlarge"
    toText M3_2XLarge  = "m3.2xlarge"
    toText C1_Medium   = "c1.medium"
    toText C1_XLarge   = "c1.xlarge"
    toText C3_Large    = "c3.large"
    toText C3_XLarge   = "c3.xlarge"
    toText C3_2XLarge  = "c3.2xlarge"
    toText C3_4XLarge  = "c3.4xlarge"
    toText C3_8XLarge  = "c3.8xlarge"
    toText CC2_8XLarge = "cc2.8xlarge"
    toText G2_2XLarge  = "g2.2xlarge"
    toText CG1_4XLarge = "cg1.4xlarge"
    toText CR1_8XLarge = "cr1.8xlarge"
    toText I2_XLarge   = "i2.xlarge"
    toText I2_2XLarge  = "i2.2xlarge"
    toText I2_4XLarge  = "i2.4xlarge"
    toText I2_8XLarge  = "i2.8xlarge"
    toText HS1_8XLarge = "hs1.8xlarge"
    toText HI1_4XLarge = "hi1.4xlarge"

instance Show InstanceType where
    show = showToText

instance IsQuery InstanceType where
    queryPickler = primQuery

instance FromXML InstanceType where
    fromXML = primFromXML

instance ToXML InstanceType where
    toXML = primToXML
