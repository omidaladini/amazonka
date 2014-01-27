{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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

import           Data.Default
import           Data.Monoid
import           Data.String
import           Data.Tagged
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Text.Helpers
import           GHC.Generics
import           Network.HTTP.QueryString.Generic
import           Text.XML.Generic

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
    fromXML = primFromXML

instance ToXML ResourceName where
    toXML = primToXML

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
    fromText e                = failFromText $ "Unrecognised region: " <> e

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

instance ToQuery Region where
    toQuery = primToQuery

instance FromXML Region where
    fromXML = primFromXML

instance ToXML Region where
    toXML = primToXML

instance Default Region where
    def = NorthVirginia

data AvailabilityZone = AZ
    { azRegion :: !Region
    , azSuffix :: !Char
    } deriving (Eq, Ord, Generic)

instance FromText AvailabilityZone where
    fromText txt
        | Text.length txt < 2 =
            failFromText $ "Unable to parse AvailabilityZone: " <> txt
        | otherwise =
            (`AZ` Text.last txt) `fmap` fromText (Text.init txt)

instance Read AvailabilityZone where
    readsPrec _ = readFromText

instance ToText AvailabilityZone where
    toText AZ{..} = toText azRegion `Text.snoc` azSuffix

instance Show AvailabilityZone where
    show = showToText

instance ToQuery AvailabilityZone where
    toQuery = primToQuery

instance FromXML AvailabilityZone where
    fromXML = primFromXML

instance ToXML AvailabilityZone where
    toXML = primToXML

-- data InstanceType
--     = T1_Micro
--       -- ^
--       --
--       -- * @Architecture:@ 32-bit/64-bit
--       --
--       -- * @vCPU:@ 1
--       --
--       -- * @ECPU:@ Variable
--       --
--       -- * @Memory (GiB):@ 0.615
--       --
--       -- * @Instance Storage (GB):@ EBS only
--       --
--       -- * @EBS Optimised:@ -
--       --
--       -- * @Network Performance:@ Very Low
--     | M3_XLarge
--       -- ^
--       --
--       -- * @Architecture:@ 64-bit
--       --
--       -- * @vCPU:@ 4
--       --
--       -- * @ECPU:@ 13
--       --
--       -- * @Memory (GiB):@ 15
--       --
--       -- * @Instance Storage (GB):@ 2 x 40 SSD
--       --
--       -- * @EBS Optimised:@ Yes
--       --
--       -- * @Network Performance:@ Moderate
--     | M3_2XLarge
--       -- ^
--       --
--       -- * @Architecture:@ 64-bit
--       --
--       -- * @vCPU:@ 8
--       --
--       -- * @ECPU:@ 26
--       --
--       -- * @Memory (GiB):@ 30
--       --
--       -- * @Instance Storage (GB):@ 2 x 80 SSD
--       --
--       -- * @EBS Optimised:@ Yes
--       --
--       -- * @Network Performance:@ High
--     | M1_Small
--       -- ^
--       --
--       -- * @Architecture:@ 32-bit/64-bit
--       --
--       -- * @vCPU:@ 11
--       --
--       -- * @ECPU:@ 1
--       --
--       -- * @Memory (GiB):@ 1.7
--       --
--       -- * @Instance Storage (GB):@ 1 x 160
--       --
--       -- * @EBS Optimised:@ -
--       --
--       -- * @Network Performance:@ Low
--     | M1_Medium
--       -- ^
--       --
--       -- * @Architecture:@ 32-bit/64-bit
--       --
--       -- * @vCPU:@ 1
--       --
--       -- * @ECPU:@ 2
--       --
--       -- * @Memory (GiB):@ 3.75
--       --
--       -- * @Instance Storage (GB):@ 1 x 410
--       --
--       -- * @EBS Optimised:@ -
--       --
--       -- * @Network Performance:@ Moderate
--     | M1_Large
--       -- ^
--       --
--       -- * @Architecture:@ 64-bit
--       --
--       -- * @vCPU:@ 2
--       --
--       -- * @ECPU:@ 4
--       --
--       -- * @Memory (GiB):@ 7.5
--       --
--       -- * @Instance Storage (GB):@ 2 x 420
--       --
--       -- * @EBS Optimised:@ Yes
--       --
--       -- * @Network Performance:@ Moderate
--     | M1_XLarge
--       -- ^
--       --
--       -- * @Architecture:@ 64-bit
--       --
--       -- * @vCPU:@ 4
--       --
--       -- * @ECPU:@ 8
--       --
--       -- * @Memory (GiB):@ 15
--       --
--       -- * @Instance Storage (GB):@ 4 x 420
--       --
--       -- * @EBS Optimised:@ Yes
--       --
--       -- * @Network Performance:@ High
--     | C3_Large
--       -- ^
--       --
--       -- * @Architecture:@ 64-bit
--       --
--       -- * @vCPU:@ 2
--       --
--       -- * @ECPU:@ 7
--       --
--       -- * @Memory (GiB):@ 3.75
--       --
--       -- * @Instance Storage (GB):@ 2 x 16 SSD
--       --
--       -- * @EBS Optimised:@ -
--       --
--       -- * @Network Performance:@ Moderate
--     | C3_XLarge
--       -- ^
--       --
--       -- * @Architecture:@ 64-bit
--       --
--       -- * @vCPU:@ 4
--       --
--       -- * @ECPU:@ 14
--       --
--       -- * @Memory (GiB):@ 7.5
--       --
--       -- * @Instance Storage (GB):@ 2 x 40 SSD
--       --
--       -- * @EBS Optimised:@ Yes
--       --
--       -- * @Network Performance:@ Moderate
--     | C3_2XLarge
--       -- ^
--       --
--       -- * @Architecture:@ 64-bit
--       --
--       -- * @vCPU:@ 8
--       --
--       -- * @ECPU:@ 28
--       --
--       -- * @Memory (GiB):@ 15
--       --
--       -- * @Instance Storage (GB):@ 2 x 80 SSD
--       --
--       -- * @EBS Optimised:@ Yes
--       --
--       -- * @Network Performance:@ High
--     | C3_4XLarge
--       -- ^
--       --
--       -- * @Architecture:@ 64-bit
--       --
--       -- * @vCPU:@ 16
--       --
--       -- * @ECPU:@ 55
--       --
--       -- * @Memory (GiB):@ 30
--       --
--       -- * @Instance Storage (GB):@ 2 x 160 SSD
--       --
--       -- * @EBS Optimised:@ Yes
--       --
--       -- * @Network Performance:@ High
--     | C3_8XLarge
--       -- ^
--       --
--       -- * @Architecture:@ 64-bit
--       --
--       -- * @vCPU:@ 32
--       --
--       -- * @ECPU:@ 108
--       --
--       -- * @Memory (GiB):@ 60
--       --
--       -- * @Instance Storage (GB):@ 2 x 320 SSD
--       --
--       -- * @EBS Optimised:@ -
--       --
--       -- * @Network Performance:@ 10 Gigabit
--     | C1_Medium
--       -- ^
--       --
--       -- * @Architecture:@ 32-bit/64-bit
--       --
--       -- * @vCPU:@ 2
--       --
--       -- * @ECPU:@ 5
--       --
--       -- * @Memory (GiB):@ 1.7
--       --
--       -- * @Instance Storage (GB):@ 1 x 350
--       --
--       -- * @EBS Optimised:@ -
--       --
--       -- * @Network Performance:@ Moderate
--     | C1_XLarge
--       -- ^
--       --
--       -- * @Architecture:@ 64-bit
--       --
--       -- * @vCPU:@ 8
--       --
--       -- * @ECPU:@ 20
--       --
--       -- * @Memory (GiB):@ 7
--       --
--       -- * @Instance Storage (GB):@ 4 x 420
--       --
--       -- * @EBS Optimised:@ Yes
--       --
--       -- * @Network Performance:@ High
--     | CC2_8XLarge
--       -- ^
--       --
--       -- * @Architecture:@ 64-bit
--       --
--       -- * @vCPU:@ 32
--       --
--       -- * @ECPU:@ 88
--       --
--       -- * @Memory (GiB):@ 60.5
--       --
--       -- * @Instance Storage (GB):@ 4 x 840
--       --
--       -- * @EBS Optimised:@ -
--       --
--       -- * @Network Performance:@ 10 Gigabit
--     | G2_2XLarge
--       -- ^
--       --
--       -- * @Architecture:@ 64-bit
--       --
--       -- * @vCPU:@ 8
--       --
--       -- * @ECPU:@ 26
--       --
--       -- * @Memory (GiB):@ 15
--       --
--       -- * @Instance Storage (GB):@ 1 x 60 SSD
--       --
--       -- * @EBS Optimised:@ Yes
--       --
--       -- * @Network Performance:@ High
--     | CG1_4XLarge
--       -- ^
--       --
--       -- * @Architecture:@ 64-bit
--       --
--       -- * @vCPU:@ 16
--       --
--       -- * @ECPU:@ 33.5
--       --
--       -- * @Memory (GiB):@ 22.5
--       --
--       -- * @Instance Storage (GB):@ 2 x 840
--       --
--       -- * @EBS Optimised:@ -
--       --
--       -- * @Network Performance:@ 10 Gigabit
--     | M2_XLarge
--       -- ^
--       --
--       -- * @Architecture:@ 64-bit
--       --
--       -- * @vCPU:@ 2
--       --
--       -- * @ECPU:@ 6.5
--       --
--       -- * @Memory (GiB):@ 17.1
--       --
--       -- * @Instance Storage (GB):@ 1 x 420
--       --
--       -- * @EBS Optimised:@ -
--       --
--       -- * @Network Performance:@ Moderate
--     | M2_2XLarge
--       -- ^
--       --
--       -- * @Architecture:@ 64-bit
--       --
--       -- * @vCPU:@ 4
--       --
--       -- * @ECPU:@ 13
--       --
--       -- * @Memory (GiB):@ 34.2
--       --
--       -- * @Instance Storage (GB):@ 1 x 850
--       --
--       -- * @EBS Optimised:@ Yes
--       --
--       -- * @Network Performance:@ Moderate
--     | M2_4XLarge
--       -- ^
--       --
--       -- * @Architecture:@ 64-bit
--       --
--       -- * @vCPU:@ 8
--       --
--       -- * @ECPU:@ 26
--       --
--       -- * @Memory (GiB):@ 68.4
--       --
--       -- * @Instance Storage (GB):@ 2 x 840
--       --
--       -- * @EBS Optimised:@ Yes
--       --
--       -- * @Network Performance:@ High
--     | CR1_8XLarge
--       -- ^
--       --
--       -- * @Architecture:@ 64-bit
--       --
--       -- * @vCPU:@ 32
--       --
--       -- * @ECPU:@ 88
--       --
--       -- * @Memory (GiB):@ 244
--       --
--       -- * @Instance Storage (GB):@ 2 x 120
--       --
--       -- * @EBS Optimised:@ -
--       --
--       -- * @Network Performance:@ 10 Gigabit
--     | I2_XLarge
--       -- ^
--       --
--       -- * @Architecture:@ 64-bit
--       --
--       -- * @vCPU:@ 4
--       --
--       -- * @ECPU:@ 14
--       --
--       -- * @Memory (GiB):@ 30.5
--       --
--       -- * @Instance Storage (GB):@ 1 x 800 SSD
--       --
--       -- * @EBS Optimised:@ Yes
--       --
--       -- * @Network Performance:@ Moderate
--     | I2_2XLarge
--       -- ^
--       --
--       -- * @Architecture:@ 64-bit
--       --
--       -- * @vCPU:@ 8
--       --
--       -- * @ECPU:@ 27
--       --
--       -- * @Memory (GiB):@ 61
--       --
--       -- * @Instance Storage (GB):@ 2 x 800 SSD
--       --
--       -- * @EBS Optimised:@ Yes
--       --
--       -- * @Network Performance:@ High
--     | I2_4XLarge
--       -- ^
--       --
--       -- * @Architecture:@ 64-bit
--       --
--       -- * @vCPU:@ 16
--       --
--       -- * @ECPU:@ 53
--       --
--       -- * @Memory (GiB):@ 122
--       --
--       -- * @Instance Storage (GB):@ 4 x 800 SSD
--       --
--       -- * @EBS Optimised:@ Yes
--       --
--       -- * @Network Performance:@ High
--     | I2_8XLarge
--       -- ^
--       --
--       -- * @Architecture:@ 64-bit
--       --
--       -- * @vCPU:@ 32
--       --
--       -- * @ECPU:@ 104
--       --
--       -- * @Memory (GiB):@ 244
--       --
--       -- * @Instance Storage (GB):@ 8 x 800 SSD
--       --
--       -- * @EBS Optimised:@ -
--       --
--       -- * @Network Performance:@ 10 Gigabit
--     | HS1_8XLarge
--       -- ^
--       --
--       -- * @Architecture:@ 64-bit
--       --
--       -- * @vCPU:@ 16
--       --
--       -- * @ECPU:@ 35
--       --
--       -- * @Memory (GiB):@ 117
--       --
--       -- * @Instance Storage (GB):@ 24 x 2,0483
--       --
--       -- * @EBS Optimised:@ -
--       --
--       -- * @Network Performance:@ 10 Gigabit
--     | HI1_4XLarge
--       -- ^
--       --
--       -- * @Architecture:@ 64-bit
--       --
--       -- * @vCPU:@ 16
--       --
--       -- * @ECPU:@ 35
--       --
--       -- * @Memory (GiB):@ 60.5
--       --
--       -- * @Instance Storage (GB):@ 2 x 1,024 SSD
--       --
--       -- * @EBS Optimised:@ -
--       --
--       -- * @Network Performance:@ 10 Gigabit
--       deriving (Eq, Ord, Generic)

-- instance FromText InstanceType where
--     fromText "t1.micro"    = Right T1_Micro
--     fromText "m1.small"    = Right M1_Small
--     fromText "m1.medium"   = Right M1_Medium
--     fromText "m1.large"    = Right M1_Large
--     fromText "m1.xlarge"   = Right M1_XLarge
--     fromText "m2.xlarge"   = Right M2_XLarge
--     fromText "m2.2xlarge"  = Right M2_2XLarge
--     fromText "m2.4xlarge"  = Right M2_4XLarge
--     fromText "m3.xlarge"   = Right M3_XLarge
--     fromText "m3.2xlarge"  = Right M3_2XLarge
--     fromText "c1.medium"   = Right C1_Medium
--     fromText "c1.xlarge"   = Right C1_XLarge
--     fromText "c3.large"    = Right C3_Large
--     fromText "c3.xlarge"   = Right C3_XLarge
--     fromText "c3.2xlarge"  = Right C3_2XLarge
--     fromText "c3.4xlarge"  = Right C3_4XLarge
--     fromText "c3.8xlarge"  = Right C3_8XLarge
--     fromText "cc2.8xlarge" = Right CC2_8XLarge
--     fromText "g2.2xlarge"  = Right G2_2XLarge
--     fromText "cg1.4xlarge" = Right CG1_4XLarge
--     fromText "cr1.8xlarge" = Right CR1_8XLarge
--     fromText "i2.xlarge"   = Right I2_XLarge
--     fromText "i2.2xlarge"  = Right I2_2XLarge
--     fromText "i2.4xlarge"  = Right I2_4XLarge
--     fromText "i2.8xlarge"  = Right I2_8XLarge
--     fromText "hs1.8xlarge" = Right HS1_8XLarge
--     fromText "hi1.4xlarge" = Right HI1_4XLarge
--     fromText e             = failFromText $ "Unrecognised instance type: " <> e

-- instance Read InstanceType where
--     readsPrec _ = readFromText

-- instance ToText InstanceType where
--     toText T1_Micro    = "t1.micro"
--     toText M1_Small    = "m1.small"
--     toText M1_Medium   = "m1.medium"
--     toText M1_Large    = "m1.large"
--     toText M1_XLarge   = "m1.xlarge"
--     toText M2_XLarge   = "m2.xlarge"
--     toText M2_2XLarge  = "m2.2xlarge"
--     toText M2_4XLarge  = "m2.4xlarge"
--     toText M3_XLarge   = "m3.xlarge"
--     toText M3_2XLarge  = "m3.2xlarge"
--     toText C1_Medium   = "c1.medium"
--     toText C1_XLarge   = "c1.xlarge"
--     toText C3_Large    = "c3.large"
--     toText C3_XLarge   = "c3.xlarge"
--     toText C3_2XLarge  = "c3.2xlarge"
--     toText C3_4XLarge  = "c3.4xlarge"
--     toText C3_8XLarge  = "c3.8xlarge"
--     toText CC2_8XLarge = "cc2.8xlarge"
--     toText G2_2XLarge  = "g2.2xlarge"
--     toText CG1_4XLarge = "cg1.4xlarge"
--     toText CR1_8XLarge = "cr1.8xlarge"
--     toText I2_XLarge   = "i2.xlarge"
--     toText I2_2XLarge  = "i2.2xlarge"
--     toText I2_4XLarge  = "i2.4xlarge"
--     toText I2_8XLarge  = "i2.8xlarge"
--     toText HS1_8XLarge = "hs1.8xlarge"
--     toText HI1_4XLarge = "hi1.4xlarge"

-- instance Show InstanceType where
--     show = showToText

-- instance ToQuery InstanceType where
--     toQuery = primToQuery

-- instance FromXML InstanceType where
--     fromXML = primFromXML

-- instance ToXML InstanceType where
--     toXML = primToXML
