{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

-- Module      : Network.AWS.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Types where

import           Control.Applicative
import           Control.Error
import           Control.Exception
import           Control.Monad
import           Control.Monad.Error              (MonadError)
import qualified Control.Monad.Error              as Error
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Aeson                       hiding (Error, decode)
import qualified Data.Attoparsec.Text             as AText
import           Data.ByteString.Char8            (ByteString)
import qualified Data.ByteString.Char8            as BS
import           Data.Conduit
import qualified Data.Conduit.Binary              as Conduit
import           Data.Default
import           Data.IORef
import           Data.Monoid
import           Data.String
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding               as Text
import           Data.Text.Helpers
import           Data.Time
import           GHC.Generics
import           Network.HTTP.Conduit
import           Network.HTTP.QueryString.Generic
import           Network.HTTP.Types
import           Text.XML.Generic

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
    parseJSON (Object o) = Auth
        <$> o .:  "AccessKeyId"
        <*> o .:  "SecretAccessKey"
        <*> o .:? "Token"
        <*> o .:? "Expiration"
    parseJSON _ = mzero

data Env = Env
    { awsRegion   :: !Region
    , awsDebug    :: !Bool
    , awsResource :: !InternalState
    , awsManager  :: !Manager
    , awsAuth     :: !(IORef Auth)
    }

newtype AWSError = AWSError { awsErrors :: [String] }
    deriving (Eq, Show)

instance Monoid AWSError where
    mempty      = AWSError []
    mappend a b = AWSError $ awsErrors a ++ awsErrors b

instance IsString AWSError where
    fromString = AWSError . (:[])

eitherError :: Either String a -> AWS a
eitherError = either throwError return

throwError :: String -> AWS a
throwError = AWS . Error.throwError . fromString

newtype AWS a = AWS
    { unwrap :: ReaderT Env (EitherT AWSError IO) a
    } deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadUnsafeIO
        , MonadThrow
        , MonadReader Env
        , MonadError AWSError
        )

instance MonadResource AWS where
    liftResourceT f = AWS $
        fmap awsResource ask >>= liftIO . runInternalState f

instance MonadThrow (EitherT AWSError IO) where
    monadThrow = liftIO . throwIO

getAuth :: AWS Auth
getAuth = AWS $ fmap awsAuth ask >>= liftIO . readIORef

getManager :: AWS Manager
getManager = AWS $ awsManager <$> ask

getRegion :: AWS Region
getRegion = AWS $ awsRegion <$> ask

getDebug :: AWS Bool
getDebug = AWS $ awsDebug <$> ask

whenDebug :: AWS () -> AWS ()
whenDebug f = getDebug >>= \p -> when p f

type Signer = RawRequest -> Auth -> Region -> UTCTime -> Request

data Endpoint
    = Global
    | Regional
    | Custom ByteString

data Service = Service
    { svcEndpoint :: !Endpoint
    , svcSigner   :: !Signer
    , svcName     :: !ByteString
    , svcVersion  :: !ByteString
    }

region :: Service -> AWS Region
region Service{..} =
    case svcEndpoint of
        Global -> return def
        _      -> getRegion


endpoint :: Service -> Region -> ByteString
endpoint Service{..} reg =
    case svcEndpoint of
        Custom bs -> bs
        Global    -> svcName <> ".amazonaws.com"
        Regional  -> BS.intercalate "." $
            [svcName, BS.pack $ show reg, "amazonaws.com"]

data RawRequest = RawRequest
    { rawService :: !Service
    , rawMethod  :: !StdMethod
    , rawPath    :: !ByteString
    , rawQuery   :: [(ByteString, ByteString)]
    , rawHeaders :: [Header]
    , rawBody    :: !RequestBody
    }

instance Show RawRequest where
    show RawRequest{..} = unlines
        [ "RawRequest:"
        , "rawMethod  = " ++ show rawMethod
        , "rawPath    = " ++ show rawPath
        , "rawHeaders = " ++ show rawHeaders
        , "rawQuery   = " ++ show rawQuery
        ]

class AWSRequest a where
    type Er a
    type Rs a

    request  :: a -> RawRequest
    response :: a
             -> Response (ResumableSource AWS ByteString)
             -> AWS (Either (Er a) (Rs a))

    default response :: (FromXML (Er a), FromXML (Rs a))
                     => a
                     -> Response (ResumableSource AWS ByteString)
                     -> AWS (Either (Er a) (Rs a))
    response _ rs = (responseBody rs $$+- Conduit.sinkLbs)
        >>= f (statusIsSuccessful $ responseStatus rs)
      where
        f True  = fmap Right . eitherError . decodeXML
        f False = fmap Left  . eitherError . decodeXML

class AWSPager a where
    next :: AWSRequest a => a -> Rs a -> Maybe a


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

instance ToQuery AvailabilityZone where
    toQuery = primToQuery

instance FromXML AvailabilityZone where
    fromXML = primFromXML

instance ToXML AvailabilityZone where
    toXML = primToXML

data InstanceType
    = T1_Micro
      -- ^
      --
      -- * @Architecture:@ 32-bit/64-bit
      --
      -- * @vCPU:@ 1
      --
      -- * @ECPU:@ Variable
      --
      -- * @Memory (GiB):@ 0.615
      --
      -- * @Instance Storage (GB):@ EBS only
      --
      -- * @EBS Optimised:@ -
      --
      -- * @Network Performance:@ Very Low
    | M3_XLarge
      -- ^
      --
      -- * @Architecture:@ 64-bit
      --
      -- * @vCPU:@ 4
      --
      -- * @ECPU:@ 13
      --
      -- * @Memory (GiB):@ 15
      --
      -- * @Instance Storage (GB):@ 2 x 40 SSD
      --
      -- * @EBS Optimised:@ Yes
      --
      -- * @Network Performance:@ Moderate
    | M3_2XLarge
      -- ^
      --
      -- * @Architecture:@ 64-bit
      --
      -- * @vCPU:@ 8
      --
      -- * @ECPU:@ 26
      --
      -- * @Memory (GiB):@ 30
      --
      -- * @Instance Storage (GB):@ 2 x 80 SSD
      --
      -- * @EBS Optimised:@ Yes
      --
      -- * @Network Performance:@ High
    | M1_Small
      -- ^
      --
      -- * @Architecture:@ 32-bit/64-bit
      --
      -- * @vCPU:@ 11
      --
      -- * @ECPU:@ 1
      --
      -- * @Memory (GiB):@ 1.7
      --
      -- * @Instance Storage (GB):@ 1 x 160
      --
      -- * @EBS Optimised:@ -
      --
      -- * @Network Performance:@ Low
    | M1_Medium
      -- ^
      --
      -- * @Architecture:@ 32-bit/64-bit
      --
      -- * @vCPU:@ 1
      --
      -- * @ECPU:@ 2
      --
      -- * @Memory (GiB):@ 3.75
      --
      -- * @Instance Storage (GB):@ 1 x 410
      --
      -- * @EBS Optimised:@ -
      --
      -- * @Network Performance:@ Moderate
    | M1_Large
      -- ^
      --
      -- * @Architecture:@ 64-bit
      --
      -- * @vCPU:@ 2
      --
      -- * @ECPU:@ 4
      --
      -- * @Memory (GiB):@ 7.5
      --
      -- * @Instance Storage (GB):@ 2 x 420
      --
      -- * @EBS Optimised:@ Yes
      --
      -- * @Network Performance:@ Moderate
    | M1_XLarge
      -- ^
      --
      -- * @Architecture:@ 64-bit
      --
      -- * @vCPU:@ 4
      --
      -- * @ECPU:@ 8
      --
      -- * @Memory (GiB):@ 15
      --
      -- * @Instance Storage (GB):@ 4 x 420
      --
      -- * @EBS Optimised:@ Yes
      --
      -- * @Network Performance:@ High
    | C3_Large
      -- ^
      --
      -- * @Architecture:@ 64-bit
      --
      -- * @vCPU:@ 2
      --
      -- * @ECPU:@ 7
      --
      -- * @Memory (GiB):@ 3.75
      --
      -- * @Instance Storage (GB):@ 2 x 16 SSD
      --
      -- * @EBS Optimised:@ -
      --
      -- * @Network Performance:@ Moderate
    | C3_XLarge
      -- ^
      --
      -- * @Architecture:@ 64-bit
      --
      -- * @vCPU:@ 4
      --
      -- * @ECPU:@ 14
      --
      -- * @Memory (GiB):@ 7.5
      --
      -- * @Instance Storage (GB):@ 2 x 40 SSD
      --
      -- * @EBS Optimised:@ Yes
      --
      -- * @Network Performance:@ Moderate
    | C3_2XLarge
      -- ^
      --
      -- * @Architecture:@ 64-bit
      --
      -- * @vCPU:@ 8
      --
      -- * @ECPU:@ 28
      --
      -- * @Memory (GiB):@ 15
      --
      -- * @Instance Storage (GB):@ 2 x 80 SSD
      --
      -- * @EBS Optimised:@ Yes
      --
      -- * @Network Performance:@ High
    | C3_4XLarge
      -- ^
      --
      -- * @Architecture:@ 64-bit
      --
      -- * @vCPU:@ 16
      --
      -- * @ECPU:@ 55
      --
      -- * @Memory (GiB):@ 30
      --
      -- * @Instance Storage (GB):@ 2 x 160 SSD
      --
      -- * @EBS Optimised:@ Yes
      --
      -- * @Network Performance:@ High
    | C3_8XLarge
      -- ^
      --
      -- * @Architecture:@ 64-bit
      --
      -- * @vCPU:@ 32
      --
      -- * @ECPU:@ 108
      --
      -- * @Memory (GiB):@ 60
      --
      -- * @Instance Storage (GB):@ 2 x 320 SSD
      --
      -- * @EBS Optimised:@ -
      --
      -- * @Network Performance:@ 10 Gigabit
    | C1_Medium
      -- ^
      --
      -- * @Architecture:@ 32-bit/64-bit
      --
      -- * @vCPU:@ 2
      --
      -- * @ECPU:@ 5
      --
      -- * @Memory (GiB):@ 1.7
      --
      -- * @Instance Storage (GB):@ 1 x 350
      --
      -- * @EBS Optimised:@ -
      --
      -- * @Network Performance:@ Moderate
    | C1_XLarge
      -- ^
      --
      -- * @Architecture:@ 64-bit
      --
      -- * @vCPU:@ 8
      --
      -- * @ECPU:@ 20
      --
      -- * @Memory (GiB):@ 7
      --
      -- * @Instance Storage (GB):@ 4 x 420
      --
      -- * @EBS Optimised:@ Yes
      --
      -- * @Network Performance:@ High
    | CC2_8XLarge
      -- ^
      --
      -- * @Architecture:@ 64-bit
      --
      -- * @vCPU:@ 32
      --
      -- * @ECPU:@ 88
      --
      -- * @Memory (GiB):@ 60.5
      --
      -- * @Instance Storage (GB):@ 4 x 840
      --
      -- * @EBS Optimised:@ -
      --
      -- * @Network Performance:@ 10 Gigabit
    | G2_2XLarge
      -- ^
      --
      -- * @Architecture:@ 64-bit
      --
      -- * @vCPU:@ 8
      --
      -- * @ECPU:@ 26
      --
      -- * @Memory (GiB):@ 15
      --
      -- * @Instance Storage (GB):@ 1 x 60 SSD
      --
      -- * @EBS Optimised:@ Yes
      --
      -- * @Network Performance:@ High
    | CG1_4XLarge
      -- ^
      --
      -- * @Architecture:@ 64-bit
      --
      -- * @vCPU:@ 16
      --
      -- * @ECPU:@ 33.5
      --
      -- * @Memory (GiB):@ 22.5
      --
      -- * @Instance Storage (GB):@ 2 x 840
      --
      -- * @EBS Optimised:@ -
      --
      -- * @Network Performance:@ 10 Gigabit
    | M2_XLarge
      -- ^
      --
      -- * @Architecture:@ 64-bit
      --
      -- * @vCPU:@ 2
      --
      -- * @ECPU:@ 6.5
      --
      -- * @Memory (GiB):@ 17.1
      --
      -- * @Instance Storage (GB):@ 1 x 420
      --
      -- * @EBS Optimised:@ -
      --
      -- * @Network Performance:@ Moderate
    | M2_2XLarge
      -- ^
      --
      -- * @Architecture:@ 64-bit
      --
      -- * @vCPU:@ 4
      --
      -- * @ECPU:@ 13
      --
      -- * @Memory (GiB):@ 34.2
      --
      -- * @Instance Storage (GB):@ 1 x 850
      --
      -- * @EBS Optimised:@ Yes
      --
      -- * @Network Performance:@ Moderate
    | M2_4XLarge
      -- ^
      --
      -- * @Architecture:@ 64-bit
      --
      -- * @vCPU:@ 8
      --
      -- * @ECPU:@ 26
      --
      -- * @Memory (GiB):@ 68.4
      --
      -- * @Instance Storage (GB):@ 2 x 840
      --
      -- * @EBS Optimised:@ Yes
      --
      -- * @Network Performance:@ High
    | CR1_8XLarge
      -- ^
      --
      -- * @Architecture:@ 64-bit
      --
      -- * @vCPU:@ 32
      --
      -- * @ECPU:@ 88
      --
      -- * @Memory (GiB):@ 244
      --
      -- * @Instance Storage (GB):@ 2 x 120
      --
      -- * @EBS Optimised:@ -
      --
      -- * @Network Performance:@ 10 Gigabit
    | I2_XLarge
      -- ^
      --
      -- * @Architecture:@ 64-bit
      --
      -- * @vCPU:@ 4
      --
      -- * @ECPU:@ 14
      --
      -- * @Memory (GiB):@ 30.5
      --
      -- * @Instance Storage (GB):@ 1 x 800 SSD
      --
      -- * @EBS Optimised:@ Yes
      --
      -- * @Network Performance:@ Moderate
    | I2_2XLarge
      -- ^
      --
      -- * @Architecture:@ 64-bit
      --
      -- * @vCPU:@ 8
      --
      -- * @ECPU:@ 27
      --
      -- * @Memory (GiB):@ 61
      --
      -- * @Instance Storage (GB):@ 2 x 800 SSD
      --
      -- * @EBS Optimised:@ Yes
      --
      -- * @Network Performance:@ High
    | I2_4XLarge
      -- ^
      --
      -- * @Architecture:@ 64-bit
      --
      -- * @vCPU:@ 16
      --
      -- * @ECPU:@ 53
      --
      -- * @Memory (GiB):@ 122
      --
      -- * @Instance Storage (GB):@ 4 x 800 SSD
      --
      -- * @EBS Optimised:@ Yes
      --
      -- * @Network Performance:@ High
    | I2_8XLarge
      -- ^
      --
      -- * @Architecture:@ 64-bit
      --
      -- * @vCPU:@ 32
      --
      -- * @ECPU:@ 104
      --
      -- * @Memory (GiB):@ 244
      --
      -- * @Instance Storage (GB):@ 8 x 800 SSD
      --
      -- * @EBS Optimised:@ -
      --
      -- * @Network Performance:@ 10 Gigabit
    | HS1_8XLarge
      -- ^
      --
      -- * @Architecture:@ 64-bit
      --
      -- * @vCPU:@ 16
      --
      -- * @ECPU:@ 35
      --
      -- * @Memory (GiB):@ 117
      --
      -- * @Instance Storage (GB):@ 24 x 2,0483
      --
      -- * @EBS Optimised:@ -
      --
      -- * @Network Performance:@ 10 Gigabit
    | HI1_4XLarge
      -- ^
      --
      -- * @Architecture:@ 64-bit
      --
      -- * @vCPU:@ 16
      --
      -- * @ECPU:@ 35
      --
      -- * @Memory (GiB):@ 60.5
      --
      -- * @Instance Storage (GB):@ 2 x 1,024 SSD
      --
      -- * @EBS Optimised:@ -
      --
      -- * @Network Performance:@ 10 Gigabit
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

instance ToQuery InstanceType where
    toQuery = primToQuery

instance FromXML InstanceType where
    fromXML = primFromXML

instance ToXML InstanceType where
    toXML = primToXML
