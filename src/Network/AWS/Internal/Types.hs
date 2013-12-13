{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

-- Module      : Network.AWS.Internal.Types
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.Types where

import           Control.Applicative
import           Control.Error
import           Control.Exception
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Aeson                      hiding (Error)
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy.Char8      as LBS
import           Data.Conduit
import qualified Data.Conduit.Binary             as Conduit
import           Data.Foldable                   (Foldable)
import           Data.IORef
import           Data.Monoid
import           Data.String
import           Data.Time
import           GHC.Generics
import           Network.HTTP.Conduit
import           Network.HTTP.QueryString.Pickle
import           Network.HTTP.Types
import qualified Text.ParserCombinators.ReadP    as ReadP
import qualified Text.Read                       as Read
import           Text.XML.Expat.Pickle.Generic

class Rq a where
    type Er a
    type Rs a

    request  :: a -> Raw
    response :: MonadIO m
             => a
             -> Response (ResumableSource (AWS m) ByteString)
             -> AWS m (Either AWSError (Either (Er a) (Rs a)))

    default response :: (MonadIO m, IsXML (Er a), IsXML (Rs a))
                     => a
                     -> Response (ResumableSource (AWS m) ByteString)
                     -> AWS m (Either AWSError (Either (Er a) (Rs a)))
    response _ rs = do
        -- FIXME: use xml-conduit instead of hexpat to avoid need to conv to bs
        lbs <- responseBody rs $$+- Conduit.sinkLbs
        let bs = LBS.toStrict lbs
        whenDebug . liftIO $ BS.putStrLn bs
        return $
            if statusIsSuccessful $ responseStatus rs
                then either (Left . Err) (Right . Right) $ success bs
                else either (Left . Err) (Right . Left)  $ failure bs
      where
        success :: ByteString -> Either String (Rs a)
        success = fromXML

        failure :: ByteString -> Either String (Er a)
        failure = fromXML

instance Show (ResumableSource (AWS m) ByteString) where
    show _ = "ResumableSource AWS ByteString"

class Pg a where
    next :: a -> Rs a -> Maybe a

data AWSError = Err String | Ex SomeException | Ers [AWSError]
    deriving (Show)

instance Monoid AWSError where
    mempty = Ers []
    mappend (Ers a) (Ers b) = Ers $ a ++ b
    mappend (Ers a) b       = Ers $ a ++ [b]
    mappend a       (Ers b) = Ers $ a : b
    mappend a       b       = Ers [a, b]

instance Error AWSError where
    strMsg = Err

instance IsString AWSError where
    fromString = Err

class ToError a where
    toError :: a -> AWSError

instance ToError AWSError where
    toError = id

instance ToError String where
    toError = Err

instance ToError SomeException where
    toError = Ex

data Auth = Auth
    { accessKeyId     :: !ByteString
    , secretAccessKey :: !ByteString
    , securityToken   :: Maybe ByteString
    , expiration      :: Maybe UTCTime
    }

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

newtype AWS m a = AWS
    { unwrap :: ReaderT Env (EitherT AWSError m) a
    } deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader Env
        , MonadError AWSError
        )

instance MonadTrans AWS where
    lift = AWS . lift . lift

instance MonadThrow m => MonadThrow (AWS m) where
    monadThrow = lift . monadThrow

instance (MonadIO m, MonadUnsafeIO m, MonadThrow m) => MonadResource (AWS m) where
    liftResourceT f = AWS $ fmap awsResource ask >>= liftIO . runInternalState f

-- instance MonadIO m => MonadResource (AWS m) where
--     liftResourceT r = AWS $ do
--         e <- ask
--         liftIO . runInternalState r $ awsResource e

getAuth :: MonadIO m => AWS m Auth
getAuth = AWS $ fmap awsAuth ask >>= liftIO . readIORef

getManager :: Monad m => AWS m Manager
getManager = AWS $ awsManager <$> ask

getRegion :: Monad m => AWS m Region
getRegion = AWS $ awsRegion <$> ask

getDebug :: Monad m => AWS m Bool
getDebug = AWS $ awsDebug <$> ask

whenDebug :: Monad m => AWS m () -> AWS m ()
whenDebug f = getDebug >>= \p -> when p f

type Signer = Raw -> Auth -> Region -> UTCTime -> Request

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

endpoint :: Service -> Region -> ByteString
endpoint Service{..} reg =
    case svcEndpoint of
        Custom bs -> bs
        Global    -> svcName <> ".amazonaws.com"
        Regional  -> BS.intercalate "." $
            [svcName, BS.pack $ show reg, "amazonaws.com"]

data Raw = Raw
    { rqService :: !Service
    , rqMethod  :: !StdMethod
    , rqPath    :: !ByteString
    , rqQuery   :: [(ByteString, ByteString)]
    , rqHeaders :: [Header]
    , rqBody    :: !RequestBody
    }

instance Show Raw where
    show Raw{..} = unlines
        [ "Raw:"
        , "rqMethod  = " ++ show rqMethod
        , "rqPath    = " ++ show rqPath
        , "rqHeaders = " ++ show rqHeaders
        , "rqQuery   = " ++ show rqQuery
        ]

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

instance Show Region where
    show reg = case reg of
        NorthVirginia   -> "us-east-1"
        NorthCalifornia -> "us-west-1"
        Oregon          -> "us-west-2"
        Ireland         -> "eu-west-1"
        Singapore       -> "ap-southeast-1"
        Tokyo           -> "ap-northeast-1"
        Sydney          -> "ap-southeast-2"
        SaoPaulo        -> "sa-east-1"

instance Read Region where
    readPrec = readAssocList
        [ ("us-east-1",      NorthVirginia)
        , ("us-west-1",      NorthCalifornia)
        , ("us-west-2",      Oregon)
        , ("eu-west-1",      Ireland)
        , ("ap-southeast-1", Singapore)
        , ("ap-northeast-1", Tokyo)
        , ("ap-southeast-2", Sydney)
        , ("sa-east-1",      SaoPaulo)
        ]

instance IsQuery Region where
    queryPickler = qpPrim

instance IsXML Region where
    xmlPickler = xpContent xpPrim

defaultRegion :: Region
defaultRegion = NorthVirginia

region :: Monad m => Service -> AWS m Region
region Service{..} =
    case svcEndpoint of
        Global -> return defaultRegion
        _      -> getRegion

data AvailabilityZone = AZ
    { azRegion :: !Region
    , azSuffix :: !Char
    } deriving (Eq, Ord)

instance Show AvailabilityZone where
    show (AZ r z) = show r ++ [z]

instance Read AvailabilityZone where
    readsPrec _ [] = []
    readsPrec _ s  = [(AZ (read $ init s) (last s), "")]

instance IsQuery AvailabilityZone where
    queryPickler = qpPrim

instance IsXML AvailabilityZone where
    xmlPickler = xpContent xpPrim

data InstanceType
    = T1_Micro
    | M1_Small
    | M1_Medium
    | M1_Large
    | M1_XLarge
    | M3_XLarge
    | M3_2XLarge
    | C3_XLarge
    | C3_2XLarge
    | C3_4XLarge
    | C3_8XLarge
    | C1_Medium
    | C1_XLarge
    | CC2_8XLarge
    | M2_XLarge
    | M2_2XLarge
    | M2_4XLarge
    | CR1_8XLarge
    | HI1_4XLarge
    | HS1_8XLarge
    | CG1_4XLarge
      deriving (Eq, Ord, Generic)

instance Show InstanceType where
    show typ = case typ of
        T1_Micro    -> "t1.micro"
        M1_Small    -> "m1.small"
        M1_Medium   -> "m1.medium"
        M1_Large    -> "m1.large"
        M1_XLarge   -> "m1.xlarge"
        M3_XLarge   -> "m3.xlarge"
        M3_2XLarge  -> "m3.2xlarge"
        C3_XLarge   -> "c3.xlarge"
        C3_2XLarge  -> "c3.2xlarge"
        C3_4XLarge  -> "c3.4xlarge"
        C3_8XLarge  -> "c3.8xlarge"
        C1_Medium   -> "c1.medium"
        C1_XLarge   -> "c1.xlarge"
        CC2_8XLarge -> "cc2.8xlarge"
        M2_XLarge   -> "m2.xlarge"
        M2_2XLarge  -> "m2.2xlarge"
        M2_4XLarge  -> "m2.4xlarge"
        CR1_8XLarge -> "cr1.8xlarge"
        HI1_4XLarge -> "hi1.4xlarge"
        HS1_8XLarge -> "hs1.8xlarge"
        CG1_4XLarge -> "cg1.4xlarge"

instance Read InstanceType where
    readPrec = readAssocList
        [ ("t1.micro",    T1_Micro)
        , ("m1.small",    M1_Small)
        , ("m1.medium",   M1_Medium)
        , ("m1.large",    M1_Large)
        , ("m1.xlarge",   M1_XLarge)
        , ("m3.xlarge",   M3_XLarge)
        , ("m3.2xlarge",  M3_2XLarge)
        , ("c3.xlarge",   C3_XLarge)
        , ("c3.2xlarge",  C3_2XLarge)
        , ("c3.4xlarge",  C3_4XLarge)
        , ("c3.8xlarge",  C3_8XLarge)
        , ("c1.medium",   C1_Medium)
        , ("c1.xlarge",   C1_XLarge)
        , ("cc2.8xlarge", CC2_8XLarge)
        , ("m2.xlarge",   M2_XLarge)
        , ("m2.2xlarge",  M2_2XLarge)
        , ("m2.4xlarge",  M2_4XLarge)
        , ("cr1.8xlarge", CR1_8XLarge)
        , ("hi1.4xlarge", HI1_4XLarge)
        , ("hs1.8xlarge", HS1_8XLarge)
        , ("cg1.4xlarge", CG1_4XLarge)
        ]

instance IsQuery InstanceType where
    queryPickler = qpPrim

instance IsXML InstanceType where
    xmlPickler = xpContent xpPrim

readAssocList :: [(String, a)] -> Read.ReadPrec a
readAssocList xs = Read.choice $
    map (\(x, y) -> Read.lift $ ReadP.string x >> return y) xs

newtype Items a = Items { items :: [a] }
    deriving (Eq, Show, Generic, Foldable)

newtype Members a = Members { members :: [a] }
    deriving (Eq, Show, Generic, Foldable)
