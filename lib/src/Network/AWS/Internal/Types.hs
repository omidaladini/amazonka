{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
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
import           Control.Monad.Error               (MonadError)
import qualified Control.Monad.Error               as Error
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Aeson                        hiding (Error, decode)
import           Data.ByteString.Char8             (ByteString)
import qualified Data.ByteString.Char8             as BS
import           Data.Conduit
import qualified Data.Conduit.Binary               as Conduit
import           Data.Default
import           Data.IORef
import           Data.Monoid
import           Data.Text                         (Text)
import qualified Data.Text.Encoding                as Text
import           Data.Time
import           Network.AWS.Internal.Types.Common
import           Network.HTTP.Conduit
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

throwError :: String -> AWS a
throwError = AWS . Error.throwError . AWSError . (:[])

eitherError :: Either String a -> AWS a
eitherError = either throwError return

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
        f True  = fmap Right . eitherError . decode
        f False = fmap Left  . eitherError . decode

class AWSPager a where
    next :: AWSRequest a => a -> Rs a -> Maybe a