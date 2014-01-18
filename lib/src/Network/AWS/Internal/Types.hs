{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import           Control.Monad.Error
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Aeson                        hiding (Error)
import qualified Data.Attoparsec.Text              as AText
import           Data.ByteString.Char8             (ByteString)
import qualified Data.ByteString.Char8             as BS
import qualified Data.ByteString.Lazy.Char8        as LBS
import           Data.Conduit
import qualified Data.Conduit.Binary               as Conduit
import           Data.Default
import           Data.Foldable                     (Foldable)
import           Data.IORef
import           Data.Monoid
import           Data.String
import           Data.Text                         (Text)
import qualified Data.Text.Encoding                as Text
import           Data.Time
import           GHC.Generics
import           Network.AWS.Internal.Types.Common
import           Network.HTTP.Conduit
import           Network.HTTP.QueryString.Pickle
import           Network.HTTP.Types
import qualified Text.ParserCombinators.ReadP      as ReadP
import qualified Text.Read                         as Read
import           Text.XML.Generic

class Rq a where
    type Er a
    type Rs a

    request  :: a -> Raw
    response :: a
             -> Response (ResumableSource AWS ByteString)
             -> AWS (Either AWSError (Either (Er a) (Rs a)))

    default response :: (FromXML (Er a), FromXML (Rs a))
                     => a
                     -> Response (ResumableSource AWS ByteString)
                     -> AWS (Either AWSError (Either (Er a) (Rs a)))
    response _ rs = do
        -- FIXME: use xml-conduit instead of hexpat to avoid need to conv to bs
        lbs <- responseBody rs $$+- Conduit.sinkLbs
        let bs = LBS.toStrict lbs
        whenDebug . liftIO $ BS.putStrLn bs
        return $
            if statusIsSuccessful $ responseStatus rs
                then either (Left . Err) (Right . Right) $ success bs
                else either (Left . Err) (Right . Left) $ failure bs
      where
        success :: ByteString -> Either String (Rs a)
        success = fromXML

        failure :: ByteString -> Either String (Er a)
        failure = fromXML

instance Show (ResumableSource AWS ByteString) where
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

-- newtype Items a = Items { items :: [a] }
--     deriving (Eq, Show, Generic, Foldable)

-- newtype Members a = Members { members :: [a] }
--     deriving (Eq, Show, Generic, Foldable)
