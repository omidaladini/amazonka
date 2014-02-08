{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

-- Module      : Network.AWS.Internal.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
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
import           Data.ByteString.Char8             (ByteString)
import qualified Data.ByteString.Lazy.Char8        as LBS
import           Data.Conduit
import qualified Data.Conduit.Binary               as Conduit
import           Data.IORef
import           Data.Monoid
import           Data.String
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import           Data.Time
import           Network.AWS.Generics.XML
import           Network.AWS.Internal.Types.Common
import           Network.HTTP.Conduit
import           Network.HTTP.Types

data Env = Env
    { awsRegion   :: !Region
    , awsDebug    :: !Bool
    , awsResource :: !InternalState
    , awsManager  :: !Manager
    , awsAuth     :: !(IORef Auth)
    }

newtype AWS a = AWS
    { unwrap :: ReaderT Env (EitherT AWSErrors IO) a
    } deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadUnsafeIO
        , MonadThrow
        , MonadReader Env
        , MonadError AWSErrors
        )

instance MonadResource AWS where
    liftResourceT f = AWS $
        fmap awsResource ask >>= liftIO . runInternalState f

instance MonadThrow (EitherT AWSErrors IO) where
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

newtype AWSErrors = AWSErrors { awsErrors :: [Text] }
    deriving (Eq, Show)

instance Monoid AWSErrors where
    mempty      = AWSErrors []
    mappend a b = AWSErrors $ awsErrors a ++ awsErrors b

instance IsString AWSErrors where
    fromString = AWSErrors . (:[]) . Text.pack

instance Error AWSErrors where
    strMsg = fromString
    noMsg  = AWSErrors []

class AWSError a where
    awsError :: a -> AWSErrors

instance AWSError Text where
    awsError = AWSErrors . (:[])

instance AWSError String where
    awsError = fromString

instance AWSError SomeException where
    awsError = awsError . show

awsThrow :: AWSError e => e -> AWS a
awsThrow = throwError . awsError

awsEitherT :: AWSError e => EitherT e IO a -> AWS a
awsEitherT = AWS . lift . fmapLT awsError

awsEither :: AWSError e => Either e a -> AWS a
awsEither = either awsThrow return

data Signer = Signer
    { sigAccess  :: !ByteString
    , sigSecret  :: !ByteString
    , sigToken   :: Maybe ByteString
    , sigTime    :: !UTCTime
    , sigRegion  :: !ByteString
    , sigService :: !ByteString
    , sigVersion :: !ByteString
    , sigMethod  :: !ByteString
    , sigHost    :: !ByteString
    , sigPath    :: !ByteString
    , sigQuery   :: Query
    , sigHeaders :: [Header]
    , sigBody    :: RequestBody
    }

data Endpoint
    = Global
    | Regional
    | Custom !ByteString

data Service = Service
    { svcEndpoint :: !Endpoint
    , svcSigner   :: Signer -> Request
    , svcName     :: !ByteString
    , svcVersion  :: !ByteString
    }

data RawRequest = RawRequest
    { rawService :: !Service
    , rawMethod  :: !StdMethod
    , rawPath    :: !Text
    , rawQuery   :: QueryText
    , rawHeaders :: [Header]
    , rawBody    :: RequestBody
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

    default response :: (Show (Er a), Show (Rs a), FromXML (Er a), FromXML (Rs a))
                     => a
                     -> Response (ResumableSource AWS ByteString)
                     -> AWS (Either (Er a) (Rs a))
    response _ rs = do
        printDebug rs
        lbs <- responseBody rs $$+- Conduit.sinkLbs
        printDebug lbs
        x   <- f (statusIsSuccessful $ responseStatus rs) lbs
        printDebug x
        return x
      where
        f True  = fmap Right . awsEither . decodeXML
        f False = fmap Left  . awsEither . decodeXML

class AWSPager a where
    next :: AWSRequest a => a -> Rs a -> Maybe a

newtype Debug a = Debug { unDebug :: a }

printDebug :: Show (Debug a) => a -> AWS ()
printDebug = whenDebug . liftIO . print . Debug

instance Show (ResumableSource AWS ByteString) where
    show = const "ResumableSource AWS ByteString"

instance Show (Debug Request) where
    show r = "Debug => Request:\n" ++ show r

instance Show (Debug (Response (ResumableSource AWS ByteString))) where
    show (Debug rs) = "Debug => Response:\n" ++ show rs

instance Show (Debug LBS.ByteString) where
    show (Debug lbs) = "Debug => Lazy ByteString:\n" ++ LBS.unpack lbs

instance (Show e, Show a) => Show (Debug (Either e a)) where
    show (Debug (Left  e)) = "Debug => Left:\n"  ++ show e
    show (Debug (Right x)) = "Debug => Right:\n" ++ show x
