{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Module      : Network.AWS.Internal.Response
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.Response where

import           Control.Arrow                      ((***))
import           Control.Error
import           Control.Monad
import           Data.Aeson
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Char8              as BS
import qualified Data.ByteString.Lazy.Char8         as LBS
import qualified Data.CaseInsensitive               as CI
import           Data.Conduit
import qualified Data.Conduit.Binary                as Conduit
import           Data.Default
import           Data.HashMap.Strict                (HashMap)
import qualified Data.HashMap.Strict                as Map
import           Data.Text                          (Text)
import qualified Data.Text.Encoding                 as Text
import           Network.AWS.Generics.XML
import           Network.AWS.Internal.Serialisation
import           Network.AWS.Internal.Types
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import qualified Text.XML                           as XML
import           Text.XML.Cursor                    (Cursor, fromDocument)

receiveXML :: (Show (Er a), Show (Rs a), FromXML (Er a))
           => (HashMap HeaderName ByteString -> Cursor -> Either String (Rs a))
           -> a
           -> Response (ResumableSource AWS ByteString)
           -> AWS (Either (Er a) (Rs a))
receiveXML f _ rs = decodeResponse decodeXML doc rs
  where
    doc lbs = fmapL show (XML.parseLBS def lbs)
        >>= f (Map.fromList $ responseHeaders rs)
          . fromDocument

receiveBody :: (Show (Er a), Show (Rs a), FromXML (Er a))
            => (HashMap HeaderName ByteString
            -> ResumableSource AWS ByteString -> Either String (Rs a))
            -> a
            -> Response (ResumableSource AWS ByteString)
            -> AWS (Either (Er a) (Rs a))
receiveBody c _ rs =
    printDebug rs >> f (statusIsSuccessful $ responseStatus rs)
  where
    f False = do
        lbs <- responseBody rs $$+- Conduit.sinkLbs
        printDebug lbs
        x   <- fmap Left  . awsEither $ decodeXML lbs
        printDebug x
        return x
    f True = fmap Right
        . awsEither
        $ c (Map.fromList $ responseHeaders rs) (responseBody rs)

receiveHeaders :: (Show (Er a), Show (Rs a), FromXML (Er a))
               => (HashMap HeaderName ByteString -> Either String (Rs a))
               -> a
               -> Response (ResumableSource AWS ByteString)
               -> AWS (Either (Er a) (Rs a))
receiveHeaders f _ rs =
    decodeResponse decodeXML (const . f . Map.fromList $ responseHeaders rs) rs

receiveEmpty :: forall a. (Show (Er a), Show (Rs a), FromXML (Er a))
              => Rs a
              -> a
              -> Response (ResumableSource AWS ByteString)
              -> AWS (Either (Er a) (Rs a))
receiveEmpty c _ rs =
    decodeResponse decodeXML (const $ Right c :: b -> Either String (Rs a)) rs

responseXML :: (Show (Er a), Show (Rs a), FromXML (Er a), FromXML (Rs a))
            => a
            -> Response (ResumableSource AWS ByteString)
            -> AWS (Either (Er a) (Rs a))
responseXML _ = decodeResponse decodeXML decodeXML

responseJSON :: (Show (Er a), Show (Rs a), FromJSON (Er a), FromJSON (Rs a))
             => a
             -> Response (ResumableSource AWS ByteString)
             -> AWS (Either (Er a) (Rs a))
responseJSON _ = decodeResponse eitherDecode eitherDecode

decodeResponse :: (Show e, Show a, AWSError x, AWSError y)
               => (LBS.ByteString -> Either x e)
               -> (LBS.ByteString -> Either y a)
               -> Response (ResumableSource AWS ByteString)
               -> AWS (Either e a)
decodeResponse failure success rs = do
    printDebug rs
    lbs <- responseBody rs $$+- Conduit.sinkLbs
    printDebug lbs
    x   <- f (statusIsSuccessful $ responseStatus rs) lbs
    printDebug x
    return x
  where
    f False = fmap Left  . awsEither . failure
    f True  = fmap Right . awsEither . success
