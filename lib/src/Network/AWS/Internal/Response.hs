{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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

import           Data.Aeson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Conduit
import qualified Data.Conduit.Binary        as Conduit
import           Network.AWS.Generics.XML
import           Network.AWS.Internal.Types
import           Network.HTTP.Conduit
import           Network.HTTP.Types

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
               => (LBS.ByteString -> Either y a)
               -> (LBS.ByteString -> Either x e)
               -> Response (ResumableSource AWS ByteString)
               -> AWS (Either e a)
decodeResponse success failure rs = do
    printDebug rs
    lbs <- responseBody rs $$+- Conduit.sinkLbs
    printDebug lbs
    x   <- f (statusIsSuccessful $ responseStatus rs) lbs
    printDebug x
    return x
  where
    f True  = fmap Right . awsEither . success
    f False = fmap Left  . awsEither . failure
