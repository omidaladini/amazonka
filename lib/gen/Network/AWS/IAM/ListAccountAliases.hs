{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ListAccountAliases
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the account aliases associated with the account. For information
-- about using an AWS account alias, see Using an Alias for Your AWS Account
-- ID in Using AWS Identity and Access Management. You can paginate the
-- results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=ListAccountAliases &Version=2010-05-08
-- &AUTHPARAMS false foocorporation c5a076e9-f1b0-11df-8fbe-45274EXAMPLE.
module Network.AWS.IAM.ListAccountAliases where

import           Control.Monad
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import           Text.XML.Generic

import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields where applicable.
listAccountAliases :: AWS (Either IAMError ListAccountAliasesResponse)
listAccountAliases = undefined $ ListAccountAliases
    { laarMarker = Nothing
    , laarMaxItems = Nothing
    }

data ListAccountAliases = ListAccountAliases
    { laarMarker :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent request
      -- after you've received a response where the results are truncated. Set it to
      -- the value of the Marker element in the response you just received.
    , laarMaxItems :: Maybe Int
      -- ^ Use this only when paginating results to indicate the maximum number of
      -- account aliases you want in the response. If there are additional account
      -- aliases beyond the maximum you specify, the IsTruncated response element is
      -- true. This parameter is optional. If you do not include it, it defaults to
      -- 100.
    } deriving (Eq, Show, Generic)

instance ToQuery ListAccountAliases

instance AWSRequest ListAccountAliases where
    type Er ListAccountAliases = IAMError
    type Rs ListAccountAliases = ListAccountAliasesResponse
    request = getQuery service "ListAccountAliases"

instance AWSPager ListAccountAliases where
    next rq rs
        | Just x <- laarrsMarker rs = Just $ rq { laarMarker = Just x }
        | otherwise = Nothing

data ListAccountAliasesResponse = ListAccountAliasesResponse
    { laarrsAccountAliases :: [Text]
      -- ^ A list of aliases associated with the account.
    , laarrsIsTruncated :: Maybe Bool
      -- ^ A flag that indicates whether there are more account aliases to list. If
      -- your results were truncated, you can make a subsequent pagination request
      -- using the Marker request parameter to retrieve more account aliases in the
      -- list.
    , laarrsMarker :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent request
      -- after you've received a response where the results are truncated. Set it to
      -- the value of the Marker element in the response you just received.
    } deriving (Eq, Show, Generic)

instance FromXML ListAccountAliasesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ListAccountAliasesResponse"
        :| ["ListAccountAliasesResult"]
