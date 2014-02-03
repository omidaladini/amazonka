{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ListMFADevices
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the MFA devices. If the request includes the user name, then this
-- action lists all the MFA devices associated with the specified user name.
-- If you do not specify a user name, IAM determines the user name implicitly
-- based on the AWS access key ID signing the request. You can paginate the
-- results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=ListMFADevices &UserName=Bob &AUTHPARAMS
-- Bob R1234 false 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.ListMFADevices where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
listMFADevices :: ListMFADevices
listMFADevices = ListMFADevices
    { lmfadrMarker = Nothing
    , lmfadrMaxItems = Nothing
    , lmfadrUserName = Nothing
    }

data ListMFADevices = ListMFADevices
    { lmfadrMarker :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent request
      -- after you've received a response where the results are truncated. Set it to
      -- the value of the Marker element in the response you just received.
    , lmfadrMaxItems :: Maybe Int
      -- ^ Use this only when paginating results to indicate the maximum number of MFA
      -- devices you want in the response. If there are additional MFA devices
      -- beyond the maximum you specify, the IsTruncated response element is true.
      -- This parameter is optional. If you do not include it, it defaults to 100.
    , lmfadrUserName :: Maybe Text
      -- ^ Name of the user whose MFA devices you want to list.
    } deriving (Eq, Show, Generic)

instance ToQuery ListMFADevices

instance AWSRequest ListMFADevices where
    type Er ListMFADevices = IAMError
    type Rs ListMFADevices = ListMFADevicesResponse
    request = getQuery service "ListMFADevices"

instance AWSPager ListMFADevices where
    next rq rs
        | Just x <- lmfadrrsMarker rs = Just $ rq { lmfadrMarker = Just x }
        | otherwise = Nothing

data ListMFADevicesResponse = ListMFADevicesResponse
    { lmfadrrsIsTruncated :: Maybe Bool
      -- ^ A flag that indicates whether there are more MFA devices to list. If your
      -- results were truncated, you can make a subsequent pagination request using
      -- the Marker request parameter to retrieve more MFA devices in the list.
    , lmfadrrsMFADevices :: [MFADevice]
      -- ^ A list of MFA devices.
    , lmfadrrsMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the value to
      -- use for the Marker parameter in a subsequent pagination request.
    } deriving (Eq, Show, Generic)

instance FromXML ListMFADevicesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ListMFADevicesResponse"
        :| ["ListMFADevicesResult"]
