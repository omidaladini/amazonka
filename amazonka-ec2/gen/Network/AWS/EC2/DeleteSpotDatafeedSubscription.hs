{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteSpotDatafeedSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the datafeed for Spot Instances. For more information, see Spot
-- Instances in the Amazon Elastic Compute Cloud User Guide. Example This
-- example request deletes the datafeed for the AWS account.
-- https://ec2.amazonaws.com/?Action=DeleteSpotDatafeedSubscription
-- &amp;AUTHPARAMS &lt;DeleteSpotDatafeedSubscriptionResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/DeleteSpotDatafeedSubscriptionResponse&gt;.
module Network.AWS.EC2.DeleteSpotDatafeedSubscription
    (
    -- * Request
      DeleteSpotDatafeedSubscription
    -- ** Request constructor
    , mkDeleteSpotDatafeedSubscription
    -- * Response
    , DeleteSpotDatafeedSubscriptionResponse
    -- ** Response constructor
    , mkDeleteSpotDatafeedSubscriptionResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data DeleteSpotDatafeedSubscription = DeleteSpotDatafeedSubscription
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteSpotDatafeedSubscription' request.
mkDeleteSpotDatafeedSubscription :: DeleteSpotDatafeedSubscription
mkDeleteSpotDatafeedSubscription = DeleteSpotDatafeedSubscription

instance ToQuery DeleteSpotDatafeedSubscription where
    toQuery = genericQuery def

data DeleteSpotDatafeedSubscriptionResponse = DeleteSpotDatafeedSubscriptionResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteSpotDatafeedSubscriptionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteSpotDatafeedSubscriptionResponse :: DeleteSpotDatafeedSubscriptionResponse
mkDeleteSpotDatafeedSubscriptionResponse = DeleteSpotDatafeedSubscriptionResponse

instance AWSRequest DeleteSpotDatafeedSubscription where
    type Sv DeleteSpotDatafeedSubscription = EC2
    type Rs DeleteSpotDatafeedSubscription = DeleteSpotDatafeedSubscriptionResponse

    request = post "DeleteSpotDatafeedSubscription"
    response _ = nullaryResponse DeleteSpotDatafeedSubscriptionResponse