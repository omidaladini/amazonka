{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeImages
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeImages operation returns information about AMIs, AKIs, and ARIs
-- available to the user. Information returned includes image type, product
-- codes, architecture, and kernel and RAM disk IDs. Images available to the
-- user include public images available for any user to launch, private images
-- owned by the user making the request, and private images owned by other
-- users for which the user has explicit launch permissions. Launch
-- permissions fall into three categories: Public: The owner of the AMI
-- granted launch permissions for the AMI to the all group. All users have
-- launch permissions for these AMIs. Explicit: The owner of the AMI granted
-- launch permissions to a specific user. Implicit: A user has implicit launch
-- permissions for all AMIs he or she owns. The list of AMIs returned can be
-- modified by specifying AMI IDs, AMI owners, or users with launch
-- permissions. If no options are specified, Amazon EC2 returns all AMIs for
-- which the user has launch permissions. If you specify one or more AMI IDs,
-- only AMIs that have the specified IDs are returned. If you specify an
-- invalid AMI ID, a fault is returned. If you specify an AMI ID for which you
-- do not have access, it will not be included in the returned results. If you
-- specify one or more AMI owners, only AMIs from the specified owners and for
-- which you have access are returned. The results can include the account IDs
-- of the specified owners, amazon for AMIs owned by Amazon or self for AMIs
-- that you own. If you specify a list of executable users, only users that
-- have launch permissions for the AMIs are returned. You can specify account
-- IDs (if you own the AMI(s)), self for AMIs for which you own or have
-- explicit permissions, or all for public AMIs. Deregistered images are
-- included in the returned results for an unspecified interval after
-- deregistration.
module Network.AWS.EC2.DescribeImages where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeImages = DescribeImages
    { dieDryRun :: Maybe Bool
    , dieExecutableUsers :: [Text]
      -- ^ An optional list of users whose launch permissions will be used to scope
      -- the described AMIs. Valid values are: self : AMIs for which you have
      -- explicit launch permissions AWS account ID : AMIs for which this account ID
      -- has launch permissions all : AMIs that have public launch permissions The
      -- values self and all are literals.
    , dieFilters :: [Filter]
      -- ^ A list of filters used to match properties for Images. For a complete
      -- reference to the available filter keys for this operation, see the Amazon
      -- EC2 API reference.
    , dieImageIds :: [Text]
      -- ^ An optional list of the AMI IDs to describe. If not specified, all AMIs
      -- will be described.
    , dieOwners :: [Text]
      -- ^ An optional list of owners by which to scope the described AMIs. Valid
      -- values are: self : AMIs owned by you AWS account ID : AMIs owned by this
      -- account ID aws-marketplace : AMIs owned by the AWS Marketplace amazon :
      -- AMIs owned by Amazon all : Do not scope the AMIs returned by owner The
      -- values self, aws-marketplace, amazon, and all are literals.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeImages

instance AWSRequest DescribeImages where
    type Er DescribeImages = EC2Error
    type Rs DescribeImages = DescribeImagesResponse
    request = getQuery service "DescribeImages"

data DescribeImagesResponse = DescribeImagesResponse
    { dierImages :: [Image]
      -- ^ The list of the described AMIs.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeImagesResponse where
    fromXMLOptions = xmlOptions
