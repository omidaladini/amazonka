{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CopyImage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Initiates the copy of an AMI from the specified source region to the region
-- in which the request was made. You specify the destination region by using
-- its endpoint when making the request. AMIs that use encrypted Amazon EBS
-- snapshots cannot be copied with this method. For more information, see
-- Copying AMIs in the Amazon Elastic Compute Cloud User Guide. Example This
-- example request copies the AMI in us-west-2 with the ID ami-1a2b3c4d,
-- naming the new AMI My-Standard-AMI.
-- https://ec2.amazonaws.com/?Action=CopyImage &amp;SourceRegion=us-west-2
-- &amp;SourceImageId=ami-1a2b3c4d &amp;Name=My-Standard-AMI
-- &amp;Description=This%20is%20the%20new%20version%20of%20My-Standard-AMI
-- &amp;ClientToken=550e8400-e29b-41d4-a716-446655440000 &amp;AUTHPARAMS
-- &lt;CopyImageResponse xmlns="http://ec2.amazonaws.com/doc/2014-02-01/"&gt;
-- &lt;requestId&gt;60bc441d-fa2c-494d-b155-5d6a3EXAMPLE&lt;/requestId&gt;
-- &lt;imageId&gt;ami-4d3c2b1a&lt;/imageId&gt; &lt;/CopyImageResponse&gt;.
module Network.AWS.EC2.CopyImage
    (
    -- * Request
      CopyImage
    -- ** Request constructor
    , mkCopyImage
    -- ** Request lenses
    , ciSourceRegion
    , ciSourceImageId
    , ciName
    , ciDescription
    , ciClientToken

    -- * Response
    , CopyImageResponse
    -- ** Response constructor
    , mkCopyImageResponse
    -- ** Response lenses
    , cirImageId
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data CopyImage = CopyImage
    { _ciSourceRegion :: !Text
    , _ciSourceImageId :: !Text
    , _ciName :: !(Maybe Text)
    , _ciDescription :: !(Maybe Text)
    , _ciClientToken :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CopyImage' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SourceRegion ::@ @Text@
--
-- * @SourceImageId ::@ @Text@
--
-- * @Name ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @ClientToken ::@ @Maybe Text@
--
mkCopyImage :: Text -- ^ 'ciSourceRegion'
            -> Text -- ^ 'ciSourceImageId'
            -> CopyImage
mkCopyImage p1 p2 = CopyImage
    { _ciSourceRegion = p1
    , _ciSourceImageId = p2
    , _ciName = Nothing
    , _ciDescription = Nothing
    , _ciClientToken = Nothing
    }

-- | The name of the region that contains the AMI to copy.
ciSourceRegion :: Lens' CopyImage Text
ciSourceRegion = lens _ciSourceRegion (\s a -> s { _ciSourceRegion = a })

-- | The ID of the AMI to copy.
ciSourceImageId :: Lens' CopyImage Text
ciSourceImageId = lens _ciSourceImageId (\s a -> s { _ciSourceImageId = a })

-- | The name of the new AMI in the destination region.
ciName :: Lens' CopyImage (Maybe Text)
ciName = lens _ciName (\s a -> s { _ciName = a })

-- | A description for the new AMI in the destination region.
ciDescription :: Lens' CopyImage (Maybe Text)
ciDescription = lens _ciDescription (\s a -> s { _ciDescription = a })

-- | Unique, case-sensitive identifier you provide to ensure idempotency of the
-- request. For more information, see How to Ensure Idempotency in the Amazon
-- Elastic Compute Cloud User Guide.
ciClientToken :: Lens' CopyImage (Maybe Text)
ciClientToken = lens _ciClientToken (\s a -> s { _ciClientToken = a })

instance ToQuery CopyImage where
    toQuery = genericQuery def

newtype CopyImageResponse = CopyImageResponse
    { _cirImageId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CopyImageResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ImageId ::@ @Maybe Text@
--
mkCopyImageResponse :: CopyImageResponse
mkCopyImageResponse = CopyImageResponse
    { _cirImageId = Nothing
    }

-- | The ID of the new AMI.
cirImageId :: Lens' CopyImageResponse (Maybe Text)
cirImageId = lens _cirImageId (\s a -> s { _cirImageId = a })

instance FromXML CopyImageResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CopyImage where
    type Sv CopyImage = EC2
    type Rs CopyImage = CopyImageResponse

    request = post "CopyImage"
    response _ = xmlResponse