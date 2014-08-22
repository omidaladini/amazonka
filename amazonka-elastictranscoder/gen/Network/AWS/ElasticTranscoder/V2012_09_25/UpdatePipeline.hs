{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticTranscoder.V2012_09_25.UpdatePipeline
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Use the UpdatePipeline operation to update settings for a pipeline. When
-- you change pipeline settings, your changes take effect immediately. Jobs
-- that you have already submitted and that Elastic Transcoder has not started
-- to process are affected in addition to jobs that you submit after you
-- change settings.
module Network.AWS.ElasticTranscoder.V2012_09_25.UpdatePipeline where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.ElasticTranscoder.V2012_09_25.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'UpdatePipeline' request.
updatePipeline :: Text -- ^ '_uprId'
               -> UpdatePipeline
updatePipeline p1 = UpdatePipeline
    { _uprId = p1
    , _uprInputBucket = Nothing
    , _uprName = Nothing
    , _uprNotifications = Nothing
    , _uprContentConfig = Nothing
    , _uprThumbnailConfig = Nothing
    , _uprRole = Nothing
    }

data UpdatePipeline = UpdatePipeline
    { _uprId :: Text
      -- ^ The ID of the pipeline that you want to update.
    , _uprInputBucket :: Maybe Text
      -- ^ The Amazon S3 bucket in which you saved the media files that you
      -- want to transcode and the graphics that you want to use as
      -- watermarks.
    , _uprName :: Maybe Text
      -- ^ The name of the pipeline. We recommend that the name be unique
      -- within the AWS account, but uniqueness is not enforced.
      -- Constraints: Maximum 40 characters.
    , _uprNotifications :: Maybe Notifications
      -- ^ The Amazon Simple Notification Service (Amazon SNS) topic or
      -- topics to notify in order to report job status. To receive
      -- notifications, you must also subscribe to the new topic in the
      -- Amazon SNS console.
    , _uprContentConfig :: Maybe PipelineOutputConfig
      -- ^ The optional ContentConfig object specifies information about the
      -- Amazon S3 bucket in which you want Elastic Transcoder to save
      -- transcoded files and playlists: which bucket to use, which users
      -- you want to have access to the files, the type of access you want
      -- users to have, and the storage class that you want to assign to
      -- the files. If you specify values for ContentConfig, you must also
      -- specify values for ThumbnailConfig. If you specify values for
      -- ContentConfig and ThumbnailConfig, omit the OutputBucket object.
      -- Bucket: The Amazon S3 bucket in which you want Elastic Transcoder
      -- to save transcoded files and playlists. Permissions (Optional):
      -- The Permissions object specifies which users you want to have
      -- access to transcoded files and the type of access you want them
      -- to have. You can grant permissions to a maximum of 30 users
      -- and/or predefined Amazon S3 groups. Grantee Type: Specify the
      -- type of value that appears in the Grantee object: Canonical: The
      -- value in the Grantee object is either the canonical user ID for
      -- an AWS account or an origin access identity for an Amazon
      -- CloudFront distribution. For more information about canonical
      -- user IDs, see Access Control List (ACL) Overview in the Amazon
      -- Simple Storage Service Developer Guide. For more information
      -- about using CloudFront origin access identities to require that
      -- users use CloudFront URLs instead of Amazon S3 URLs, see Using an
      -- Origin Access Identity to Restrict Access to Your Amazon S3
      -- Content. A canonical user ID is not the same as an AWS account
      -- number. Email: The value in the Grantee object is the registered
      -- email address of an AWS account. Group: The value in the Grantee
      -- object is one of the following predefined Amazon S3 groups:
      -- AllUsers, AuthenticatedUsers, or LogDelivery. Grantee: The AWS
      -- user or group that you want to have access to transcoded files
      -- and playlists. To identify the user or group, you can specify the
      -- canonical user ID for an AWS account, an origin access identity
      -- for a CloudFront distribution, the registered email address of an
      -- AWS account, or a predefined Amazon S3 group Access: The
      -- permission that you want to give to the AWS user that you
      -- specified in Grantee. Permissions are granted on the files that
      -- Elastic Transcoder adds to the bucket, including playlists and
      -- video files. Valid values include: READ: The grantee can read the
      -- objects and metadata for objects that Elastic Transcoder adds to
      -- the Amazon S3 bucket. READ_ACP: The grantee can read the object
      -- ACL for objects that Elastic Transcoder adds to the Amazon S3
      -- bucket. WRITE_ACP: The grantee can write the ACL for the objects
      -- that Elastic Transcoder adds to the Amazon S3 bucket.
      -- FULL_CONTROL: The grantee has READ, READ_ACP, and WRITE_ACP
      -- permissions for the objects that Elastic Transcoder adds to the
      -- Amazon S3 bucket. StorageClass: The Amazon S3 storage class,
      -- Standard or ReducedRedundancy, that you want Elastic Transcoder
      -- to assign to the video files and playlists that it stores in your
      -- Amazon S3 bucket.
    , _uprThumbnailConfig :: Maybe PipelineOutputConfig
      -- ^ The ThumbnailConfig object specifies several values, including
      -- the Amazon S3 bucket in which you want Elastic Transcoder to save
      -- thumbnail files, which users you want to have access to the
      -- files, the type of access you want users to have, and the storage
      -- class that you want to assign to the files. If you specify values
      -- for ContentConfig, you must also specify values for
      -- ThumbnailConfig even if you don't want to create thumbnails. If
      -- you specify values for ContentConfig and ThumbnailConfig, omit
      -- the OutputBucket object. Bucket: The Amazon S3 bucket in which
      -- you want Elastic Transcoder to save thumbnail files. Permissions
      -- (Optional): The Permissions object specifies which users and/or
      -- predefined Amazon S3 groups you want to have access to thumbnail
      -- files, and the type of access you want them to have. You can
      -- grant permissions to a maximum of 30 users and/or predefined
      -- Amazon S3 groups. GranteeType: Specify the type of value that
      -- appears in the Grantee object: Canonical: The value in the
      -- Grantee object is either the canonical user ID for an AWS account
      -- or an origin access identity for an Amazon CloudFront
      -- distribution. A canonical user ID is not the same as an AWS
      -- account number. Email: The value in the Grantee object is the
      -- registered email address of an AWS account. Group: The value in
      -- the Grantee object is one of the following predefined Amazon S3
      -- groups: AllUsers, AuthenticatedUsers, or LogDelivery. Grantee:
      -- The AWS user or group that you want to have access to thumbnail
      -- files. To identify the user or group, you can specify the
      -- canonical user ID for an AWS account, an origin access identity
      -- for a CloudFront distribution, the registered email address of an
      -- AWS account, or a predefined Amazon S3 group. Access: The
      -- permission that you want to give to the AWS user that you
      -- specified in Grantee. Permissions are granted on the thumbnail
      -- files that Elastic Transcoder adds to the bucket. Valid values
      -- include: READ: The grantee can read the thumbnails and metadata
      -- for objects that Elastic Transcoder adds to the Amazon S3 bucket.
      -- READ_ACP: The grantee can read the object ACL for thumbnails that
      -- Elastic Transcoder adds to the Amazon S3 bucket. WRITE_ACP: The
      -- grantee can write the ACL for the thumbnails that Elastic
      -- Transcoder adds to the Amazon S3 bucket. FULL_CONTROL: The
      -- grantee has READ, READ_ACP, and WRITE_ACP permissions for the
      -- thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
      -- StorageClass: The Amazon S3 storage class, Standard or
      -- ReducedRedundancy, that you want Elastic Transcoder to assign to
      -- the thumbnails that it stores in your Amazon S3 bucket.
    , _uprRole :: Maybe Text
      -- ^ The IAM Amazon Resource Name (ARN) for the role that you want
      -- Elastic Transcoder to use to transcode jobs for this pipeline.
    } deriving (Show, Generic)

makeLenses ''UpdatePipeline

instance ToPath UpdatePipeline where
    toPath UpdatePipeline{..} = mconcat
        [ "/2012-09-25/pipelines/"
        , toBS _uprId
        ]

instance ToQuery UpdatePipeline

instance ToHeaders UpdatePipeline

instance ToJSON UpdatePipeline

data UpdatePipelineResponse = UpdatePipelineResponse
    { _upsPipeline :: Pipeline
      -- ^ The pipeline (queue) that is used to manage jobs.
    } deriving (Show, Generic)

makeLenses ''UpdatePipelineResponse

instance FromJSON UpdatePipelineResponse

instance AWSRequest UpdatePipeline where
    type Sv UpdatePipeline = ElasticTranscoder
    type Rs UpdatePipeline = UpdatePipelineResponse

    request = put
    response _ = jsonResponse