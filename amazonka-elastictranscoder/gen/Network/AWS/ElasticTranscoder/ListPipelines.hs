{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticTranscoder.ListPipelines
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ListPipelines operation gets a list of the pipelines associated with
-- the current AWS account. GET /2012-09-25/pipelines HTTP/1.1 Content-Type:
-- charset=UTF-8 Accept: */* Host: elastictranscoder.[Elastic
-- Transcoder-endpoint].amazonaws.com:443 x-amz-date: 20130114T174952Z
-- Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] Status: 200 OK x-amzn-RequestId:
-- c321ec43-378e-11e2-8e4c-4d5b971203e9 Content-Type: application/json
-- Content-Length: [number-of-characters-in-response] Date: Mon, 14 Jan 2013
-- 06:01:47 GMT { "Pipelines":[ { "Id":"1111111111111-abcde1",
-- "Name":"Tokyo-Default",
-- "InputBucket":"salesoffice-tokyo.example.com-source",
-- "Role":"arn:aws:iam::123456789012:role/Elastic_Transcoder_Default_Role",
-- "Notifications":{ "Progressing":"", "Completed":"", "Warning":"",
-- "Error":"arn:aws:sns:us-east-1:111222333444:ETS_Errors" },
-- "ContentConfig":{ "Bucket":"salesoffice-tokyo.example.com-public-promos",
-- "Permissions":[ { "GranteeType":"Email",
-- "Grantee":"marketing-promos-tokyo@example.com", "Access":[ "FullControl" ]
-- } ], "StorageClass":"Standard" }, "ThumbnailConfig":{
-- "Bucket":"salesoffice-tokyo.example.com-public-promos-thumbnails",
-- "Permissions":[ { "GranteeType":"Email",
-- "Grantee":"marketing-promos-tokyo@example.com", "Access":[ "FullControl" ]
-- } ], "StorageClass":"ReducedRedundancy" }, "Status":"Active" }, {
-- "Id":"2222222222222-abcde2", "Name":"Amsterdam-Default",
-- "InputBucket":"salesoffice-amsterdam.example.com-source",
-- "Role":"arn:aws:iam::123456789012:role/Elastic_Transcoder_Default_Role",
-- "Notifications":{ "Progressing":"", "Completed":"", "Warning":"",
-- "Error":"arn:aws:sns:us-east-1:111222333444:ETS_Errors" },
-- "ContentConfig":{
-- "Bucket":"salesoffice-amsterdam.example.com-public-promos", "Permissions":[
-- { "GranteeType":"Email",
-- "Grantee":"marketing-promos-amsterdam@example.com", "Access":[
-- "FullControl" ] } ], "StorageClass":"Standard" }, nails",
-- "ThumbnailConfig":{
-- "Bucket":"salesoffice-amsterdam.example.com-public-promos-thumb
-- "Permissions":[ { "GranteeType":"Email",
-- "Grantee":"marketing-promos-amsterdam@example.com", "Access":[
-- "FullControl" ] } ], "StorageClass":"ReducedRedundancy" },
-- "Status":"Active" } ] }.
module Network.AWS.ElasticTranscoder.ListPipelines
    (
    -- * Request
      ListPipelines
    -- ** Request constructor
    , mkListPipelines
    -- ** Request lenses
    , lpAscending
    , lpPageToken

    -- * Response
    , ListPipelinesResponse
    -- ** Response constructor
    , mkListPipelinesResponse
    -- ** Response lenses
    , lprPipelines
    , lprNextPageToken
    ) where

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The ListPipelineRequest structure.
data ListPipelines = ListPipelines
    { _lpAscending :: !(Maybe Text)
    , _lpPageToken :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListPipelines' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Ascending ::@ @Maybe Text@
--
-- * @PageToken ::@ @Maybe Text@
--
mkListPipelines :: ListPipelines
mkListPipelines = ListPipelines
    { _lpAscending = Nothing
    , _lpPageToken = Nothing
    }

-- | To list pipelines in chronological order by the date and time that they
-- were created, enter true. To list pipelines in reverse chronological order,
-- enter false.
lpAscending :: Lens' ListPipelines (Maybe Text)
lpAscending = lens _lpAscending (\s a -> s { _lpAscending = a })

-- | When Elastic Transcoder returns more than one page of results, use
-- pageToken in subsequent GET requests to get each successive page of
-- results.
lpPageToken :: Lens' ListPipelines (Maybe Text)
lpPageToken = lens _lpPageToken (\s a -> s { _lpPageToken = a })

instance ToPath ListPipelines

instance ToQuery ListPipelines

instance ToHeaders ListPipelines

instance ToJSON ListPipelines

-- | A list of the pipelines associated with the current AWS account.
data ListPipelinesResponse = ListPipelinesResponse
    { _lprPipelines :: [Pipeline]
    , _lprNextPageToken :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListPipelinesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Pipelines ::@ @[Pipeline]@
--
-- * @NextPageToken ::@ @Maybe Text@
--
mkListPipelinesResponse :: ListPipelinesResponse
mkListPipelinesResponse = ListPipelinesResponse
    { _lprPipelines = mempty
    , _lprNextPageToken = Nothing
    }

-- | An array of Pipeline objects.
lprPipelines :: Lens' ListPipelinesResponse [Pipeline]
lprPipelines = lens _lprPipelines (\s a -> s { _lprPipelines = a })

-- | A value that you use to access the second and subsequent pages of results,
-- if any. When the pipelines fit on one page or when you've reached the last
-- page of results, the value of NextPageToken is null.
lprNextPageToken :: Lens' ListPipelinesResponse (Maybe Text)
lprNextPageToken =
    lens _lprNextPageToken (\s a -> s { _lprNextPageToken = a })

instance FromJSON ListPipelinesResponse

instance AWSRequest ListPipelines where
    type Sv ListPipelines = ElasticTranscoder
    type Rs ListPipelines = ListPipelinesResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListPipelines where
    next rq rs = (\x -> rq & lpPageToken ?~ x)
        <$> (rs ^. lprNextPageToken)