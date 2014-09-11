{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticTranscoder.CreatePreset
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreatePreset operation creates a preset with settings that you specify.
-- Elastic Transcoder checks the CreatePreset settings to ensure that they
-- meet Elastic Transcoder requirements and to determine whether they comply
-- with H.264 standards. If your settings are not valid for Elastic
-- Transcoder, Elastic Transcoder returns an HTTP 400 response
-- (ValidationException) and does not create the preset. If the settings are
-- valid for Elastic Transcoder but aren't strictly compliant with the H.264
-- standard, Elastic Transcoder creates the preset and returns a warning
-- message in the response. This helps you determine whether your settings
-- comply with the H.264 standard while giving you greater flexibility with
-- respect to the video that Elastic Transcoder produces. Elastic Transcoder
-- uses the H.264 video-compression format. For more information, see the
-- International Telecommunication Union publication Recommendation ITU-T
-- H.264: Advanced video coding for generic audiovisual services. POST
-- /2012-09-25/presets HTTP/1.1 Content-Type: application/json; charset=UTF-8
-- Accept: */* Host: elastictranscoder.[Elastic
-- Transcoder-endpoint].amazonaws.com:443 x-amz-date: 20130114T174952Z
-- Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] Content-Length:
-- [number-of-characters-in-JSON-string] { "Name":"DefaultPreset",
-- "Description":"Use for published videos", "Container":"mp4", "Audio":{
-- "Codec":"AAC", "SampleRate":"44100", "BitRate":"96", "Channels":"2" },
-- "Video":{ "Codec":"H.264", "CodecOptions":{ "Profile":"main",
-- "Level":"2.2", "MaxReferenceFrames":"3", "MaxBitRate":"", "BufferSize":""
-- }, "KeyframesMaxDist":"240", "FixedGOP":"false", "BitRate":"1600",
-- "FrameRate":"30", "MaxWidth": "auto", "MaxHeight": "auto", "SizingPolicy":
-- "Fit", "PaddingPolicy": "NoPad", "DisplayAspectRatio": "16:9",
-- "Watermarks":[ { "Id":"company logo", "MaxWidth":"20%", "MaxHeight":"20%",
-- "SizingPolicy":"ShrinkToFit", "HorizontalAlign":"Right",
-- "HorizontalOffset":"10px", "VerticalAlign":"Bottom",
-- "VerticalOffset":"10px", "Opacity":"55.5", "Target":"Content" } ]},
-- "Thumbnails":{ "Format":"png", "Interval":"120", "MaxWidth": "auto,
-- "MaxHeight": "auto", "SizingPolicy": "Fit", "PaddingPolicy": "NoPad" } }
-- Status: 201 Created x-amzn-RequestId: c321ec43-378e-11e2-8e4c-4d5b971203e9
-- Content-Type: application/json Content-Length:
-- [number-of-characters-in-response] Date: Mon, 14 Jan 2013 06:01:47 GMT {
-- "Preset":{ "Audio":{ "BitRate":"96", "Channels":"2", "Codec":"AAC",
-- "SampleRate":"44100" }, "Container":"mp4", "Description":"Use for published
-- videos", "Id":"5555555555555-abcde5", "Name":"DefaultPreset",
-- "Thumbnails":{ "Format":"png", "Interval":"120", "MaxWidth": "auto,
-- "MaxHeight": "auto", "SizingPolicy": "Fit", "PaddingPolicy": "NoPad" },
-- "Type":"Custom", "Video":{ "Codec":"H.264", "CodecOptions":{
-- "Profile":"main", "Level":"2.2", "MaxReferenceFrames":"3", "MaxBitRate":"",
-- "BufferSize":"" }, "KeyframesMaxDist":"240", "FixedGOP":"false",
-- "BitRate":"1600", "FrameRate":"30", "MaxWidth": "auto", "MaxHeight":
-- "auto", "SizingPolicy": "Fit", "PaddingPolicy": "NoPad",
-- "DisplayAspectRatio": "16:9", "Watermarks":[ { "Id":"company logo",
-- "MaxWidth":"20%", "MaxHeight":"20%", "SizingPolicy":"ShrinkToFit",
-- "HorizontalAlign":"Right", "HorizontalOffset":"10px",
-- "VerticalAlign":"Bottom", "VerticalOffset":"10px", "Opacity":"55.5",
-- "Target":"Content" } ] } }, "Warning":"" }.
module Network.AWS.ElasticTranscoder.CreatePreset
    (
    -- * Request
      CreatePreset
    -- ** Request constructor
    , mkCreatePreset
    -- ** Request lenses
    , cp1Name
    , cp1Description
    , cp1Container
    , cp1Video
    , cp1Audio
    , cp1Thumbnails

    -- * Response
    , CreatePresetResponse
    -- ** Response constructor
    , mkCreatePresetResponse
    -- ** Response lenses
    , cprrPreset
    , cprrWarning
    ) where

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The CreatePresetRequest structure.
data CreatePreset = CreatePreset
    { _cp1Name :: !Text
    , _cp1Description :: !(Maybe Text)
    , _cp1Container :: !Text
    , _cp1Video :: Maybe VideoParameters
    , _cp1Audio :: Maybe AudioParameters
    , _cp1Thumbnails :: Maybe Thumbnails
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreatePreset' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @Container ::@ @Text@
--
-- * @Video ::@ @Maybe VideoParameters@
--
-- * @Audio ::@ @Maybe AudioParameters@
--
-- * @Thumbnails ::@ @Maybe Thumbnails@
--
mkCreatePreset :: Text -- ^ 'cp1Name'
               -> Text -- ^ 'cp1Container'
               -> CreatePreset
mkCreatePreset p1 p3 = CreatePreset
    { _cp1Name = p1
    , _cp1Description = Nothing
    , _cp1Container = p3
    , _cp1Video = Nothing
    , _cp1Audio = Nothing
    , _cp1Thumbnails = Nothing
    }

-- | The name of the preset. We recommend that the name be unique within the AWS
-- account, but uniqueness is not enforced.
cp1Name :: Lens' CreatePreset Text
cp1Name = lens _cp1Name (\s a -> s { _cp1Name = a })

-- | A description of the preset.
cp1Description :: Lens' CreatePreset (Maybe Text)
cp1Description = lens _cp1Description (\s a -> s { _cp1Description = a })

-- | The container type for the output file. Valid values include mp3, mp4, ogg,
-- ts, and webm.
cp1Container :: Lens' CreatePreset Text
cp1Container = lens _cp1Container (\s a -> s { _cp1Container = a })

-- | A section of the request body that specifies the video parameters.
cp1Video :: Lens' CreatePreset (Maybe VideoParameters)
cp1Video = lens _cp1Video (\s a -> s { _cp1Video = a })

-- | A section of the request body that specifies the audio parameters.
cp1Audio :: Lens' CreatePreset (Maybe AudioParameters)
cp1Audio = lens _cp1Audio (\s a -> s { _cp1Audio = a })

-- | A section of the request body that specifies the thumbnail parameters, if
-- any.
cp1Thumbnails :: Lens' CreatePreset (Maybe Thumbnails)
cp1Thumbnails = lens _cp1Thumbnails (\s a -> s { _cp1Thumbnails = a })

instance ToPath CreatePreset

instance ToQuery CreatePreset

instance ToHeaders CreatePreset

instance ToJSON CreatePreset

-- | The CreatePresetResponse structure.
data CreatePresetResponse = CreatePresetResponse
    { _cprrPreset :: Maybe Preset
    , _cprrWarning :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreatePresetResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Preset ::@ @Maybe Preset@
--
-- * @Warning ::@ @Maybe Text@
--
mkCreatePresetResponse :: CreatePresetResponse
mkCreatePresetResponse = CreatePresetResponse
    { _cprrPreset = Nothing
    , _cprrWarning = Nothing
    }

-- | A section of the response body that provides information about the preset
-- that is created.
cprrPreset :: Lens' CreatePresetResponse (Maybe Preset)
cprrPreset = lens _cprrPreset (\s a -> s { _cprrPreset = a })

-- | If the preset settings don't comply with the standards for the video codec
-- but Elastic Transcoder created the preset, this message explains the reason
-- the preset settings don't meet the standard. Elastic Transcoder created the
-- preset because the settings might produce acceptable output.
cprrWarning :: Lens' CreatePresetResponse (Maybe Text)
cprrWarning = lens _cprrWarning (\s a -> s { _cprrWarning = a })

instance FromJSON CreatePresetResponse

instance AWSRequest CreatePreset where
    type Sv CreatePreset = ElasticTranscoder
    type Rs CreatePreset = CreatePresetResponse

    request = get
    response _ = jsonResponse