{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.V2014_06_15.BundleInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Bundles an Amazon instance store-backed Windows instance. During bundling,
-- only the root device volume (C:\) is bundled. Data on other instance store
-- volumes is not preserved. This procedure is not applicable for Linux/Unix
-- instances or Windows instances that are backed by Amazon EBS. For more
-- information, see Creating an Instance Store-Backed Windows AMI. Example
-- This example request bundles the specified instance. Before you specify a
-- value for your access key ID, review and follow the guidance in Best
-- Practices for Managing AWS Access Keys.
-- https://ec2.amazonaws.com/?Action=BundleInstance &amp;InstanceId=i-e468cd8d
-- &amp;Storage.S3.AWSAccessKeyId='AKIAIOSFODNN7EXAMPLE'
-- &amp;Storage.S3.Bucket=myawsbucket &amp;Storage.S3.Prefix=winami
-- &amp;Storage.S3.UploadPolicy=eyJleHBpcmF0aW9uIjogIjIwMDgtMDgtMzBUMDg6NDk6MD
-- laIiwiY29uZGl0aW9ucyI6IFt7ImJ1Y2tldCI6ICJteS1idWNrZXQifSxbInN0YXJ0cy13aXRoIiwgI
-- iRrZXkiLCAibXktbmV3LWltYWdlIl0seyJhY2wiOiAiZWMyLWJ1bmRsZS1yZWFkIn1dfEXAMPLE
-- &amp;Storage.S3.UploadPolicySignature=fh5tyyyQD8W4COEthj3nlGNEXAMPLE
-- &amp;AUTHPARAMS &lt;BundleInstanceResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-02-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;bundleInstanceTask&gt; &lt;instanceId&gt;i-12345678&lt;/instanceId&gt;
-- &lt;bundleId&gt;bun-c1a540a8&lt;/bundleId&gt;
-- &lt;state&gt;bundling&lt;/state&gt;
-- &lt;startTime&gt;2008-10-07T11:41:50.000Z&lt;/startTime&gt;
-- &lt;updateTime&gt;2008-10-07T11:51:50.000Z&lt;/updateTime&gt;
-- &lt;progress&gt;70%&lt;/progress&gt; &lt;storage&gt; &lt;S3&gt;
-- &lt;bucket&gt;myawsbucket&lt;/bucket&gt;
-- &lt;prefix&gt;winami&lt;/prefix&gt; &lt;/S3&gt; &lt;/storage&gt;
-- &lt;/bundleInstanceTask&gt; &lt;/BundleInstanceResponse&gt;.
module Network.AWS.EC2.V2014_06_15.BundleInstance where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'BundleInstance' request.
bundleInstance :: Storage -- ^ '_birStorage'
               -> Text -- ^ '_birInstanceId'
               -> BundleInstance
bundleInstance p1 p2 = BundleInstance
    { _birStorage = p1
    , _birInstanceId = p2
    , _birDryRun = Nothing
    }

data BundleInstance = BundleInstance
    { _birStorage :: Storage
      -- ^ The bucket in which to store the AMI. You can specify a bucket
      -- that you already own or a new bucket that Amazon EC2 creates on
      -- your behalf. If you specify a bucket that belongs to someone
      -- else, Amazon EC2 returns an error.
    , _birInstanceId :: Text
      -- ^ The ID of the instance to bundle.
    , _birDryRun :: Maybe Bool
      -- ^ 
    } deriving (Show, Generic)

makeLenses ''BundleInstance

instance ToQuery BundleInstance where
    toQuery = genericToQuery def

data BundleInstanceResponse = BundleInstanceResponse
    { _bisBundleTask :: Maybe BundleTask
      -- ^ Information about the bundle task.
    } deriving (Show, Generic)

makeLenses ''BundleInstanceResponse

instance AWSRequest BundleInstance where
    type Sv BundleInstance = EC2
    type Rs BundleInstance = BundleInstanceResponse

    request = post "BundleInstance"
    response _ = cursorResponse $ \hs xml ->
        pure BundleInstanceResponse
            <*> xml %|? "BundleTask"