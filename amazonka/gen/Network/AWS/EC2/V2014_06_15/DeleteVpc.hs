{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DeleteVpc
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified VPC. You must detach or delete all gateways and
-- resources that are associated with the VPC before you can delete it. For
-- example, you must terminate all instances running in the VPC, delete all
-- security groups associated with the VPC (except the default one), delete
-- all route tables associated with the VPC (except the default one), and so
-- on. Example This example deletes the specified VPC.
-- https://ec2.amazonaws.com/?Action=DeleteVpc &amp;VpcId=vpc-1a2b3c4d
-- &amp;AUTHPARAMS &lt;DeleteVpcResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteVpcResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DeleteVpc where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteVpc' request.
deleteVpc :: Text -- ^ '_dvwVpcId'
          -> DeleteVpc
deleteVpc p1 = DeleteVpc
    { _dvwVpcId = p1
    , _dvwDryRun = Nothing
    }

data DeleteVpc = DeleteVpc
    { _dvwVpcId :: Text
      -- ^ The ID of the VPC.
    , _dvwDryRun :: Maybe Bool
      -- ^ 
    } deriving (Show, Generic)

makeLenses ''DeleteVpc

instance ToQuery DeleteVpc where
    toQuery = genericToQuery def

data DeleteVpcResponse = DeleteVpcResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteVpcResponse

instance AWSRequest DeleteVpc where
    type Sv DeleteVpc = EC2
    type Rs DeleteVpc = DeleteVpcResponse

    request = post "DeleteVpc"
    response _ = nullaryResponse DeleteVpcResponse