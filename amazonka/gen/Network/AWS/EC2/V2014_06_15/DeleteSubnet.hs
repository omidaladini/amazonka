{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DeleteSubnet
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified subnet. You must terminate all running instances in
-- the subnet before you can delete the subnet. Example This example deletes
-- the specified subnet. https://ec2.amazonaws.com/?Action=DeleteSubnet
-- &amp;SubnetId=subnet-9d4a7b6c &amp;AUTHPARAMS &lt;DeleteSubnetResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteSubnetResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DeleteSubnet where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteSubnet' request.
deleteSubnet :: Text -- ^ '_dswSubnetId'
             -> DeleteSubnet
deleteSubnet p1 = DeleteSubnet
    { _dswSubnetId = p1
    , _dswDryRun = Nothing
    }

data DeleteSubnet = DeleteSubnet
    { _dswSubnetId :: Text
      -- ^ The ID of the subnet.
    , _dswDryRun :: Maybe Bool
      -- ^ 
    } deriving (Show, Generic)

makeLenses ''DeleteSubnet

instance ToQuery DeleteSubnet where
    toQuery = genericToQuery def

data DeleteSubnetResponse = DeleteSubnetResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteSubnetResponse

instance AWSRequest DeleteSubnet where
    type Sv DeleteSubnet = EC2
    type Rs DeleteSubnet = DeleteSubnetResponse

    request = post "DeleteSubnet"
    response _ = nullaryResponse DeleteSubnetResponse