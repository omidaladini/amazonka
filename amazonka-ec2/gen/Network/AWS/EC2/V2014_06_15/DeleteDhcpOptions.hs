{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DeleteDhcpOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified set of DHCP options. You must disassociate the set of
-- DHCP options before you can delete it. You can disassociate the set of DHCP
-- options by associating either a new set of options or the default set of
-- options with the VPC. Example This example deletes the specified set of
-- DHCP options. https://ec2.amazonaws.com/?Action=DeleteDhcpOptions
-- &amp;DhcpOptionsId=dopt-7a8b9c2d &amp;AUTHPARAMS
-- &lt;DeleteDhcpOptionsResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteDhcpOptionsResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DeleteDhcpOptions
    (
    -- * Request
      DeleteDhcpOptions
    -- ** Request constructor
    , deleteDhcpOptions
    -- ** Request lenses
    , ddorDhcpOptionsId

    -- * Response
    , DeleteDhcpOptionsResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteDhcpOptions' request.
deleteDhcpOptions :: Text -- ^ 'ddorDhcpOptionsId'
                  -> DeleteDhcpOptions
deleteDhcpOptions p1 = DeleteDhcpOptions
    { _ddorDhcpOptionsId = p1
    }

data DeleteDhcpOptions = DeleteDhcpOptions
    { _ddorDhcpOptionsId :: Text
      -- ^ The ID of the DHCP options set.
    } deriving (Show, Generic)

-- | The ID of the DHCP options set.
ddorDhcpOptionsId
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteDhcpOptions
    -> f DeleteDhcpOptions
ddorDhcpOptionsId f x =
    (\y -> x { _ddorDhcpOptionsId = y })
       <$> f (_ddorDhcpOptionsId x)
{-# INLINE ddorDhcpOptionsId #-}

instance ToQuery DeleteDhcpOptions where
    toQuery = genericQuery def

data DeleteDhcpOptionsResponse = DeleteDhcpOptionsResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteDhcpOptions where
    type Sv DeleteDhcpOptions = EC2
    type Rs DeleteDhcpOptions = DeleteDhcpOptionsResponse

    request = post "DeleteDhcpOptions"
    response _ = nullaryResponse DeleteDhcpOptionsResponse
