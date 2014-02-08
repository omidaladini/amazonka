{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteKeyPair
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeleteKeyPair operation deletes a key pair.
module Network.AWS.EC2.DeleteKeyPair where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteKeyPair :: Text
              -- ^ The name of the Amazon EC2 key pair to delete.
              -> DeleteKeyPair
deleteKeyPair p1 = DeleteKeyPair
    { dkprKeyName = p1
    , dkprDryRun = Nothing
    }

data DeleteKeyPair = DeleteKeyPair
    { dkprDryRun :: Maybe Bool
    , dkprKeyName :: !Text
      -- ^ The name of the Amazon EC2 key pair to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteKeyPair

instance AWSRequest DeleteKeyPair where
    type Er DeleteKeyPair = EC2Error
    type Rs DeleteKeyPair = DeleteKeyPairResponse
    request = getQuery service "DeleteKeyPair"

data DeleteKeyPairResponse = DeleteKeyPairResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteKeyPairResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteKeyPairResponse"
