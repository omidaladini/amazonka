{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.ConfirmPublicVirtualInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Accept ownership of a public virtual interface created by another customer.
-- After the virtual interface owner calls this function, the specified
-- virtual interface will be created and made available for handling traffic.
module Network.AWS.DirectConnect.ConfirmPublicVirtualInterface where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.DirectConnect.Service
import Network.AWS.DirectConnect.Types

data ConfirmPublicVirtualInterface = ConfirmPublicVirtualInterface
    { cpvitVirtualInterfaceId :: !Text
      -- ^ ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
    } deriving (Eq, Show, Generic)

instance ToJSON ConfirmPublicVirtualInterface

instance AWSRequest ConfirmPublicVirtualInterface where
    type Er ConfirmPublicVirtualInterface = DirectConnectError
    type Rs ConfirmPublicVirtualInterface = ConfirmPublicVirtualInterfaceResponse
    request  = getJSON service
    response = responseJSON

data ConfirmPublicVirtualInterfaceResponse = ConfirmPublicVirtualInterfaceResponse
    { cpvitrsVirtualInterfaceState :: Maybe VirtualInterfaceState
      -- ^ State of the virtual interface. Confirming: The creation of the virtual
      -- interface is pending confirmation from the virtual interface owner. If the
      -- owner of the virtual interface is different from the owner of the
      -- connection on which it is provisioned, then the virtual interface will
      -- remain in this state until it is confirmed by the virtual interface owner.
      -- Verifying: This state only applies to public virtual interfaces. Each
      -- public virtual interface needs validation before the virtual interface can
      -- be created. Pending: A virtual interface is in this state from the time
      -- that it is created until the virtual interface is ready to forward traffic.
      -- Available: A virtual interface that is able to forward traffic. Deleting: A
      -- virtual interface is in this state immediately after calling
      -- DeleteVirtualInterface until it can no longer forward traffic. Deleted: A
      -- virtual interface that cannot forward traffic. Rejected: The virtual
      -- interface owner has declined creation of the virtual interface. If a
      -- virtual interface in the 'Confirming' state is deleted by the virtual
      -- interface owner, the virtual interface will enter the 'Rejected' state.
    } deriving (Eq, Show, Generic)

instance FromJSON ConfirmPublicVirtualInterfaceResponse