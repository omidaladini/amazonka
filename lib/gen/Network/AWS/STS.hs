-- Module      : Network.AWS.STS
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.STS
    (
    -- * Operations
    -- ** AssumeRole
      module Network.AWS.STS.AssumeRole
    -- ** DecodeAuthorizationMessage
    , module Network.AWS.STS.DecodeAuthorizationMessage
    -- ** AssumeRoleWithWebIdentity
    , module Network.AWS.STS.AssumeRoleWithWebIdentity
    -- ** GetFederationToken
    , module Network.AWS.STS.GetFederationToken
    -- ** GetSessionToken
    , module Network.AWS.STS.GetSessionToken
    -- ** AssumeRoleWithSAML
    , module Network.AWS.STS.AssumeRoleWithSAML

    -- * Types
    -- ** FederatedUser
    , FederatedUser (..)
    -- ** Credentials
    , Credentials (..)
    -- ** AssumedRoleUser
    , AssumedRoleUser (..)

    -- * Errors
    , STSError (..)
    ) where

import Network.AWS.STS.Service
import Network.AWS.STS.Types

import Network.AWS.STS.AssumeRole
import Network.AWS.STS.DecodeAuthorizationMessage
import Network.AWS.STS.AssumeRoleWithWebIdentity
import Network.AWS.STS.GetFederationToken
import Network.AWS.STS.GetSessionToken
import Network.AWS.STS.AssumeRoleWithSAML
