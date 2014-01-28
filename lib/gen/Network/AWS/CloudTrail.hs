-- Module      : Network.AWS.CloudTrail
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CloudTrail
    (
    -- * Operations
    -- ** DescribeTrails
      module Network.AWS.CloudTrail.DescribeTrails
    -- ** StopLogging
    , module Network.AWS.CloudTrail.StopLogging
    -- ** DeleteTrail
    , module Network.AWS.CloudTrail.DeleteTrail
    -- ** UpdateTrail
    , module Network.AWS.CloudTrail.UpdateTrail
    -- ** CreateTrail
    , module Network.AWS.CloudTrail.CreateTrail
    -- ** GetTrailStatus
    , module Network.AWS.CloudTrail.GetTrailStatus
    -- ** StartLogging
    , module Network.AWS.CloudTrail.StartLogging

    -- * Types
    -- ** Trail
    , Trail (..)

    -- * Errors
    , CloudTrailError (..)
    ) where

import Network.AWS.CloudTrail.Service
import Network.AWS.CloudTrail.Types

import Network.AWS.CloudTrail.DescribeTrails
import Network.AWS.CloudTrail.StopLogging
import Network.AWS.CloudTrail.DeleteTrail
import Network.AWS.CloudTrail.UpdateTrail
import Network.AWS.CloudTrail.CreateTrail
import Network.AWS.CloudTrail.GetTrailStatus
import Network.AWS.CloudTrail.StartLogging
