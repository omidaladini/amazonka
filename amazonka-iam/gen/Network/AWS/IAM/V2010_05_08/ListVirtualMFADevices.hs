{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.ListVirtualMFADevices
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the virtual MFA devices under the AWS account by assignment status.
-- If you do not specify an assignment status, the action returns a list of
-- all virtual MFA devices. Assignment status can be Assigned, Unassigned, or
-- Any. You can paginate the results using the MaxItems and Marker parameters.
-- the AssignmentStatus is Any --> https://iam.amazonaws.com/
-- ?Action=ListVirtualMFADevices &AssignmentStatus=Any &AUTHPARAMS associated
-- with the account: the first device is unassigned, the second is assigned to
-- the root account, and the third is assigned to a user named ExampleUser
-- under the account. --> false arn:aws:iam::123456789012:mfa/MFAdeviceName
-- arn:aws:iam::123456789012:mfa/RootMFAdeviceName 2011-10-20T20:49:03Z
-- 123456789012 arn:aws:iam::123456789012:root 2009-10-13T22:00:36Z
-- arn:aws:iam:::mfa/ExampleUserMFAdeviceName 2011-10-31T20:45:02Z
-- AIDEXAMPLE4EXAMPLEXYZ / ExampleUser
-- arn:aws:iam::111122223333:user/ExampleUser 2011-07-01T17:23:07Z
-- b61ce1b1-0401-11e1-b2f8-2dEXAMPLEbfc.
module Network.AWS.IAM.V2010_05_08.ListVirtualMFADevices
    (
    -- * Request
      ListVirtualMFADevices
    -- ** Request constructor
    , listVirtualMFADevices
    -- ** Request lenses
    , lvmfadrAssignmentStatus
    , lvmfadrMarker
    , lvmfadrMaxItems

    -- * Response
    , ListVirtualMFADevicesResponse
    -- ** Response lenses
    , lvmfadsIsTruncated
    , lvmfadsVirtualMFADevices
    , lvmfadsMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListVirtualMFADevices' request.
listVirtualMFADevices :: ListVirtualMFADevices
listVirtualMFADevices = ListVirtualMFADevices
    { _lvmfadrAssignmentStatus = Nothing
    , _lvmfadrMarker = Nothing
    , _lvmfadrMaxItems = Nothing
    }

data ListVirtualMFADevices = ListVirtualMFADevices
    { _lvmfadrAssignmentStatus :: Maybe AssignmentStatusType
      -- ^ The status (unassigned or assigned) of the devices to list. If
      -- you do not specify an AssignmentStatus, the action defaults to
      -- Any which lists both assigned and unassigned virtual MFA devices.
    , _lvmfadrMarker :: Maybe Text
      -- ^ Use this parameter only when paginating results, and only in a
      -- subsequent request after you've received a response where the
      -- results are truncated. Set it to the value of the Marker element
      -- in the response you just received.
    , _lvmfadrMaxItems :: Maybe Integer
      -- ^ Use this parameter only when paginating results to indicate the
      -- maximum number of user names you want in the response. If there
      -- are additional user names beyond the maximum you specify, the
      -- IsTruncated response element is true. This parameter is optional.
      -- If you do not include it, it defaults to 100.
    } deriving (Show, Generic)

-- | The status (unassigned or assigned) of the devices to list. If you do not
-- specify an AssignmentStatus, the action defaults to Any which lists both
-- assigned and unassigned virtual MFA devices.
lvmfadrAssignmentStatus
    :: Functor f
    => (Maybe AssignmentStatusType
    -> f (Maybe AssignmentStatusType))
    -> ListVirtualMFADevices
    -> f ListVirtualMFADevices
lvmfadrAssignmentStatus f x =
    (\y -> x { _lvmfadrAssignmentStatus = y })
       <$> f (_lvmfadrAssignmentStatus x)
{-# INLINE lvmfadrAssignmentStatus #-}

-- | Use this parameter only when paginating results, and only in a subsequent
-- request after you've received a response where the results are truncated.
-- Set it to the value of the Marker element in the response you just
-- received.
lvmfadrMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListVirtualMFADevices
    -> f ListVirtualMFADevices
lvmfadrMarker f x =
    (\y -> x { _lvmfadrMarker = y })
       <$> f (_lvmfadrMarker x)
{-# INLINE lvmfadrMarker #-}

-- | Use this parameter only when paginating results to indicate the maximum
-- number of user names you want in the response. If there are additional user
-- names beyond the maximum you specify, the IsTruncated response element is
-- true. This parameter is optional. If you do not include it, it defaults to
-- 100.
lvmfadrMaxItems
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ListVirtualMFADevices
    -> f ListVirtualMFADevices
lvmfadrMaxItems f x =
    (\y -> x { _lvmfadrMaxItems = y })
       <$> f (_lvmfadrMaxItems x)
{-# INLINE lvmfadrMaxItems #-}

instance ToQuery ListVirtualMFADevices where
    toQuery = genericQuery def

data ListVirtualMFADevicesResponse = ListVirtualMFADevicesResponse
    { _lvmfadsIsTruncated :: Bool
      -- ^ A flag that indicates whether there are more items to list. If
      -- your results were truncated, you can make a subsequent pagination
      -- request using the Marker request parameter to retrieve more items
      -- the list.
    , _lvmfadsVirtualMFADevices :: [VirtualMFADevice]
    , _lvmfadsMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    } deriving (Show, Generic)

-- | A flag that indicates whether there are more items to list. If your results
-- were truncated, you can make a subsequent pagination request using the
-- Marker request parameter to retrieve more items the list.
lvmfadsIsTruncated
    :: Functor f
    => (Bool
    -> f (Bool))
    -> ListVirtualMFADevicesResponse
    -> f ListVirtualMFADevicesResponse
lvmfadsIsTruncated f x =
    (\y -> x { _lvmfadsIsTruncated = y })
       <$> f (_lvmfadsIsTruncated x)
{-# INLINE lvmfadsIsTruncated #-}

lvmfadsVirtualMFADevices
    :: Functor f
    => ([VirtualMFADevice]
    -> f ([VirtualMFADevice]))
    -> ListVirtualMFADevicesResponse
    -> f ListVirtualMFADevicesResponse
lvmfadsVirtualMFADevices f x =
    (\y -> x { _lvmfadsVirtualMFADevices = y })
       <$> f (_lvmfadsVirtualMFADevices x)
{-# INLINE lvmfadsVirtualMFADevices #-}

-- | If IsTruncated is true, this element is present and contains the value to
-- use for the Marker parameter in a subsequent pagination request.
lvmfadsMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListVirtualMFADevicesResponse
    -> f ListVirtualMFADevicesResponse
lvmfadsMarker f x =
    (\y -> x { _lvmfadsMarker = y })
       <$> f (_lvmfadsMarker x)
{-# INLINE lvmfadsMarker #-}

instance FromXML ListVirtualMFADevicesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListVirtualMFADevices where
    type Sv ListVirtualMFADevices = IAM
    type Rs ListVirtualMFADevices = ListVirtualMFADevicesResponse

    request = post "ListVirtualMFADevices"
    response _ = xmlResponse

instance AWSPager ListVirtualMFADevices where
    next rq rs
        | not (_lvmfadsIsTruncated rs) = Nothing
        | otherwise = Just $ rq
            { _lvmfadrMarker = _lvmfadsMarker rs
            }
