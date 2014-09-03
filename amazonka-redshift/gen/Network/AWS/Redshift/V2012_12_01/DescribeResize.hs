{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DescribeResize
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about the last resize operation for the specified
-- cluster. If no resize operation has ever been initiated for the specified
-- cluster, a HTTP 404 error is returned. If a resize operation was initiated
-- and completed, the status of the resize remains as SUCCEEDED until the next
-- resize. A resize operation can be requested using ModifyCluster and
-- specifying a different number or type of nodes for the cluster.
-- https://redshift.us-east-1.amazonaws.com/ ?Action=DescribeResize
-- &ClusterIdentifier=examplecluster &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121207/us-east-1/redshift/aws4_request
-- &x-amz-date=20121207T232427Z
-- &x-amz-signedheaders=content-type;host;x-amz-date multi-node SUCCEEDED
-- 6.5263 66922 0 users venue sales listing event date category 10254
-- dw1.xlarge 2 a6d59c61-a162-11e2-b2bc-fb54c9d11e09.
module Network.AWS.Redshift.V2012_12_01.DescribeResize
    (
    -- * Request
      DescribeResize
    -- ** Request constructor
    , describeResize
    -- ** Request lenses
    , drmClusterIdentifier

    -- * Response
    , DescribeResizeResponse
    -- ** Response lenses
    , rpmAvgResizeRateInMegaBytesPerSecond
    , rpmImportTablesCompleted
    , rpmImportTablesInProgress
    , rpmImportTablesNotStarted
    , rpmTargetNumberOfNodes
    , rpmTotalResizeDataInMegaBytes
    , rpmProgressInMegaBytes
    , rpmElapsedTimeInSeconds
    , rpmEstimatedTimeToCompletionInSeconds
    , rpmTargetNodeType
    , rpmTargetClusterType
    , rpmStatus
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeResize' request.
describeResize :: Text -- ^ 'drmClusterIdentifier'
               -> DescribeResize
describeResize p1 = DescribeResize
    { _drmClusterIdentifier = p1
    }

data DescribeResize = DescribeResize
    { _drmClusterIdentifier :: Text
      -- ^ The unique identifier of a cluster whose resize progress you are
      -- requesting. This parameter isn't case-sensitive. By default,
      -- resize operations for all clusters defined for an AWS account are
      -- returned.
    } deriving (Show, Generic)

-- | The unique identifier of a cluster whose resize progress you are
-- requesting. This parameter isn't case-sensitive. By default, resize
-- operations for all clusters defined for an AWS account are returned.
drmClusterIdentifier
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeResize
    -> f DescribeResize
drmClusterIdentifier f x =
    (\y -> x { _drmClusterIdentifier = y })
       <$> f (_drmClusterIdentifier x)
{-# INLINE drmClusterIdentifier #-}

instance ToQuery DescribeResize where
    toQuery = genericQuery def

data DescribeResizeResponse = DescribeResizeResponse
    { _rpmAvgResizeRateInMegaBytesPerSecond :: Maybe Double
      -- ^ The average rate of the resize operation over the last few
      -- minutes, measured in megabytes per second. After the resize
      -- operation completes, this value shows the average rate of the
      -- entire resize operation.
    , _rpmImportTablesCompleted :: [Text]
      -- ^ The names of tables that have been completely imported . Valid
      -- Values: List of table names.
    , _rpmImportTablesInProgress :: [Text]
      -- ^ The names of tables that are being currently imported. Valid
      -- Values: List of table names.
    , _rpmImportTablesNotStarted :: [Text]
      -- ^ The names of tables that have not been yet imported. Valid
      -- Values: List of table names.
    , _rpmTargetNumberOfNodes :: Maybe Integer
      -- ^ The number of nodes that the cluster will have after the resize
      -- operation is complete.
    , _rpmTotalResizeDataInMegaBytes :: Maybe Integer
      -- ^ The estimated total amount of data, in megabytes, on the cluster
      -- before the resize operation began.
    , _rpmProgressInMegaBytes :: Maybe Integer
      -- ^ While the resize operation is in progress, this value shows the
      -- current amount of data, in megabytes, that has been processed so
      -- far. When the resize operation is complete, this value shows the
      -- total amount of data, in megabytes, on the cluster, which may be
      -- more or less than TotalResizeDataInMegaBytes (the estimated total
      -- amount of data before resize).
    , _rpmElapsedTimeInSeconds :: Maybe Integer
      -- ^ The amount of seconds that have elapsed since the resize
      -- operation began. After the resize operation completes, this value
      -- shows the total actual time, in seconds, for the resize
      -- operation.
    , _rpmEstimatedTimeToCompletionInSeconds :: Maybe Integer
      -- ^ The estimated time remaining, in seconds, until the resize
      -- operation is complete. This value is calculated based on the
      -- average resize rate and the estimated amount of data remaining to
      -- be processed. Once the resize operation is complete, this value
      -- will be 0.
    , _rpmTargetNodeType :: Maybe Text
      -- ^ The node type that the cluster will have after the resize
      -- operation is complete.
    , _rpmTargetClusterType :: Maybe Text
      -- ^ The cluster type after the resize operation is complete. Valid
      -- Values: multi-node | single-node.
    , _rpmStatus :: Maybe Text
      -- ^ The status of the resize operation. Valid Values: NONE |
      -- IN_PROGRESS | FAILED | SUCCEEDED.
    } deriving (Show, Generic)

-- | The average rate of the resize operation over the last few minutes,
-- measured in megabytes per second. After the resize operation completes,
-- this value shows the average rate of the entire resize operation.
rpmAvgResizeRateInMegaBytesPerSecond
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> DescribeResizeResponse
    -> f DescribeResizeResponse
rpmAvgResizeRateInMegaBytesPerSecond f x =
    (\y -> x { _rpmAvgResizeRateInMegaBytesPerSecond = y })
       <$> f (_rpmAvgResizeRateInMegaBytesPerSecond x)
{-# INLINE rpmAvgResizeRateInMegaBytesPerSecond #-}

-- | The names of tables that have been completely imported . Valid Values: List
-- of table names.
rpmImportTablesCompleted
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DescribeResizeResponse
    -> f DescribeResizeResponse
rpmImportTablesCompleted f x =
    (\y -> x { _rpmImportTablesCompleted = y })
       <$> f (_rpmImportTablesCompleted x)
{-# INLINE rpmImportTablesCompleted #-}

-- | The names of tables that are being currently imported. Valid Values: List
-- of table names.
rpmImportTablesInProgress
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DescribeResizeResponse
    -> f DescribeResizeResponse
rpmImportTablesInProgress f x =
    (\y -> x { _rpmImportTablesInProgress = y })
       <$> f (_rpmImportTablesInProgress x)
{-# INLINE rpmImportTablesInProgress #-}

-- | The names of tables that have not been yet imported. Valid Values: List of
-- table names.
rpmImportTablesNotStarted
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DescribeResizeResponse
    -> f DescribeResizeResponse
rpmImportTablesNotStarted f x =
    (\y -> x { _rpmImportTablesNotStarted = y })
       <$> f (_rpmImportTablesNotStarted x)
{-# INLINE rpmImportTablesNotStarted #-}

-- | The number of nodes that the cluster will have after the resize operation
-- is complete.
rpmTargetNumberOfNodes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeResizeResponse
    -> f DescribeResizeResponse
rpmTargetNumberOfNodes f x =
    (\y -> x { _rpmTargetNumberOfNodes = y })
       <$> f (_rpmTargetNumberOfNodes x)
{-# INLINE rpmTargetNumberOfNodes #-}

-- | The estimated total amount of data, in megabytes, on the cluster before the
-- resize operation began.
rpmTotalResizeDataInMegaBytes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeResizeResponse
    -> f DescribeResizeResponse
rpmTotalResizeDataInMegaBytes f x =
    (\y -> x { _rpmTotalResizeDataInMegaBytes = y })
       <$> f (_rpmTotalResizeDataInMegaBytes x)
{-# INLINE rpmTotalResizeDataInMegaBytes #-}

-- | While the resize operation is in progress, this value shows the current
-- amount of data, in megabytes, that has been processed so far. When the
-- resize operation is complete, this value shows the total amount of data, in
-- megabytes, on the cluster, which may be more or less than
-- TotalResizeDataInMegaBytes (the estimated total amount of data before
-- resize).
rpmProgressInMegaBytes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeResizeResponse
    -> f DescribeResizeResponse
rpmProgressInMegaBytes f x =
    (\y -> x { _rpmProgressInMegaBytes = y })
       <$> f (_rpmProgressInMegaBytes x)
{-# INLINE rpmProgressInMegaBytes #-}

-- | The amount of seconds that have elapsed since the resize operation began.
-- After the resize operation completes, this value shows the total actual
-- time, in seconds, for the resize operation.
rpmElapsedTimeInSeconds
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeResizeResponse
    -> f DescribeResizeResponse
rpmElapsedTimeInSeconds f x =
    (\y -> x { _rpmElapsedTimeInSeconds = y })
       <$> f (_rpmElapsedTimeInSeconds x)
{-# INLINE rpmElapsedTimeInSeconds #-}

-- | The estimated time remaining, in seconds, until the resize operation is
-- complete. This value is calculated based on the average resize rate and the
-- estimated amount of data remaining to be processed. Once the resize
-- operation is complete, this value will be 0.
rpmEstimatedTimeToCompletionInSeconds
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeResizeResponse
    -> f DescribeResizeResponse
rpmEstimatedTimeToCompletionInSeconds f x =
    (\y -> x { _rpmEstimatedTimeToCompletionInSeconds = y })
       <$> f (_rpmEstimatedTimeToCompletionInSeconds x)
{-# INLINE rpmEstimatedTimeToCompletionInSeconds #-}

-- | The node type that the cluster will have after the resize operation is
-- complete.
rpmTargetNodeType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeResizeResponse
    -> f DescribeResizeResponse
rpmTargetNodeType f x =
    (\y -> x { _rpmTargetNodeType = y })
       <$> f (_rpmTargetNodeType x)
{-# INLINE rpmTargetNodeType #-}

-- | The cluster type after the resize operation is complete. Valid Values:
-- multi-node | single-node.
rpmTargetClusterType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeResizeResponse
    -> f DescribeResizeResponse
rpmTargetClusterType f x =
    (\y -> x { _rpmTargetClusterType = y })
       <$> f (_rpmTargetClusterType x)
{-# INLINE rpmTargetClusterType #-}

-- | The status of the resize operation. Valid Values: NONE | IN_PROGRESS |
-- FAILED | SUCCEEDED.
rpmStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeResizeResponse
    -> f DescribeResizeResponse
rpmStatus f x =
    (\y -> x { _rpmStatus = y })
       <$> f (_rpmStatus x)
{-# INLINE rpmStatus #-}

instance FromXML DescribeResizeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeResize where
    type Sv DescribeResize = Redshift
    type Rs DescribeResize = DescribeResizeResponse

    request = post "DescribeResize"
    response _ = xmlResponse
