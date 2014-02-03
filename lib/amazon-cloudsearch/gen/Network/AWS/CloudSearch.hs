-- Module      : Network.AWS.CloudSearch
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CloudSearch
    (
    -- * Operations
    -- ** DescribeSynonymOptions
      module Network.AWS.CloudSearch.DescribeSynonymOptions
    -- ** UpdateStopwordOptions
    , module Network.AWS.CloudSearch.UpdateStopwordOptions
    -- ** DescribeRankExpressions
    , module Network.AWS.CloudSearch.DescribeRankExpressions
    -- ** DescribeDefaultSearchField
    , module Network.AWS.CloudSearch.DescribeDefaultSearchField
    -- ** DescribeServiceAccessPolicies
    , module Network.AWS.CloudSearch.DescribeServiceAccessPolicies
    -- ** DeleteRankExpression
    , module Network.AWS.CloudSearch.DeleteRankExpression
    -- ** UpdateSynonymOptions
    , module Network.AWS.CloudSearch.UpdateSynonymOptions
    -- ** DescribeStopwordOptions
    , module Network.AWS.CloudSearch.DescribeStopwordOptions
    -- ** DescribeDomains
    , module Network.AWS.CloudSearch.DescribeDomains
    -- ** DefineRankExpression
    , module Network.AWS.CloudSearch.DefineRankExpression
    -- ** CreateDomain
    , module Network.AWS.CloudSearch.CreateDomain
    -- ** DescribeStemmingOptions
    , module Network.AWS.CloudSearch.DescribeStemmingOptions
    -- ** DescribeIndexFields
    , module Network.AWS.CloudSearch.DescribeIndexFields
    -- ** IndexDocuments
    , module Network.AWS.CloudSearch.IndexDocuments
    -- ** DeleteIndexField
    , module Network.AWS.CloudSearch.DeleteIndexField
    -- ** UpdateStemmingOptions
    , module Network.AWS.CloudSearch.UpdateStemmingOptions
    -- ** UpdateServiceAccessPolicies
    , module Network.AWS.CloudSearch.UpdateServiceAccessPolicies
    -- ** UpdateDefaultSearchField
    , module Network.AWS.CloudSearch.UpdateDefaultSearchField
    -- ** DeleteDomain
    , module Network.AWS.CloudSearch.DeleteDomain
    -- ** DefineIndexField
    , module Network.AWS.CloudSearch.DefineIndexField

    -- * Types
    -- ** UIntOptions
    , UIntOptions (..)
    -- ** TextOptions
    , TextOptions (..)
    -- ** SynonymOptionsStatus
    , SynonymOptionsStatus (..)
    -- ** StopwordOptionsStatus
    , StopwordOptionsStatus (..)
    -- ** StemmingOptionsStatus
    , StemmingOptionsStatus (..)
    -- ** SourceDataTrimTitle
    , SourceDataTrimTitle (..)
    -- ** SourceDataMap
    , SourceDataMap (..)
    -- ** SourceData
    , SourceData (..)
    -- ** SourceAttribute
    , SourceAttribute (..)
    -- ** ServiceEndpoint
    , ServiceEndpoint (..)
    -- ** RankExpressionStatus
    , RankExpressionStatus (..)
    -- ** OptionStatus
    , OptionStatus (..)
    -- ** NamedRankExpression
    , NamedRankExpression (..)
    -- ** LiteralOptions
    , LiteralOptions (..)
    -- ** IndexFieldStatus
    , IndexFieldStatus (..)
    -- ** IndexField
    , IndexField (..)
    -- ** DomainStatus
    , DomainStatus (..)
    -- ** DefaultSearchFieldStatus
    , DefaultSearchFieldStatus (..)
    -- ** AccessPoliciesStatus
    , AccessPoliciesStatus (..)
    -- ** SourceDataFunction
    , SourceDataFunction (..)
    -- ** OptionState
    , OptionState (..)
    -- ** IndexFieldType
    , IndexFieldType (..)

    -- * Errors
    , CloudSearchError (..)
    ) where

import Network.AWS.CloudSearch.Service
import Network.AWS.CloudSearch.Types

import Network.AWS.CloudSearch.DescribeSynonymOptions
import Network.AWS.CloudSearch.UpdateStopwordOptions
import Network.AWS.CloudSearch.DescribeRankExpressions
import Network.AWS.CloudSearch.DescribeDefaultSearchField
import Network.AWS.CloudSearch.DescribeServiceAccessPolicies
import Network.AWS.CloudSearch.DeleteRankExpression
import Network.AWS.CloudSearch.UpdateSynonymOptions
import Network.AWS.CloudSearch.DescribeStopwordOptions
import Network.AWS.CloudSearch.DescribeDomains
import Network.AWS.CloudSearch.DefineRankExpression
import Network.AWS.CloudSearch.CreateDomain
import Network.AWS.CloudSearch.DescribeStemmingOptions
import Network.AWS.CloudSearch.DescribeIndexFields
import Network.AWS.CloudSearch.IndexDocuments
import Network.AWS.CloudSearch.DeleteIndexField
import Network.AWS.CloudSearch.UpdateStemmingOptions
import Network.AWS.CloudSearch.UpdateServiceAccessPolicies
import Network.AWS.CloudSearch.UpdateDefaultSearchField
import Network.AWS.CloudSearch.DeleteDomain
import Network.AWS.CloudSearch.DefineIndexField
