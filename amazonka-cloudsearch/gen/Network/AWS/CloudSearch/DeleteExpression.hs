{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.DeleteExpression
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes an Expression from the search domain. For more information, see
-- Configuring Expressions in the Amazon CloudSearch Developer Guide.
module Network.AWS.CloudSearch.DeleteExpression
    (
    -- * Request
      DeleteExpression
    -- ** Request constructor
    , mkDeleteExpression
    -- ** Request lenses
    , de1DomainName
    , de1ExpressionName

    -- * Response
    , DeleteExpressionResponse
    -- ** Response constructor
    , mkDeleteExpressionResponse
    -- ** Response lenses
    , derrExpression
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import Network.AWS.Prelude

-- | Container for the parameters to the DeleteExpression operation. Specifies
-- the name of the domain you want to update and the name of the expression
-- you want to delete.
data DeleteExpression = DeleteExpression
    { _de1DomainName :: !Text
    , _de1ExpressionName :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteExpression' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DomainName ::@ @Text@
--
-- * @ExpressionName ::@ @Text@
--
mkDeleteExpression :: Text -- ^ 'de1DomainName'
                   -> Text -- ^ 'de1ExpressionName'
                   -> DeleteExpression
mkDeleteExpression p1 p2 = DeleteExpression
    { _de1DomainName = p1
    , _de1ExpressionName = p2
    }

-- | A string that represents the name of a domain. Domain names are unique
-- across the domains owned by an account within an AWS region. Domain names
-- start with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
de1DomainName :: Lens' DeleteExpression Text
de1DomainName = lens _de1DomainName (\s a -> s { _de1DomainName = a })

-- | The name of the Expression to delete.
de1ExpressionName :: Lens' DeleteExpression Text
de1ExpressionName =
    lens _de1ExpressionName (\s a -> s { _de1ExpressionName = a })

instance ToQuery DeleteExpression where
    toQuery = genericQuery def

-- | The result of a DeleteExpression request. Specifies the expression being
-- deleted.
newtype DeleteExpressionResponse = DeleteExpressionResponse
    { _derrExpression :: ExpressionStatus
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteExpressionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Expression ::@ @ExpressionStatus@
--
mkDeleteExpressionResponse :: ExpressionStatus -- ^ 'derrExpression'
                           -> DeleteExpressionResponse
mkDeleteExpressionResponse p1 = DeleteExpressionResponse
    { _derrExpression = p1
    }

-- | The status of the expression being deleted.
derrExpression :: Lens' DeleteExpressionResponse ExpressionStatus
derrExpression = lens _derrExpression (\s a -> s { _derrExpression = a })

instance FromXML DeleteExpressionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteExpression where
    type Sv DeleteExpression = CloudSearch
    type Rs DeleteExpression = DeleteExpressionResponse

    request = post "DeleteExpression"
    response _ = xmlResponse