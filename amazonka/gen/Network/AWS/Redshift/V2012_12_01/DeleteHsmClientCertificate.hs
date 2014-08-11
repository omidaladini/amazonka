{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DeleteHsmClientCertificate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified HSM client certificate.
module Network.AWS.Redshift.V2012_12_01.DeleteHsmClientCertificate where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

data DeleteHsmClientCertificate = DeleteHsmClientCertificate
    { _dhccnHsmClientCertificateIdentifier :: Text
      -- ^ The identifier of the HSM client certificate to be deleted.
    } deriving (Show, Generic)

makeLenses ''DeleteHsmClientCertificate

instance ToQuery DeleteHsmClientCertificate where
    toQuery = genericToQuery def

data DeleteHsmClientCertificateResponse = DeleteHsmClientCertificateResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteHsmClientCertificateResponse

instance AWSRequest DeleteHsmClientCertificate where
    type Sv DeleteHsmClientCertificate = Redshift
    type Rs DeleteHsmClientCertificate = DeleteHsmClientCertificateResponse

    request = post "DeleteHsmClientCertificate"
    response _ = nullaryResponse DeleteHsmClientCertificateResponse