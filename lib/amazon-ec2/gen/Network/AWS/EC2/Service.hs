{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.Service
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.EC2.Service where

import Data.Default
import Data.Tagged
import GHC.Generics               (Generic)
import Network.AWS.Internal
import Network.AWS.Internal.Types (Service(..))
import Text.XML.Generic

-- | Currently supported version (@2013-10-15@) of the @Amazon Elastic Compute Cloud@ service.
service :: Service
service = Service Global v2 "ec2" "2013-10-15"

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlInherit   = True
    , xmlNamespace = Just "http://ec2.amazonaws.com/doc/2013-10-15/"
    }

data EC2Error = EC2Error
    deriving (Eq, Show, Generic)

instance FromXML EC2Error where
    fromXMLOptions = xmlOptions
