{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.Service
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CloudSearch.Service where

import Data.Default
import Data.Tagged
import GHC.Generics               (Generic)
import Network.AWS.Internal
import Network.AWS.Internal.Types (Service(..))
import Text.XML.Generic

-- | Currently supported version (@2011-02-01@) of the @Amazon CloudSearch@ service.
service :: Service
service = Service Global v4 "cloudsearch" "2011-02-01"

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlInherit   = True
    , xmlNamespace = Just "http://cloudsearch.amazonaws.com/doc/2011-02-01"
    }

data CloudSearchError
    = BaseException
    | InternalException
    | InvalidTypeException
    | LimitExceededException
    | ResourceNotFoundException
      deriving (Eq, Show, Generic)

instance FromXML CloudSearchError where
    fromXMLOptions = xmlOptions
