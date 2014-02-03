{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.GetTemplate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the template body for a specified stack. You can get the template
-- for running or deleted stacks. For deleted stacks, GetTemplate returns the
-- template for up to 90 days after the stack has been deleted. If the
-- template does not exist, a ValidationError is returned.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=GetTemplate
-- &StackName=MyStack &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature] "{ "AWSTemplateFormatVersion" : "2010-09-09",
-- "Description" : "Simple example", "Resources" : { "MySQS" : { "Type" :
-- "AWS::SQS::Queue", "Properties" : { } } } }.
module Network.AWS.CloudFormation.GetTemplate where

import           Control.Monad
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import           Text.XML.Generic

import Network.AWS.CloudFormation.Service
import Network.AWS.CloudFormation.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
getTemplate :: Text
            -> GetTemplate
getTemplate p1 = GetTemplate
    { gtiStackName = p1
    }

data GetTemplate = GetTemplate
    { gtiStackName :: !Text
      -- ^ The name or the unique identifier associated with the stack, which are not
      -- always interchangeable: Running stacks: You can specify either the stack's
      -- name or its unique stack ID. Deleted stacks: You must specify the unique
      -- stack ID. Default: There is no default value.
    } deriving (Eq, Show, Generic)

instance ToQuery GetTemplate

instance AWSRequest GetTemplate where
    type Er GetTemplate = CloudFormationError
    type Rs GetTemplate = GetTemplateResponse
    request = getQuery service "GetTemplate"

data GetTemplateResponse = GetTemplateResponse
    { gtirsTemplateBody :: Maybe Text
      -- ^ Structure containing the template body. (For more information, go to the
      -- AWS CloudFormation User Guide.).
    } deriving (Eq, Show, Generic)

instance FromXML GetTemplateResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "GetTemplateResponse"
        :| ["GetTemplateResult"]
