{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.Service
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.IAM.Service where

import Network.AWS.Core
import Network.AWS.Generics.XML

-- | Currently supported version (@2010-05-08@) of the @AWS Identity and Access Management@ service.
service :: Service
service = Service Global v4 "iam" "2010-05-08" Nothing

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlInherit   = True
    , xmlNamespace = Just "https://iam.amazonaws.com/doc/2010-05-08/"
    }

data IAMError
    = DeleteConflictException
    | DuplicateCertificateException
    | EntityAlreadyExistsException
    | EntityTemporarilyUnmodifiableException
    | InvalidAuthenticationCodeException
    | InvalidCertificateException
    | InvalidInputException
    | InvalidUserTypeException
    | KeyPairMismatchException
    | LimitExceededException
    | MalformedCertificateException
    | MalformedPolicyDocumentException
    | NoSuchEntityException
    | PasswordPolicyViolationException
      deriving (Eq, Show, Generic)

instance FromXML IAMError where
    fromXMLOptions = xmlOptions
