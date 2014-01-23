-- Module      : Data.Text.Helpers
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               Berkeley Software Distribution License, v. 3.0.
--               You can obtain it at
--               http://http://opensource.org/licenses/BSD-3-Clause.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Data.Text.Helpers where

import           Data.Text (Text)
import qualified Data.Text as Text

failFromText :: Text -> Either String a
failFromText = Left . Text.unpack

readFromText :: FromText a => ReadS a
readFromText = either (const []) (\x -> [(x, "")]) . fromText . Text.pack

showToText :: ToText a => a -> String
showToText = Text.unpack . toText

class FromText a where
    fromText :: Text -> Either String a

instance FromText Text where
    fromText = Right

class ToText a where
    toText :: a -> Text

instance ToText Text where
    toText = id
