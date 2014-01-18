-- Module      : Text.Class
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               Berkeley Software Distribution License, v. 3.0.
--               You can obtain it at
--               http://http://opensource.org/licenses/BSD-3-Clause.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.Class where

import           Control.Monad
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.Encoding              as Text
import           Network.HTTP.QueryString.Pickle
import           Text.Read

readFromText :: FromText a => ReadS a
readFromText = either (const []) (\x -> [(x, "")]) . fromText . Text.pack

showToText :: ToText a => a -> String
showToText = Text.unpack . toText

primQuery :: (FromText a, ToText a) => QueryPU a
primQuery = QueryPU p u
  where
    p = pickle queryPickler . toText
    u = join . fmap fromText . unpickle queryPickler

class FromText a where
    fromText :: Text -> Either String a

instance FromText Text where
    fromText = Right

class ToText a where
    toText :: a -> Text

instance ToText Text where
    toText = id
