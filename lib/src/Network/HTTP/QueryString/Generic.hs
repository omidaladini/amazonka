-- Module      : Network.HTTP.QueryString.Generic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               Berkeley Software Distribution License, v. 3.0.
--               You can obtain it at
--               http://http://opensource.org/licenses/BSD-3-Clause.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.HTTP.QueryString.Generic where

import           Data.Char
import           Data.Default
import           Data.Monoid
import           Data.Text          (Text)
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import           Data.Text.Helpers
import           GHC.Generics

data Query
    = Value Text
    | Pair Text Query
    | List [Query]
      deriving (Eq, Show)

data QueryOptions = QueryOptions
    { ctorMod  :: String -> Text
    , fieldMod :: String -> Text
    }

instance Default QueryOptions
    def = QueryOptions
        { ctorMod  = Text.pack
        , fieldMod = Text.dropWhile isLower . Text.pack
        }

-- decode

fromLoweredQuery :: (Generic a, GToQuery (Rep a))
                 => Text
                 -> Either String a

genericFromQuery :: (Generic a, GToQuery (Rep a))
                 => Text
                 -> Either String a

class FromQuery a where
    fromQuery :: QueryOptions -> Text -> Either String a

    default fromQuery :: (Generic a, GFromQuery (Rep a))
                      => Text
                      -> Either String a
    fromQuery = genericFromQuery def

-- encode

toLoweredQuery :: (Generic a, GToQuery (Rep a))
               => a
               -> Text

genericToQuery :: (Generic a, GToQuery (Rep a))
               => QueryOptions
               -> a
               -> Text

class ToQuery a where
    toQuery :: QueryOptions -> a -> Text

    default toQuery :: (Generic a, GToQuery (Rep a))
                    => a
                    -> Text
    toQuery = genericToQuery def
