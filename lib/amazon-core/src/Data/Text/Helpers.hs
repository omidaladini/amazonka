{-# LANGUAGE OverloadedStrings #-}

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

import qualified Data.Attoparsec.Text             as AText
import qualified Data.CaseInsensitive             as CI
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding               as Text
import qualified Data.Text.Lazy                   as LText
import qualified Data.Text.Lazy.Builder           as LText
import qualified Data.Text.Lazy.Builder.Int       as LText
import qualified Data.Text.Lazy.Builder.RealFloat as LText
import           Data.Time
import           Data.Time.Formatters

fromTextFail :: Text -> Either String a
fromTextFail = Left . Text.unpack

fromTextRead :: FromText a => ReadS a
fromTextRead = either (const []) (\x -> [(x, "")]) . fromText . Text.pack

toTextShow :: ToText a => a -> String
toTextShow = Text.unpack . toText

class FromText a where
    fromText :: Text -> Either String a

instance FromText Text where
    fromText = Right

instance FromText Int where
    fromText = AText.parseOnly AText.decimal

instance FromText Integer where
    fromText = AText.parseOnly AText.decimal

instance FromText Float where
    fromText = AText.parseOnly AText.rational

instance FromText Double where
    fromText = AText.parseOnly AText.rational

instance FromText Bool where
    fromText t
        | "false" <- x = Right False
        | "true"  <- x = Right True
        | otherwise    = fromTextFail $ "Unrecognised Boolean: " <> t
      where
        x = CI.foldedCase $ CI.mk t

-- FIXME: probably shouldn't be using From/ToText for headers
instance FromText UTCTime where
    fromText = undefined

class ToText a where
    toText :: a -> Text

instance ToText Text where
    toText = id

instance ToText Int where
    toText = integralToText

instance ToText Integer where
    toText = integralToText

instance ToText Float where
    toText = floatToText

instance ToText Double where
    toText = floatToText

instance ToText UTCTime where
    toText = Text.decodeUtf8 . formatRFC822

integralToText :: Integral a => a -> Text
integralToText = strictFromBuilder . LText.decimal

floatToText :: RealFloat a => a -> Text
floatToText = strictFromBuilder . LText.realFloat

strictFromBuilder :: LText.Builder -> Text
strictFromBuilder = LText.toStrict . LText.toLazyText
