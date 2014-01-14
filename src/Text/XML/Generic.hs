{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-# LANGUAGE UndecidableInstances       #-}

-- Module      : Text.XML.Generic
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               Berkeley Software Distribution License, v. 3.0.
--               You can obtain it at
--               http://http://opensource.org/licenses/BSD-3-Clause.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.XML.Generic where

import           Data.ByteString                  (ByteString)
import           Data.ByteString.Lazy             as LBS
import           Data.Default
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import qualified Data.Text.Lazy                   as LText
import qualified Data.Text.Lazy.Builder           as LText
import qualified Data.Text.Lazy.Builder.Int       as LText
import qualified Data.Text.Lazy.Builder.RealFloat as LText
import           GHC.Generics
import           Text.XML

data Bar = Bar
    { barText :: Text
    } deriving (Show, Generic)

instance ToXML Bar

data Foo = Foo
    { fooInt  :: Int
--    , fooList :: [Text]
    , text    :: Text
    , bar     :: Bar
    } deriving (Show, Generic)

instance ToXML Foo where
    toOptions = const $ def
        { namespace = Just "something"
        }

instance ToXMLRoot Foo

data XMLOptions = XMLOptions
    { namespace :: Maybe Text
    , listName  :: Maybe Text
    , ctorMod   :: String -> Text
    , fieldMod  :: String -> Text
    }

instance Default XMLOptions where
    def = XMLOptions
        { namespace = Nothing
        , listName  = Nothing
        , ctorMod   = Text.pack
        , fieldMod  = Text.pack
        }

encode :: ToXMLRoot a => a -> LBS.ByteString
encode x = renderLBS (def { rsPretty = True }) $ Document p e []
  where
    p = Prologue [] Nothing []
    e = Element n mempty $ toXML o x
    n = Name (rootName x) (namespace o) Nothing
    o = toOptions x

class ToXML a => ToXMLRoot a where
    rootName :: a -> Text

    default rootName :: (Generic a, GToXMLRoot (Rep a)) => a -> Text
    rootName x = gRootName (toOptions x) (from x)

class GToXMLRoot f where
    gRootName :: XMLOptions -> f a -> Text

instance GToXMLRoot f => GToXMLRoot (D1 c f) where
    gRootName opts = gRootName opts . unM1

instance Constructor c => GToXMLRoot (C1 c f) where
    gRootName opts _ = ctorMod opts $ conName (undefined :: C1 c f p)

class ToXML a where
    toOptions :: a -> XMLOptions
    toXML     :: XMLOptions -> a -> [Node]

    toOptions = const def

    default toXML :: (Generic a, GToXML (Rep a)) => XMLOptions -> a -> [Node]
    toXML o = gToXML o . from

instance ToXML Text where
    toXML _ = (:[]) . NodeContent

instance ToXML LText.Text where
    toXML o = toXML o . LText.toStrict

instance ToXML String where
    toXML o = toXML o . Text.pack

instance ToXML Int where
    toXML _ = nodeFromIntegral

instance ToXML Integer where
    toXML _ = nodeFromIntegral

instance ToXML Double where
    toXML _ = nodeFromFloat

instance ToXML Float where
    toXML _ = nodeFromFloat

nodeFromFloat :: RealFloat a => a -> [Node]
nodeFromFloat = nodeFromBuilder . LText.realFloat

nodeFromIntegral :: Integral a => a -> [Node]
nodeFromIntegral =  nodeFromBuilder . LText.decimal

nodeFromBuilder :: LText.Builder -> [Node]
nodeFromBuilder = (:[]) . NodeContent . LText.toStrict . LText.toLazyText

class GToXML f where
    gToXML :: XMLOptions -> f a -> [Node]

instance (GToXML f, GToXML g) => GToXML (f :*: g) where
    gToXML o (x :*: y) = gToXML o x ++ gToXML o y

instance ToXML a => GToXML (K1 R a) where
    gToXML o f =
        | Nothing <- namespace p = toXML o x
        | otherwise              = toXML p x
      where
        p = toOptions x
         x = unK1 f

instance GToXML f => GToXML (D1 c f) where
    gToXML o = gToXML o . unM1

instance GToXML f => GToXML (C1 c f) where
    gToXML o = gToXML o . unM1

instance (Selector c, GToXML f) => GToXML (S1 c f) where
    gToXML o f = [NodeElement . Element n mempty . gToXML o $ unM1 f]
      where
        n = Name (fieldMod o $ selName (undefined :: S1 c a p))
            (namespace o)
            Nothing
