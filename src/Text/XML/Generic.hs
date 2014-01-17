{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

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

import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Lazy             as LBS
import           Data.Default
import           Data.Monoid
import           Data.Tagged
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

instance XMLRoot Bar

instance ToXML Bar where
    toXMLOptions = const def
        { namespace = Just "else"
        }

instance FromXML Bar

data Foo = Foo
    { fooInt  :: Int
--    , fooList :: [Text]
    , text    :: Text
    , bar     :: Bar
    } deriving (Show, Generic)

instance XMLRoot Foo where
    rootName _ _ = "NotFoo"

instance ToXML Foo where
    toXMLOptions = const def
        { namespace = Just "something"
        }

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

encode :: (XMLRoot a, ToXML a) => Bool -> a -> LBS.ByteString
encode p x = renderLBS (def { rsPretty = p }) $ Document
    (Prologue [] Nothing [])
    (Element (Name (rootName o x) (namespace o) Nothing) mempty $ toXML o x)
    []
  where
    o = toXMLOptions x

decode :: forall a. (XMLRoot a, FromXML a) => LBS.ByteString -> Either String a
decode = f . parseLBS def
  where
    f (Left ex) = Left $ show ex
    f (Right Document{..})
        | elementName documentRoot == n = fromXML o $ elementNodes documentRoot
        | otherwise = Left $ concat
            [ "Unexpected root element: "
            , show $ elementName documentRoot
            , ", expecting: "
            , show n
            ]

    n = Name (rootName o x) (namespace o) Nothing
    o = fromXMLOptions x

    x :: a
    x = undefined

class XMLRoot a where
    rootName :: XMLOptions -> a -> Text

    default rootName :: (Generic a, GXMLRoot (Rep a))
                     => XMLOptions
                     -> a
                     -> Text
    rootName o = gRootName o . from

class GXMLRoot f where
    gRootName :: XMLOptions -> f a -> Text

instance GXMLRoot f => GXMLRoot (D1 c f) where
    gRootName o = gRootName o . unM1

instance Constructor c => GXMLRoot (C1 c f) where
    gRootName o _ = ctorMod o $ conName (undefined :: C1 c f p)

class FromXML a where
    fromXMLOptions :: a -> XMLOptions
    fromXML        :: XMLOptions -> [Node] -> Either String a

    fromXMLOptions = const def

    default fromXML :: (Generic a, GFromXML (Rep a))
                    => XMLOptions
                    -> [Node]
                    -> Either String a
    fromXML o = fmap to . gFromXML o

instance FromXML Text where
    fromXML _ [NodeContent txt] = Right txt
    fromXML _ _                 = Left "Unexpected node contents."

class GFromXML f where
    gFromXML :: XMLOptions -> [Node] -> Either String (f a)

instance (GFromXML f, GFromXML g) => GFromXML (f :+: g) where
    gFromXML o ns = (L1 <$> gFromXML o ns) <|> (R1 <$> gFromXML o ns)

instance FromXML a => GFromXML (K1 R a) where
    gFromXML o = fmap K1 . fromXML o

instance GFromXML f => GFromXML (D1 c f) where
    gFromXML o = fmap M1 . gFromXML o

instance GFromXML f => GFromXML (C1 c f) where
    gFromXML o = fmap M1 . gFromXML o

instance (Selector c, GFromXML f) => GFromXML (S1 c f) where
    gFromXML o ns = findNodes ns >>= fmap M1 . gFromXML o
      where
        findNodes [] = Left $ "Failed to find: " ++ Text.unpack sel
        findNodes (NodeElement e : es)
            | elementName e == name = Right $ elementNodes e
            | otherwise    = findNodes es
        findNodes (_ : es) = findNodes es

        name = Name sel (namespace o) Nothing
        sel  = fieldMod o $ selName (undefined :: S1 c f p)

--        o' = toXMLOptions (undefined :: S1 c f p)

class ToXML a where
    toXMLOptions :: a -> XMLOptions
    toXML        :: XMLOptions -> a -> [Node]

    toXMLOptions = const def

    default toXML :: (Generic a, GToXML (Rep a))
                  => XMLOptions
                  -> a
                  -> [Node]
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
    gToXML o f
        | Nothing <- namespace p = toXML o x
        | otherwise              = toXML p x
      where
        p = toXMLOptions x
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
