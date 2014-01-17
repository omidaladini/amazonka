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
import qualified Data.Attoparsec.Text             as AText
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

newtype Qux = Qux Text
    deriving (Show, Generic)

instance XMLRoot Qux
instance ToXML Qux
instance FromXML Qux

data Baz = Baz
   deriving (Show, Generic)

instance XMLRoot Baz
instance ToXML Baz
instance FromXML Baz

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
    , fooList :: [Bar]
    , mtext   :: Maybe Text
    } deriving (Show, Generic)

instance XMLRoot Foo where
    rootName _ _ = "NotFoo"

instance ToXML Foo where
    toXMLOptions = const def
        { namespace = Just "something"
        }

data XMLOptions = XMLOptions
    { inherit   :: !Bool
    , namespace :: Maybe Text
    , listName  :: Maybe Text
    , ctorMod   :: String -> Text
    , fieldMod  :: String -> Text
    }

instance Default XMLOptions where
    def = XMLOptions
        { inherit   = True
        , namespace = Nothing
        , listName  = Just "Item"
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

instance FromXML Int where
    fromXML = nodeParser AText.decimal

instance FromXML Integer where
    fromXML = nodeParser AText.decimal

instance FromXML Double where
    fromXML = nodeParser AText.rational

instance FromXML Float where
    fromXML = nodeParser AText.rational

-- -- FIXME: make work for from xml
-- instance FromXML a => FromXML [a] where
--     fromXML o = f (listName o)
--       where
--         f (Just x) = map (g (Name x (namespace o) Nothing) . fromXML o)
--         f Nothing  = concatMap (fromXML o)

--         g n = NodeElement . Element n mempty

instance FromXML a => FromXML (Maybe a) where
    fromXML o ns =
        either (const $ Right Nothing)
               (Right . Just)
               (fromXML o ns :: Either String a)

nodeParser :: AText.Parser a -> XMLOptions -> [Node] -> Either String a
nodeParser p o = join . fmap (AText.parseOnly p) . fromXML o

class GFromXML f where
    gFromXML :: XMLOptions -> [Node] -> Either String (f a)

instance (GFromXML f, GFromXML g) => GFromXML (f :+: g) where
    gFromXML o ns = (L1 <$> gFromXML o ns) <|> (R1 <$> gFromXML o ns)

instance GFromXML U1 where
    gFromXML _ _ = Right U1

instance forall a. FromXML a => GFromXML (K1 R a) where
    gFromXML x = fmap K1 . fromXML o
      where
        o | inherit y = x
          | otherwise = y

        y = fromXMLOptions (undefined :: a)

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

instance ToXML Int where
    toXML _ = nodeFromIntegral

instance ToXML Integer where
    toXML _ = nodeFromIntegral

instance ToXML Double where
    toXML _ = nodeFromFloat

instance ToXML Float where
    toXML _ = nodeFromFloat

instance ToXML a => ToXML [a] where
    toXML o = f (listName o)
      where
        f (Just x) = map (g (Name x (namespace o) Nothing) . toXML o)
        f Nothing  = concatMap (toXML o)

        g n = NodeElement . Element n mempty

instance ToXML a => ToXML (Maybe a) where
    toXML o (Just x) = toXML o x
    toXML _ Nothing  = []

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

instance GToXML U1 where
    gToXML _ _ = []

instance ToXML a => GToXML (K1 R a) where
    gToXML x f = toXML o g
      where
        o | inherit y = x
          | otherwise = y

        y = toXMLOptions g
        g = unK1 f

instance GToXML f => GToXML (D1 c f) where
    gToXML o = gToXML o . unM1

instance GToXML f => GToXML (C1 c f) where
    gToXML o = gToXML o . unM1

instance (Selector c, GToXML f) => GToXML (S1 c f) where
    gToXML o (m1 :: (S1 c f) a) = f . gToXML o $ unM1 m1
      where
        f = case selName m1 of
            "" -> id
            n  -> (:[])
                . NodeElement
                . Element (Name (fieldMod o n) (namespace o) Nothing) mempty
