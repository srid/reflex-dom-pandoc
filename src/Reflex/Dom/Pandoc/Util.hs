{-# LANGUAGE OverloadedStrings #-}

module Reflex.Dom.Pandoc.Util where

import Data.Map (Map)
import Data.Text (Text)

import qualified Data.Map as Map
import qualified Data.Text as T

import Text.Pandoc.Definition (Attr)
import Reflex.Dom.Core

elPandocAttr
  :: DomBuilder t m
  => Text
  -- ^ Element name
  -> Attr
  -- ^ Pandoc attribute object. TODO: Use a sensible type.
  -> m a
  -- ^ Child widget
  -> m a
elPandocAttr name = elAttr name . renderAttr

renderAttr
  :: Attr
  -- ^ Pandoc attribute object. TODO: Use a sensible type.
  -> Map Text Text
renderAttr (identifier, classes, attrs) =
     "id" =: identifier
  <> "class" =: (T.unwords classes)
  <> Map.fromList attrs

addClass
  :: Text
  -- ^ The class to add
  -> Attr
  -- ^ Pandoc attribute object. TODO: Use a sensible type.
  -> Attr
addClass c (identifier, classes, attrs) = (identifier, c : classes, attrs)

headerElement :: Int -> Text
headerElement level = case level of
  1 -> "h1"
  2 -> "h2"
  3 -> "h3"
  4 -> "h4"
  5 -> "h5"
  6 -> "h6"
  _ -> error "bad header level"
