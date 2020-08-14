{-# LANGUAGE OverloadedStrings #-}

module Reflex.Dom.Pandoc.Util where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core
import Text.Pandoc.Definition (Attr)

elPandocAttr ::
  DomBuilder t m =>
  -- | Element name
  Text ->
  -- | Pandoc attribute object. TODO: Use a sensible type.
  Attr ->
  -- | Child widget
  m a ->
  m a
elPandocAttr name = elAttr name . sansEmptyAttrs . renderAttr

sansEmptyAttrs :: Map k Text -> Map k Text
sansEmptyAttrs = Map.filter (not . T.null)

renderAttr ::
  -- | Pandoc attribute object. TODO: Use a sensible type.
  Attr ->
  Map Text Text
renderAttr (identifier, classes, attrs) =
  "id" =: identifier
    <> "class" =: T.unwords classes
    <> Map.fromList attrs

addClass ::
  -- | The class to add
  Text ->
  -- | Pandoc attribute object. TODO: Use a sensible type.
  Attr ->
  Attr
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
