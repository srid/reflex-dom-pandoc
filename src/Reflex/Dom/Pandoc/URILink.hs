{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Dom.Pandoc.URILink where

import Data.Maybe
import Data.Text (Text)
import Text.Pandoc.Definition
import qualified Text.Pandoc.Walk as W
import Text.URI (URI, mkURI)

-- | A Pandoc Link node with a valid URI and a simple (unformatted) link text.
data URILink = URILink
  { _uriLink_linkText :: Text,
    _uriLink_uri :: URI
  }
  deriving (Eq, Show, Ord)

uriLinkFromInline :: Inline -> Maybe URILink
uriLinkFromInline = \case
  Link _attr [Str linkText] (url, _title) -> do
    uri <- mkURI url
    pure $ URILink linkText uri
  _ ->
    Nothing

queryURILinks :: Pandoc -> [URILink]
queryURILinks = W.query go
  where
    go :: Inline -> [URILink]
    go = maybeToList . uriLinkFromInline
