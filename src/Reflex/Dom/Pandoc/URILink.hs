{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO: Rename
module Reflex.Dom.Pandoc.URILink where

import Data.Maybe
import Text.Pandoc.Definition
import qualified Text.Pandoc.Walk as W
import Text.URI (URI, mkURI)

-- | A Pandoc Link node with a valid URI
data URILink = URILink
  { _uriLink_inner :: [Inline],
    _uriLink_uri :: URI,
    _uriLink_autolink :: Bool
  }
  deriving (Eq, Show, Ord)

uriLinkFromInline :: Inline -> Maybe URILink
uriLinkFromInline = \case
  Link _attr inner (url, _title) -> do
    uri <- mkURI url
    let autolink = inner == [Str url]
    pure $ URILink inner uri autolink
  _ ->
    Nothing

queryURILinks :: Pandoc -> [URILink]
queryURILinks = W.query go
  where
    go :: Inline -> [URILink]
    go = maybeToList . uriLinkFromInline
