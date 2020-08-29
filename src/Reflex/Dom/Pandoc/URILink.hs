{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Dom.Pandoc.URILink where

import Control.Monad (guard)
import Data.Maybe
import Text.Pandoc.Definition
import qualified Text.Pandoc.Walk as W
import Text.URI (URI, mkURI)

-- | A Pandoc Link node with a valid URI
data URILink = URILink
  { -- | This is set to Nothing for autolinks
    _uriLink_inner :: Maybe [Inline],
    _uriLink_uri :: URI
  }
  deriving (Eq, Show, Ord)

uriLinkFromInline :: Inline -> Maybe URILink
uriLinkFromInline = \case
  Link _attr inlines (url, _title) -> do
    uri <- mkURI url
    let inner = do
          guard $ inlines /= [Str url]
          pure inlines
    pure $ URILink inner uri
  _ ->
    Nothing

queryURILinks :: Pandoc -> [URILink]
queryURILinks = W.query go
  where
    go :: Inline -> [URILink]
    go = maybeToList . uriLinkFromInline
