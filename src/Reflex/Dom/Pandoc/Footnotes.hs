{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Reflex.Dom.Pandoc.Footnotes where

import Control.Monad.Reader
import Data.List (nub, sortOn)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Text as T
import Reflex.Dom.Core hiding (Link, Space)
import Text.Pandoc.Definition
import Text.Pandoc.Walk

newtype Footnote = Footnote {unFootnote :: [Block]}
  deriving (Eq, Show, Ord)

type Footnotes = Map Footnote Int

-- | Make a footnote from the Pandoc `Note` node's block elements
mkFootnote :: [Block] -> Footnote
mkFootnote blocks =
  Footnote blocks

-- | Traverse the Pandoc document accumulating any footnotes
queryFootnotes :: Pandoc -> Footnotes
queryFootnotes =
  buildFootnotes
    . query
      ( \case
          Note xs -> [mkFootnote xs]
          _ -> []
      )
  where
    buildFootnotes :: [Footnote] -> Footnotes
    buildFootnotes fs =
      Map.fromList $
        flip fmap (zip (nub fs) [1 ..]) $ \(fn, idx) ->
          (fn, idx)

renderFootnotes :: (DomBuilder t m, Monoid a) => ([Block] -> m a) -> Footnotes -> m a
renderFootnotes render footnotes = do
  if null footnotes
    then pure mempty
    else do
      elAttr "div" ("id" =: "footnotes") $ do
        el "ol" $
          fmap mconcat $
            forM (sortOn snd $ Map.toList footnotes) $ \(Footnote blks, idx) -> do
              elAttr "li" ("id" =: ("fn" <> T.pack (show idx))) $ do
                x <- render blks
                -- FIXME: This should appear inline if the footnote is a single paragraph.
                elAttr "a" ("href" =: ("#fnref" <> T.pack (show idx))) $ text "↩︎"
                pure x

renderFootnoteRef :: DomBuilder t m => Int -> m ()
renderFootnoteRef idx = do
  elNoSnippetSpan Map.empty $ do
    elClass "sup" "footnote-ref" $ do
      elAttr "a" ("id" =: ("fnref" <> T.pack (show idx)) <> "href" =: ("#fn" <> T.pack (show idx))) $ do
        text $ T.pack $ show idx
  where
    -- Prevent this element from appearing in Google search results
    -- https://developers.google.com/search/reference/robots_meta_tag#data-nosnippet-attr
    elNoSnippetSpan :: DomBuilder t m => Map Text Text -> m () -> m ()
    elNoSnippetSpan attrs = elAttr "span" ("data-nosnippet" =: "" <> attrs)

sansFootnotes :: DomBuilder t m => ReaderT Footnotes m a -> m a
sansFootnotes = flip runReaderT mempty
