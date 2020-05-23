{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Reflex.Dom.Pandoc.Footnotes where

import Control.Monad.Reader
import Data.List (nub, sortOn)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Text as T
import Reflex.Dom.Core hiding (Link, Space)
import Text.Pandoc.Definition
import Text.Pandoc.Walk

newtype Footnote = Footnote {unFootnote :: [Block]}
  deriving (Eq, Show, Ord)

type Footnotes = Map Footnote Int

getFootnotes :: Pandoc -> Footnotes
getFootnotes =
  buildFootnotes
    . query
      ( \case
          Note s -> [Footnote s]
          _ -> []
      )
  where
    buildFootnotes :: [Footnote] -> Footnotes
    buildFootnotes fs =
      Map.fromList $ flip fmap (zip (nub fs) [1 ..]) $ \(fn, idx) ->
        (fn, idx)

renderFootnotes :: (DomBuilder t m, Monoid a) => ([Block] -> m a) -> Footnotes -> m a
renderFootnotes render footnotes = do
  if null footnotes
    then pure mempty
    else do
      elAttr "div" ("id" =: "footnotes") $ do
        el "ol" $ fmap mconcat $ forM (sortOn snd $ Map.toList footnotes) $ \(Footnote blks, idx) -> do
          el "li" $ do
            -- We discard any footnotes inside footnotes
            elAttr "a" ("name" =: ("fn" <> T.pack (show idx))) blank
            x <- render blks
            -- FIXME: This should appear inline if the footnote is a single paragraph.
            elAttr "a" ("href" =: ("#fnref" <> T.pack (show idx))) $ text "↩︎"
            pure x

renderFootnoteRef :: DomBuilder t m => Int -> m ()
renderFootnoteRef idx = do
  elClass "sup" "footnote-ref" $ do
    elAttr "a" ("name" =: ("fnref" <> T.pack (show idx)) <> "href" =: ("#fn" <> T.pack (show idx))) $ do
      text $ T.pack $ show idx

sansFootnotes :: DomBuilder t m => ReaderT Footnotes m a -> m a
sansFootnotes = flip runReaderT mempty
