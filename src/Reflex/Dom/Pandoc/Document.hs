{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.Dom.Pandoc.Document
  ( elPandoc,
    elPandocInlines,
    PandocBuilder,
    PandocRaw (..),
  )
where

import Control.Monad
import Control.Monad.Reader
import Data.Bool
import qualified Data.Map as Map
import Data.Maybe
import Reflex.Dom.Core hiding (Link, Space)
import Reflex.Dom.Pandoc.Footnotes
import Reflex.Dom.Pandoc.PandocRaw
import Reflex.Dom.Pandoc.SyntaxHighlighting (elCodeHighlighted)
import Reflex.Dom.Pandoc.Util (elPandocAttr, headerElement, renderAttr)
import Text.Pandoc.Definition

-- | Like `DomBuilder` but with a capability to render pandoc raw content.
type PandocBuilder t m =
  ( DomBuilder t m,
    PandocRaw m,
    PandocRawConstraints m
  )

-- | Convert Markdown to HTML
elPandoc :: forall t m. PandocBuilder t m => Pandoc -> m ()
elPandoc doc@(Pandoc _meta blocks) = do
  let fs = getFootnotes doc
  flip runReaderT fs $ renderBlocks blocks
  renderFootnotes (sansFootnotes . renderBlocks) fs

-- | Render list of Pandoc inlines
elPandocInlines :: forall t m. PandocBuilder t m => [Inline] -> m ()
elPandocInlines = void . sansFootnotes . renderInlines

renderBlocks :: (MonadReader Footnotes m, PandocBuilder t m) => [Block] -> m ()
renderBlocks =
  mapM_ $ renderBlock

renderBlock :: (MonadReader Footnotes m, PandocBuilder t m) => Block -> m ()
renderBlock = \case
  -- Pandoc parses github tasklist as this structure.
  Plain (Str "☐" : Space : is) -> checkboxEl False >> renderInlines is
  Plain (Str "☒" : Space : is) -> checkboxEl True >> renderInlines is
  Para (Str "☐" : Space : is) -> checkboxEl False >> renderInlines is
  Para (Str "☒" : Space : is) -> checkboxEl True >> renderInlines is
  Plain xs ->
    renderInlines xs
  Para xs ->
    el "p" $ renderInlines xs
  LineBlock xss ->
    forM_ xss $ \xs -> do
      renderInlines xs <* text "\n"
  CodeBlock attr x ->
    elCodeHighlighted attr x
  RawBlock fmt x ->
    elPandocRaw fmt x
  BlockQuote xs ->
    el "blockquote" $ renderBlocks xs
  OrderedList _lattr xss ->
    -- TODO: Implement list attributes.
    el "ol" $ do
      forM_ xss $ \xs -> do
        el "li" $ renderBlocks xs
  BulletList xss ->
    el "ul" $ forM_ xss $ \xs -> el "li" $ renderBlocks xs
  DefinitionList defs ->
    el "dl" $ forM_ defs $ \(term, descList) -> do
      el "dt" $ renderInlines term
      forM_ descList $ \desc ->
        el "dd" $ renderBlocks desc
  Header level attr xs ->
    elPandocAttr (headerElement level) attr $ do
      renderInlines xs
  HorizontalRule ->
    el "hr" blank
  Table _attr _captions _colSpec (TableHead _ hrows) tbodys _tfoot -> do
    -- TODO: Rendering is basic, and needs to handle with all attributes of the AST
    elClass "table" "ui celled table" $ do
      el "thead" $ do
        forM_ hrows $ \(Row _ cells) -> do
          el "tr" $ do
            forM_ cells $ \(Cell _ _ _ _ blks) ->
              el "th" $ renderBlocks blks
      forM_ tbodys $ \(TableBody _ _ _ rows) ->
        el "tbody" $ do
          forM_ rows $ \(Row _ cells) ->
            el "tr" $ do
              forM_ cells $ \(Cell _ _ _ _ blks) ->
                el "td" $ renderBlocks blks
  Div attr xs ->
    elPandocAttr "div" attr $
      renderBlocks xs
  Null ->
    blank
  where
    checkboxEl checked =
      void $
        elAttr
          "input"
          ( mconcat $ catMaybes $
              [ Just $ "type" =: "checkbox",
                Just $ "disabled" =: "True",
                bool Nothing (Just $ "checked" =: "True") checked
              ]
          )
          blank

renderInlines :: (MonadReader Footnotes m, PandocBuilder t m) => [Inline] -> m ()
renderInlines =
  mapM_ $ renderInline

renderInline :: (MonadReader Footnotes m, PandocBuilder t m) => Inline -> m ()
renderInline = \case
  Str x ->
    text x
  Emph xs ->
    el "em" $ renderInlines xs
  Strong xs ->
    el "strong" $ renderInlines xs
  Underline xs ->
    el "u" $ renderInlines xs
  Strikeout xs ->
    el "strike" $ renderInlines xs
  Superscript xs ->
    el "sup" $ renderInlines xs
  Subscript xs ->
    el "sub" $ renderInlines xs
  SmallCaps xs ->
    el "small" $ renderInlines xs
  Quoted qt xs ->
    flip inQuotes qt $ renderInlines xs
  Cite _ _ ->
    el "pre" $ text "error[reflex-doc-pandoc]: Pandoc Cite is not handled"
  Code attr x ->
    elPandocAttr "code" attr $
      text x
  Space ->
    text " "
  SoftBreak ->
    text " "
  LineBreak ->
    text "\n"
  RawInline fmt x ->
    elPandocRaw fmt x
  Math mathType s ->
    -- http://docs.mathjax.org/en/latest/basic/mathematics.html#tex-and-latex-input
    case mathType of
      InlineMath ->
        elClass "span" "math inline" $ text $ "\\(" <> s <> "\\)"
      DisplayMath ->
        elClass "span" "math display" $ text "$$" >> text s >> text "$$"
  Link attr xs (lUrl, lTitle) -> do
    let attr' = renderAttr attr <> ("href" =: lUrl <> "title" =: lTitle)
    elAttr "a" attr' $ renderInlines xs
  Image attr xs (iUrl, iTitle) -> do
    let attr' = renderAttr attr <> ("src" =: iUrl <> "title" =: iTitle)
    elAttr "img" attr' $ renderInlines xs
  Note xs -> do
    fs :: Footnotes <- ask
    case Map.lookup (Footnote xs) fs of
      Nothing ->
        -- No footnote in the global map (this means that the user has
        -- defined a footnote inside a footnote); just put the whole thing in
        -- aside.
        elClass "aside" "footnote-inline" $ renderBlocks xs
      Just idx ->
        renderFootnoteRef idx
  -- el "aside" $ renderBlocks xs
  Span attr xs ->
    elPandocAttr "span" attr $
      renderInlines xs
  where
    inQuotes w = \case
      SingleQuote -> text "❛" >> w <* text "❜"
      DoubleQuote -> text "❝" >> w <* text "❞"
