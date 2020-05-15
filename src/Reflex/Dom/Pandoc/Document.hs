{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Reflex.Dom.Pandoc.Document
  ( elPandocDoc,
    elPandocInlines,
    elPandocHeading1,
    rawAsActual,
    rawAsRaw,
  )
where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State (modify)
import Data.Bool
import qualified Data.Map as Map
import Data.Maybe
import Data.Text.Encoding (encodeUtf8Builder)
import GHC.IORef
import Reflex.Dom.Core hiding (Link, Space)
import Reflex.Dom.Pandoc.Footnotes
import Reflex.Dom.Pandoc.SyntaxHighlighting (elCodeHighlighted)
import Reflex.Dom.Pandoc.Util (elPandocAttr, headerElement, renderAttr)
import Reflex.Host.Class
import Text.Pandoc.Definition

elPandocInlinesStatic ::
  forall t m.
  ( Reflex t,
    Monad m,
    Ref m ~ IORef,
    MonadIO m,
    MonadHold t m,
    MonadFix m,
    MonadRef m,
    Adjustable t m,
    PerformEvent t m,
    MonadReflexCreateTrigger t m
  ) =>
  [Inline] ->
  StaticDomBuilderT t m ()
elPandocInlinesStatic = void . flip runReaderT mempty . renderInlines (\f -> ReaderT $ \_ -> rawAsActual f)

-- | Render list of Pandoc inlines
elPandocInlines :: forall t m. DomBuilder t m => [Inline] -> m ()
elPandocInlines = void . sansFootnotes . renderInlines rawAsRaw

type RenderRawF t m = DomBuilder t m => Format -> m ()

rawAsRaw :: RenderRawF t m
rawAsRaw (Format s) =
  el "pre" $ text s

rawAsActual :: (Reflex t, Monad m) => Format -> StaticDomBuilderT t m ()
rawAsActual (Format s) =
  StaticDomBuilderT $ lift $ modify $ (:) $ fmap encodeUtf8Builder $ current $ constDyn s

renderBlocks :: (MonadReader Footnotes m, DomBuilder t m) => (RenderRawF t m) -> [Block] -> m ()
renderBlocks renderRaw =
  mapM_ $ renderBlock renderRaw

-- | Convert Markdown to HTML
elPandocDoc :: forall t m. DomBuilder t m => (RenderRawF t m) -> Pandoc -> m ()
elPandocDoc renderRaw doc@(Pandoc _meta blocks) = do
  let fs = getFootnotes doc
  flip runReaderT fs $ renderBlocks renderRaw blocks
  renderFootnotes (sansFootnotes . renderBlocks renderRaw) fs

-- | Render the first level of heading
elPandocHeading1 :: DomBuilder t m => Pandoc -> m ()
elPandocHeading1 (Pandoc _meta blocks) = forM_ blocks $ \case
  Header 1 _ xs -> elPandocInlines xs
  _ -> blank

renderBlock :: (MonadReader Footnotes m, DomBuilder t m) => (RenderRawF t m) -> Block -> m ()
renderBlock renderRaw = \case
  -- Pandoc parses github tasklist as this structure.
  Plain (Str "☐" : Space : is) -> checkboxEl False >> renderInlines renderRaw is
  Plain (Str "☒" : Space : is) -> checkboxEl True >> renderInlines renderRaw is
  Para (Str "☐" : Space : is) -> checkboxEl False >> renderInlines renderRaw is
  Para (Str "☒" : Space : is) -> checkboxEl True >> renderInlines renderRaw is
  Plain xs ->
    renderInlines renderRaw xs
  Para xs ->
    el "p" $ renderInlines renderRaw xs
  LineBlock xss ->
    forM_ xss $ \xs -> do
      renderInlines renderRaw xs <* text "\n"
  CodeBlock attr x ->
    elCodeHighlighted attr x
  RawBlock (Format t) x ->
    -- We are not *yet* sure what to do with this. For now, just dump it in pre.
    -- NOTE: if t==html, we must embed the raw HTML. But this doesn't seem
    -- possible reflex-dom without ghcjs constraints.
    elClass "pre" ("pandoc-raw " <> t) $ text x
  BlockQuote xs ->
    el "blockquote" $ renderBlocks renderRaw xs
  OrderedList _lattr xss ->
    -- TODO: Implement list attributes.
    el "ol" $ do
      forM_ xss $ \xs -> do
        el "li" $ renderBlocks renderRaw xs
  BulletList xss ->
    el "ul" $ forM_ xss $ \xs -> el "li" $ renderBlocks renderRaw xs
  DefinitionList defs ->
    el "dl" $ forM_ defs $ \(term, descList) -> do
      el "dt" $ renderInlines renderRaw term
      forM_ descList $ \desc ->
        el "dd" $ renderBlocks renderRaw desc
  Header level attr xs ->
    elPandocAttr (headerElement level) attr $ do
      renderInlines renderRaw xs
  HorizontalRule ->
    el "hr" blank
  Table _attr _captions _colSpec (TableHead _ hrows) tbodys _tfoot -> do
    -- TODO: Rendering is basic, and needs to handle with all attributes of the AST
    elClass "table" "ui celled table" $ do
      el "thead" $ do
        forM_ hrows $ \(Row _ cells) -> do
          el "tr" $ do
            forM_ cells $ \(Cell _ _ _ _ blks) ->
              el "th" $ renderBlocks renderRaw blks
      forM_ tbodys $ \(TableBody _ _ _ rows) ->
        el "tbody" $ do
          forM_ rows $ \(Row _ cells) ->
            el "tr" $ do
              forM_ cells $ \(Cell _ _ _ _ blks) ->
                el "td" $ renderBlocks renderRaw blks
  Div attr xs ->
    elPandocAttr "div" attr $
      renderBlocks renderRaw xs
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

renderInlines :: (MonadReader Footnotes m, DomBuilder t m) => (RenderRawF t m) -> [Inline] -> m ()
renderInlines renderRaw =
  mapM_ $ renderInline renderRaw

renderInline :: (MonadReader Footnotes m, DomBuilder t m) => (RenderRawF t m) -> Inline -> m ()
renderInline renderRaw = \case
  Str x ->
    text x
  Emph xs ->
    el "em" $ renderInlines renderRaw xs
  Strong xs ->
    el "strong" $ renderInlines renderRaw xs
  Underline xs ->
    el "u" $ renderInlines renderRaw xs
  Strikeout xs ->
    el "strike" $ renderInlines renderRaw xs
  Superscript xs ->
    el "sup" $ renderInlines renderRaw xs
  Subscript xs ->
    el "sub" $ renderInlines renderRaw xs
  SmallCaps xs ->
    el "small" $ renderInlines renderRaw xs
  Quoted qt xs ->
    flip inQuotes qt $ renderInlines renderRaw xs
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
  RawInline _ x ->
    -- See comment in RawBlock above
    el "code" $ text x
  Math mathType s ->
    -- http://docs.mathjax.org/en/latest/basic/mathematics.html#tex-and-latex-input
    case mathType of
      InlineMath ->
        elClass "span" "math inline" $ text $ "\\(" <> s <> "\\)"
      DisplayMath ->
        elClass "span" "math display" $ text "$$" >> text s >> text "$$"
  Link attr xs (lUrl, lTitle) -> do
    let attr' = renderAttr attr <> ("href" =: lUrl <> "title" =: lTitle)
    elAttr "a" attr' $ renderInlines renderRaw xs
  Image attr xs (iUrl, iTitle) -> do
    let attr' = renderAttr attr <> ("src" =: iUrl <> "title" =: iTitle)
    elAttr "img" attr' $ renderInlines renderRaw xs
  Note xs -> do
    fs :: Footnotes <- ask
    case Map.lookup (Footnote xs) fs of
      Nothing ->
        -- No footnote in the global map (this means that the user has
        -- defined a footnote inside a footnote); just put the whole thing in
        -- aside.
        elClass "aside" "footnote-inline" $ renderBlocks renderRaw xs
      Just idx ->
        renderFootnoteRef idx
  -- el "aside" $ renderBlocks xs
  Span attr xs ->
    elPandocAttr "span" attr $
      renderInlines renderRaw xs
  where
    inQuotes w = \case
      SingleQuote -> text "❛" >> w <* text "❜"
      DoubleQuote -> text "❝" >> w <* text "❞"
