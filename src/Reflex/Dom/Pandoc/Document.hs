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
    elPandocBlocks,
    PandocBuilder,
    PandocRaw (..),
    URILink (..),
    Config (..),
    defaultConfig,
  )
where

import Control.Monad
import Control.Monad.Reader
import Data.Bool
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T (pack)
import Data.Traversable (for)
import Reflex.Dom.Core hiding (Link, Space, mapAccum)
import Reflex.Dom.Pandoc.Footnotes
import Reflex.Dom.Pandoc.PandocRaw
import Reflex.Dom.Pandoc.SyntaxHighlighting (elCodeHighlighted)
import Reflex.Dom.Pandoc.URILink
import Reflex.Dom.Pandoc.Util (elPandocAttr, headerElement, renderAttr)
import Text.Pandoc.Definition

-- | Like `DomBuilder` but with a capability to render pandoc raw content.
type PandocBuilder t m =
  ( DomBuilder t m,
    PandocRaw m,
    PandocRawConstraints m
  )

data Config t m a = Config
  { -- | Custom link renderer.
    _config_renderURILink :: m a -> URILink -> m a
  }

defaultConfig :: Monad m => Config t m ()
defaultConfig =
  Config $ \f _ -> f >> pure ()

-- | Convert Markdown to HTML
elPandoc :: forall t m a. (PandocBuilder t m, Monoid a) => Config t m a -> Pandoc -> m a
elPandoc cfg doc@(Pandoc _meta blocks) = do
  divClass "pandoc" $ do
    let fs = getFootnotes doc
    x <- flip runReaderT fs $ renderBlocks cfg blocks
    fmap (x <>) $ renderFootnotes (sansFootnotes . renderBlocks cfg) fs

-- | Render list of Pandoc inlines
elPandocInlines :: PandocBuilder t m => [Inline] -> m ()
elPandocInlines = void . sansFootnotes . renderInlines defaultConfig

-- | Render list of Pandoc Blocks
elPandocBlocks :: PandocBuilder t m => [Block] -> m ()
elPandocBlocks = void . sansFootnotes . renderBlocks defaultConfig

mapAccum :: (Monoid b, Applicative f) => (a -> f b) -> [a] -> f b
mapAccum f xs =
  fmap mconcat $ for xs f

renderBlocks :: (PandocBuilder t m, Monoid a) => Config t m a -> [Block] -> ReaderT Footnotes m a
renderBlocks cfg =
  mapAccum $ renderBlock cfg

renderBlock :: (PandocBuilder t m, Monoid a) => Config t m a -> Block -> ReaderT Footnotes m a
renderBlock cfg = \case
  -- Pandoc parses github tasklist as this structure.
  Plain (Str "☐" : Space : is) -> checkboxEl False >> renderInlines cfg is
  Plain (Str "☒" : Space : is) -> checkboxEl True >> renderInlines cfg is
  Para (Str "☐" : Space : is) -> checkboxEl False >> renderInlines cfg is
  Para (Str "☒" : Space : is) -> checkboxEl True >> renderInlines cfg is
  Plain xs ->
    renderInlines cfg xs
  Para xs ->
    el "p" $ renderInlines cfg xs
  LineBlock xss ->
    flip mapAccum xss $ \xs -> do
      renderInlines cfg xs <* text "\n"
  CodeBlock attr x ->
    elCodeHighlighted attr x >> pure mempty
  RawBlock fmt x ->
    elPandocRaw fmt x >> pure mempty
  BlockQuote xs ->
    el "blockquote" $ renderBlocks cfg xs
  OrderedList (idx, style, _delim) xss ->
    -- delimStyle is not supported in HTML or in Semantic UI
    elAttr "ol" (listStyle style <> startFrom idx) $ do
      flip mapAccum xss $ \xs -> do
        el "li" $ renderBlocks cfg xs
  BulletList xss ->
    el "ul" $ flip mapAccum xss $ \xs -> el "li" $ renderBlocks cfg xs
  DefinitionList defs ->
    el "dl" $ flip mapAccum defs $ \(term, descList) -> do
      x <- el "dt" $ renderInlines cfg term
      fmap (x <>) $ flip mapAccum descList $ \desc ->
        el "dd" $ renderBlocks cfg desc
  Header level attr xs ->
    elPandocAttr (headerElement level) attr $ do
      renderInlines cfg xs
  HorizontalRule ->
    el "hr" blank >> pure mempty
  Table _attr _captions _colSpec (TableHead _ hrows) tbodys _tfoot -> do
    -- TODO: Rendering is basic, and needs to handle with all attributes of the AST
    elClass "table" "ui celled table" $ do
      x <- el "thead" $ do
        flip mapAccum hrows $ \(Row _ cells) -> do
          el "tr" $ do
            flip mapAccum cells $ \(Cell _ _ _ _ blks) ->
              el "th" $ renderBlocks cfg blks
      fmap (x <>) $ flip mapAccum tbodys $ \(TableBody _ _ _ rows) ->
        el "tbody" $ do
          flip mapAccum rows $ \(Row _ cells) ->
            el "tr" $ do
              flip mapAccum cells $ \(Cell _ _ _ _ blks) ->
                el "td" $ renderBlocks cfg blks
  Div attr xs ->
    elPandocAttr "div" attr $
      renderBlocks cfg xs
  Null ->
    blank >> pure mempty
  where
    checkboxEl checked = do
      let attrs =
            ( mconcat $ catMaybes $
                [ Just $ "type" =: "checkbox",
                  Just $ "disabled" =: "True",
                  bool Nothing (Just $ "checked" =: "True") checked
                ]
            )
          invisibleChar = "\8206"
      divClass "ui disabled fitted checkbox" $ do
        void $ elAttr "input" attrs blank
        -- Semantic UI requires a non-empty label element
        el "label" $ text invisibleChar
    startFrom idx = bool mempty ("start" =: (T.pack $ show idx)) (idx /= 1)
    listStyle = \case
      LowerRoman -> "type" =: "i"
      UpperRoman -> "type" =: "I"
      LowerAlpha -> "type" =: "a"
      UpperAlpha -> "type" =: "A"
      _ -> mempty

renderInlines :: (PandocBuilder t m, Monoid a) => Config t m a -> [Inline] -> ReaderT Footnotes m a
renderInlines cfg =
  mapAccum $ renderInline cfg

renderInline :: (PandocBuilder t m, Monoid a) => Config t m a -> Inline -> ReaderT Footnotes m a
renderInline cfg = \case
  Str x ->
    text x >> pure mempty
  Emph xs ->
    el "em" $ renderInlines cfg xs
  Strong xs ->
    el "strong" $ renderInlines cfg xs
  Underline xs ->
    el "u" $ renderInlines cfg xs
  Strikeout xs ->
    el "strike" $ renderInlines cfg xs
  Superscript xs ->
    el "sup" $ renderInlines cfg xs
  Subscript xs ->
    el "sub" $ renderInlines cfg xs
  SmallCaps xs ->
    el "small" $ renderInlines cfg xs
  Quoted qt xs ->
    flip inQuotes qt $ renderInlines cfg xs
  Cite _ _ -> do
    el "pre" $ text "error[reflex-doc-pandoc]: Pandoc Cite is not handled"
    pure mempty
  Code attr x ->
    elPandocAttr "code" attr $ do
      text x
      pure mempty
  Space ->
    text " " >> pure mempty
  SoftBreak ->
    text " " >> pure mempty
  LineBreak ->
    text "\n" >> pure mempty
  RawInline fmt x ->
    elPandocRaw fmt x >> pure mempty
  Math mathType s -> do
    -- http://docs.mathjax.org/en/latest/basic/mathematics.html#tex-and-latex-input
    case mathType of
      InlineMath ->
        elClass "span" "math inline" $ text $ "\\(" <> s <> "\\)"
      DisplayMath ->
        elClass "span" "math display" $ text "$$" >> text s >> text "$$"
    pure mempty
  inline@(Link attr xs (lUrl, lTitle)) -> do
    let defaultRender = do
          let attr' = renderAttr attr <> ("href" =: lUrl <> "title" =: lTitle)
          elAttr "a" attr' $ renderInlines cfg xs
    case uriLinkFromInline inline of
      Just uriLink -> do
        fns <- ask
        lift $ _config_renderURILink cfg (flip runReaderT fns defaultRender) uriLink
      Nothing ->
        defaultRender
  Image attr xs (iUrl, iTitle) -> do
    let attr' = renderAttr attr <> ("src" =: iUrl <> "title" =: iTitle)
    elAttr "img" attr' $ renderInlines cfg xs
  Note xs -> do
    fs :: Footnotes <- ask
    case Map.lookup (Footnote xs) fs of
      Nothing ->
        -- No footnote in the global map (this means that the user has
        -- defined a footnote inside a footnote); just put the whole thing in
        -- aside.
        elClass "aside" "footnote-inline" $ renderBlocks cfg xs
      Just idx ->
        renderFootnoteRef idx >> pure mempty
  -- el "aside" $ renderBlocks xs
  Span attr xs ->
    elPandocAttr "span" attr $
      renderInlines cfg xs
  where
    inQuotes w = \case
      SingleQuote -> text "‘" >> w <* text "’"
      DoubleQuote -> text "“" >> w <* text "”"
