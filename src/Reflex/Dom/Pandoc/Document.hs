{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.Dom.Pandoc.Document
  ( elPandocDoc,
    elPandocInlines,
    elPandocHeading1,
  )
where

import Control.Monad
import Control.Monad.Writer
import Data.Bool
import Data.Maybe
import Reflex.Dom.Core hiding (Link, Space)
import Reflex.Dom.Pandoc.SyntaxHighlighting (elCodeHighlighted)
import Reflex.Dom.Pandoc.Util (elPandocAttr, headerElement, renderAttr)
import Text.Pandoc.Definition

--newtype PandocT t m a = PandocT
--  { unPandocT :: WriterT [Footnote] m a
--  }
--  deriving (Functor, Applicative, Monad, DomBuilder t)
--
--instance (Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (PandocT t m) where
--  runWithReplace a0 a' = PandocT $ WriterT $ runWithReplace (unPandocT a0) (fmapCheap unPandocT a')
--  traverseDMapWithKeyWithAdjust f dm0 dm' = PandocT $ traverseDMapWithKeyWithAdjust (coerce f) dm0 dm'
--  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = PandocT $ traverseDMapWithKeyWithAdjustWithMove (coerce f) dm0 dm'
--  traverseIntMapWithKeyWithAdjust f im0 im' = PandocT $ traverseIntMapWithKeyWithAdjust (coerce f) im0 im'
--

-- | Convert Markdown to HTML
elPandocDoc :: forall t m. DomBuilder t m => Pandoc -> m ()
elPandocDoc (Pandoc _meta blocks) = do
  footnotes :: [Footnote] <-
    renderBlocks blocks
  renderFootnotes footnotes

renderFootnotes :: DomBuilder t m => [Footnote] -> m ()
renderFootnotes footnotes = do
  unless (null footnotes) $ do
    elAttr "div" ("id" =: "footnotes") $ do
      el "h2" $ text "Footnotes"
      el "ol" $ forM_ footnotes $ \(Footnote blks) -> do
        el "li" $ do
          -- We discard any footnotes inside footnotes
          _ :: [Footnote] <- renderBlocks blks
          pure ()

-- | Render the first level of heading
elPandocHeading1 :: (MonadWriter [Footnote] m, DomBuilder t m) => Pandoc -> m ()
elPandocHeading1 (Pandoc _meta blocks) = forM_ blocks $ \case
  Header 1 _ xs -> elPandocInlines xs
  _ -> blank

-- | Render list of Pandoc inlines
--
-- Useful when dealing with metadata values
elPandocInlines :: DomBuilder t m => [Inline] -> m ()
elPandocInlines = void . renderInlines

newtype Footnote = Footnote {unFootnote :: [Block]}
  deriving (Eq, Show, Ord)

renderBlocks :: DomBuilder t m => [Block] -> m [Footnote]
renderBlocks =
  fmap concat . mapM renderBlock

renderBlock :: DomBuilder t m => Block -> m [Footnote]
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
    fmap concat $ forM xss $ \xs -> do
      renderInlines xs <* text "\n"
  CodeBlock attr x ->
    voidM $ elCodeHighlighted attr x
  RawBlock _ x ->
    -- We are not *yet* sure what to do with this. For now, just dump it in pre.
    voidM $ elClass "pre" "pandoc-raw" $ text x
  BlockQuote xs ->
    el "blockquote" $ renderBlocks xs
  OrderedList _lattr xss ->
    -- TODO: Implement list attributes.
    el "ol" $ do
      fmap concat $ forM xss $ \xs -> do
        el "li" $ renderBlocks xs
  BulletList xss ->
    el "ul" $ fmap concat $ forM xss $ \xs -> el "li" $ renderBlocks xs
  DefinitionList defs ->
    fmap concat $ el "dl" $ forM defs $ \(term, descList) -> do
      fs :: [Footnote] <- el "dt" $ renderInlines term
      fs1 :: [Footnote] <- fmap concat $ forM descList $ \desc ->
        el "dd" $ renderBlocks desc
      pure $ fs <> fs1
  Header level attr xs ->
    elPandocAttr (headerElement level) attr $ do
      renderInlines xs
  HorizontalRule ->
    voidM $ el "hr" blank
  Table _attr _captions _colSpec (TableHead _ hrows) tbodys _tfoot -> do
    -- TODO: Rendering is basic, and needs to handle with all attributes of the AST
    elClass "table" "ui celled table" $ do
      el "thead" $ do
        fmap concat $ forM hrows $ \(Row _ cells) -> do
          el "tr" $ do
            fmap concat $ forM cells $ \(Cell _ _ _ _ blks) ->
              el "th" $ renderBlocks blks
      fmap concat $ forM tbodys $ \(TableBody _ _ _ rows) ->
        el "tbody" $ do
          fmap concat $ forM rows $ \(Row _ cells) ->
            el "tr" $ do
              fmap concat $ forM cells $ \(Cell _ _ _ _ blks) ->
                el "td" $ renderBlocks blks
  Div attr xs ->
    elPandocAttr "div" attr $
      renderBlocks xs
  Null ->
    voidM blank
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

renderInlines :: DomBuilder t m => [Inline] -> m [Footnote]
renderInlines = fmap concat . mapM renderInline

renderInline :: DomBuilder t m => Inline -> m [Footnote]
renderInline = \case
  Str x ->
    voidM $ text x
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
    voidM $ el "pre" $ text "error[reflex-doc-pandoc]: Pandoc Cite is not handled"
  Code attr x ->
    voidM $ elPandocAttr "code" attr $
      text x
  Space ->
    voidM $ text " "
  SoftBreak ->
    voidM $ text " "
  LineBreak ->
    voidM $ text "\n"
  RawInline _ x ->
    -- See comment in RawBlock above
    voidM $ el "code" $ text x
  Math mathType s ->
    -- http://docs.mathjax.org/en/latest/basic/mathematics.html#tex-and-latex-input
    voidM $ case mathType of
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
  -- TODO: This is wrong; footnotes use this. They should be rendered at bottom, not inline.
  -- Need to do with State monad I think.
  Note xs -> do
    -- TODO: Use actual footnote number (using StateT?)
    elClass "sup" "footnote-ref" $ do
      elAttr "a" ("href" =: "#footnotes") $ text "fn"
    pure [Footnote xs]
  -- el "aside" $ renderBlocks xs
  Span attr xs ->
    elPandocAttr "span" attr $
      renderInlines xs
  where
    inQuotes w = \case
      SingleQuote -> text "❛" >> w <* text "❜"
      DoubleQuote -> text "❝" >> w <* text "❞"

voidM :: forall t m a b. (Monoid b, DomBuilder t m) => m a -> m b
voidM w = w >> pure mempty
