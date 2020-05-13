{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Dom.Pandoc.Document
  ( elPandocDoc,
    elPandocInlines,
    elPandocHeading1,
  )
where

import Control.Monad
import Data.Bool
import Data.Maybe
import Reflex.Dom.Core hiding (Link, Space)
import Reflex.Dom.Pandoc.SyntaxHighlighting (elCodeHighlighted)
import Reflex.Dom.Pandoc.Util (elPandocAttr, headerElement, renderAttr)
import Text.Pandoc.Definition

-- | Convert Markdown to HTML
elPandocDoc :: DomBuilder t m => Pandoc -> m ()
elPandocDoc (Pandoc _meta blocks) = mapM_ renderBlock blocks

-- | Render the first level of heading
elPandocHeading1 :: DomBuilder t m => Pandoc -> m ()
elPandocHeading1 (Pandoc _meta blocks) = forM_ blocks $ \case
  Header 1 _ xs -> elPandocInlines xs
  _ -> blank

-- | Render list of Pandoc inlines
--
-- Useful when dealing with metadata values
elPandocInlines :: DomBuilder t m => [Inline] -> m ()
elPandocInlines = mapM_ renderInline

renderBlock :: DomBuilder t m => Block -> m ()
renderBlock = \case
  -- Pandoc parses github tasklist as this structure.
  Plain (Str "☐" : Space : is) -> checkboxEl False >> mapM_ renderInline is
  Plain (Str "☒" : Space : is) -> checkboxEl True >> mapM_ renderInline is
  Para (Str "☐" : Space : is) -> checkboxEl False >> mapM_ renderInline is
  Para (Str "☒" : Space : is) -> checkboxEl True >> mapM_ renderInline is
  Plain xs ->
    mapM_ renderInline xs
  Para xs ->
    el "p" $ mapM_ renderInline xs
  LineBlock xss ->
    forM_ xss $ \xs -> do
      mapM_ renderInline xs
      text "\n"
  CodeBlock attr x ->
    elCodeHighlighted attr x
  RawBlock _ x ->
    -- We are not *yet* sure what to do with this. For now, just dump it in pre.
    elClass "pre" "pandoc-raw" $ text x
  BlockQuote xs -> el "blockquote" $ mapM_ renderBlock xs
  OrderedList _lattr xss ->
    -- TODO: Implement list attributes.
    el "ol" $ do
      forM_ xss $ \xs -> do
        el "li" $ mapM_ renderBlock xs
  BulletList xss ->
    el "ul" $ forM_ xss $ \xs -> el "li" $ mapM_ renderBlock xs
  DefinitionList defs ->
    el "dl" $ forM_ defs $ \(term, descList) -> do
      el "dt" $ mapM_ renderInline term
      forM_ descList $ \desc ->
        el "dd" $ mapM_ renderBlock desc
  Header level attr xs ->
    elPandocAttr (headerElement level) attr $ do
      mapM_ renderInline xs
  HorizontalRule ->
    el "hr" blank
  Table _attr _captions _colSpec (TableHead _ hrows) tbodys _tfoot -> do
    -- TODO: Rendering is basic, and needs to handle with all attributes of the AST
    elClass "table" "ui celled table" $ do
      el "thead" $ do
        forM_ hrows $ \(Row _ cells) -> do
          el "tr" $ do
            forM_ cells $ \(Cell _ _ _ _ blks) ->
              el "th" $ mapM_ renderBlock blks
      forM_ tbodys $ \(TableBody _ _ _ rows) ->
        el "tbody" $ do
          forM_ rows $ \(Row _ cells) ->
            el "tr" $ do
              forM_ cells $ \(Cell _ _ _ _ blks) ->
                el "td" $ mapM_ renderBlock blks
  Div attr xs ->
    elPandocAttr "div" attr $
      mapM_ renderBlock xs
  Null -> blank
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

renderInline :: DomBuilder t m => Inline -> m ()
renderInline = \case
  Str x -> text $ x
  Emph xs -> el "em" $ mapM_ renderInline xs
  Strong xs -> el "strong" $ mapM_ renderInline xs
  Underline xs -> el "u" $ mapM_ renderInline xs
  Strikeout xs -> el "strike" $ mapM_ renderInline xs
  Superscript xs -> el "sup" $ mapM_ renderInline xs
  Subscript xs -> el "sub" $ mapM_ renderInline xs
  SmallCaps xs -> el "small" $ mapM_ renderInline xs
  Quoted qt xs ->
    flip inQuotes qt $ mapM_ renderInline xs
  Cite _ _ -> el "pre" $ text "error[reflex-doc-pandoc]: Pandoc Cite is not handled"
  Code attr x ->
    elPandocAttr "code" attr $
      text x
  Space -> text " "
  SoftBreak -> text " "
  LineBreak -> text "\n"
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
    elAttr "a" attr' $ mapM_ renderInline xs
  Image attr xs (iUrl, iTitle) -> do
    let attr' = renderAttr attr <> ("src" =: iUrl <> "title" =: iTitle)
    elAttr "img" attr' $ mapM_ renderInline xs
  -- TODO: This is wrong; footnotes use this. They should be rendered at bottom, not inline.
  -- Need to do with State monad I think.
  Note xs -> el "aside" $ mapM_ renderBlock xs
  Span attr xs ->
    elPandocAttr "span" attr $
      mapM_ renderInline xs
  where
    inQuotes w = \case
      SingleQuote -> text "❛" >> w >> text "❜"
      DoubleQuote -> text "❝" >> w >> text "❞"
