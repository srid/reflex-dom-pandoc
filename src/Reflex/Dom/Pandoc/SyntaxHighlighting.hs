{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Reflex.Dom.Pandoc.SyntaxHighlighting where

import Control.Monad (forM_)
import Data.Text (Text)

import Clay
import Reflex.Dom.Core
import Text.Pandoc.Definition (Attr)

import qualified Skylighting as S
-- import Text.Pandoc.Highlighting (highlight)

import Reflex.Dom.Pandoc.Util (addClass, elPandocAttr)

elCodeHighlighted
  :: forall t m. DomBuilder t m
  => Attr
  -- ^ Pandoc attribute object. TODO: Use a sensible type.
  -> Text
  -- ^ Code to highlight.
  -> m ()
elCodeHighlighted attrs _x =
  elPandocAttr "pre" (addClass "sourceCode" attrs) $ el "code" $
    -- TODO: Use skylight directly like MMark does
    -- https://github.com/mmark-md/mmark-ext/blob/master/Text/MMark/Extension/Skylighting.hs#L69
    text $ "TODO: Not Implemented (syntax highlighting)"
    --case highlight S.defaultSyntaxMap formatCode attrs x of
    --  Left _err -> text $ T.pack x
    --  Right w -> w
  where

-- | Highlight code syntax with Reflex widgets
formatCode :: DomBuilder t m => S.FormatOptions -> [S.SourceLine] -> m ()
formatCode _opts slines = forM_ slines $ \tokens -> do
  forM_ tokens $ \(tokenType, val) ->
    elClass "span" (tokenClass tokenType) $ text val
  text "\n"
  where
    -- | Get the CSS class name for a Skylighting token type.
    -- This mirors the unexported `short` function from `Skylighting.Format.HTML`
    tokenClass = \case
      S.KeywordTok        -> "kw"
      S.DataTypeTok       -> "dt"
      S.DecValTok         -> "dv"
      S.BaseNTok          -> "bn"
      S.FloatTok          -> "fl"
      S.CharTok           -> "ch"
      S.StringTok         -> "st"
      S.CommentTok        -> "co"
      S.OtherTok          -> "ot"
      S.AlertTok          -> "al"
      S.FunctionTok       -> "fu"
      S.RegionMarkerTok   -> "re"
      S.ErrorTok          -> "er"
      S.ConstantTok       -> "cn"
      S.SpecialCharTok    -> "sc"
      S.VerbatimStringTok -> "vs"
      S.SpecialStringTok  -> "ss"
      S.ImportTok         -> "im"
      S.DocumentationTok  -> "do"
      S.AnnotationTok     -> "an"
      S.CommentVarTok     -> "cv"
      S.VariableTok       -> "va"
      S.ControlFlowTok    -> "cf"
      S.OperatorTok       -> "op"
      S.BuiltInTok        -> "bu"
      S.ExtensionTok      -> "ex"
      S.PreprocessorTok   -> "pp"
      S.AttributeTok      -> "at"
      S.InformationTok    -> "in"
      S.WarningTok        -> "wa"
      S.NormalTok         -> ""

-- | Highlighting style for code blocks
-- TODO: Support theme files: https://pandoc.org/MANUAL.html#syntax-highlighting
style :: Css
style = do
  let bgColor = "#F5FCFF"
      fgColor = "#268BD2"
  pre ?
    backgroundColor bgColor
  code ? do
    backgroundColor bgColor
    color fgColor
  ".sourcecode" ? do
    -- Keyword
    ".kw" ? color "#600095"
    -- Datatype
    ".dt" ? color fgColor
    -- Decimal value, BaseNTok, Float
    forM_ [".dv", ".bn", ".fl"] $ \sel -> sel ? color "#AE81FF"
    -- Char
    ".ch" ? color "#37ad2d"
    -- String
    ".st" ? color "#37ad2d"
    -- Comment
    ".co" ? color "#7e8e91"
    -- Other
    ".ot" ? color "#eb005b"
    -- Alert
    ".al" ? (color "#a6e22e" >> fontWeight bold)
    -- Function
    ".fu" ? color "#333"
    -- Region marker
    ".re" ? pure ()
    -- Error
    ".er" ? (color "#e6db74" >> fontWeight bold)
