{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Dom.Pandoc.SyntaxHighlighting where

import Control.Monad (forM_, msum)
import Data.Text (Text)
import Reflex.Dom.Core
import Reflex.Dom.Pandoc.Util (elPandocAttr)
import qualified Skylighting as S
import Text.Pandoc.Definition (Attr)
import Prelude hiding (lines)

elCodeHighlighted ::
  forall t m.
  DomBuilder t m =>
  -- | Pandoc attribute object. TODO: Use a sensible type.
  Attr ->
  -- | Code to highlight.
  Text ->
  m ()
elCodeHighlighted attr@(_, langClasses, _) x = do
  case tokenizeForOneOfLang langClasses x of
    Nothing -> do
      divClass "pandoc-code nosyntax" $ do
        el "pre" $
          elPandocAttr "code" attr $
            text x
    Just lines ->
      divClass "pandoc-code highlighted" $ do
        el "pre" $
          elPandocAttr "code" attr $ do
            forM_ lines $ \line -> do
              forM_ line $ \(tokType, tok) ->
                elClass "span" (tokenClass tokType) $ text tok
              text "\n"
  where
    tokenizeForOneOfLang langs s = do
      syntax <- msum (fmap (`S.lookupSyntax` S.defaultSyntaxMap) langs)
      case S.tokenize tokenizerConfig syntax s of
        Left _ -> Nothing
        Right lines -> pure lines
    tokenizerConfig =
      S.TokenizerConfig
        { S.syntaxMap = S.defaultSyntaxMap,
          S.traceOutput = False
        }

tokenClass :: S.TokenType -> Text
tokenClass = \case
  S.KeywordTok -> "kw"
  S.DataTypeTok -> "dt"
  S.DecValTok -> "dv"
  S.BaseNTok -> "bn"
  S.FloatTok -> "fl"
  S.CharTok -> "ch"
  S.StringTok -> "st"
  S.CommentTok -> "co"
  S.OtherTok -> "ot"
  S.AlertTok -> "al"
  S.FunctionTok -> "fu"
  S.RegionMarkerTok -> "re"
  S.ErrorTok -> "er"
  S.ConstantTok -> "cn"
  S.SpecialCharTok -> "sc"
  S.VerbatimStringTok -> "vs"
  S.SpecialStringTok -> "ss"
  S.ImportTok -> "im"
  S.DocumentationTok -> "do"
  S.AnnotationTok -> "an"
  S.CommentVarTok -> "cv"
  S.VariableTok -> "va"
  S.ControlFlowTok -> "cf"
  S.OperatorTok -> "op"
  S.BuiltInTok -> "bu"
  S.ExtensionTok -> "ex"
  S.PreprocessorTok -> "pp"
  S.AttributeTok -> "at"
  S.InformationTok -> "in"
  S.WarningTok -> "wa"
  S.NormalTok -> ""
