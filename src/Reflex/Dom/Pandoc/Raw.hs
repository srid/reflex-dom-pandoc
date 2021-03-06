{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module exists primarily so as to provide an alternative to
-- `elDynHtml'` that works with the static builder. `elRawHtml` is pretty much
-- what you typically need; and if you are on GHCJS, you should define how it
-- will behave via writing instances for `PandocRaw`.
module Reflex.Dom.Pandoc.Raw
  ( RawBuilder,
    elRawHtml,
    PandocRawNode (..),
    elPandocRawNodeSafe,
    PandocRaw (..),
  )
where

import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (..), lift)
import Control.Monad.Ref (MonadRef, Ref)
import Control.Monad.State (modify)
import Data.Constraint (Constraint)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8Builder)
import GHC.IORef (IORef)
import Reflex.Dom.Core hiding (Link, Space)
import Reflex.Host.Class (MonadReflexCreateTrigger)
import Text.Pandoc.Definition (Format (..))

type RawBuilder m = (PandocRaw m, PandocRawConstraints m)

elRawHtml :: (RawBuilder m) => Text -> m ()
elRawHtml =
  elPandocRaw . PandocRawNode_Block "html"

_elRawHtmlExample :: IO ()
_elRawHtmlExample = do
  _ <- renderStatic $ do
    text "some <Text/> before"
    elRawHtml "<b>hello</b> world"
    text "some <Text/> after"
  pure ()

data PandocRawNode
  = PandocRawNode_Block Format Text
  | PandocRawNode_Inline Format Text
  deriving (Eq, Show)

elPandocRawNodeSafe :: DomBuilder t m => PandocRawNode -> m ()
elPandocRawNodeSafe = \case
  PandocRawNode_Block fmt s ->
    elPandocRawSafe "div" fmt s
  PandocRawNode_Inline fmt s ->
    elPandocRawSafe "span" fmt s

-- | Class to define how to render pandoc raw nodes
class PandocRaw m where
  -- | The constraints required to render
  type PandocRawConstraints m :: Constraint

  -- | Render a raw content of the given format
  elPandocRaw :: PandocRawConstraints m => PandocRawNode -> m ()

-- | In a static builder, we accept whatever raw html that comes through.
--
-- Non-html formats are rendered as-is.
instance PandocRaw (StaticDomBuilderT t m) where
  type
    PandocRawConstraints (StaticDomBuilderT t m) =
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
      )
  elPandocRaw = \case
    PandocRawNode_Block "html" s ->
      elPandocRawHtmlStatic s
    PandocRawNode_Inline "html" s ->
      elPandocRawHtmlStatic s
    x ->
      elPandocRawNodeSafe x

elPandocRawHtmlStatic :: (Monad m, Reflex t) => Text -> StaticDomBuilderT t m ()
elPandocRawHtmlStatic s =
  let html = encodeUtf8Builder <$> current (constDyn s)
   in StaticDomBuilderT $
        lift $
          modify $ (:) html

elPandocRawSafe :: DomBuilder t m => Text -> Format -> Text -> m ()
elPandocRawSafe e (Format fmt) s =
  elClass e ("pandoc-raw-" <> fmt <> "-block") $ text s

instance PandocRaw m => PandocRaw (ReaderT a m) where
  type PandocRawConstraints (ReaderT a m) = PandocRawConstraints m
  elPandocRaw x = ReaderT $ \_ -> elPandocRaw x

instance PandocRaw m => PandocRaw (PostBuildT t m) where
  type PandocRawConstraints (PostBuildT t m) = PandocRawConstraints m
  elPandocRaw x = PostBuildT $
    ReaderT $ \_ ->
      elPandocRaw x

instance PandocRaw m => PandocRaw (HydratableT m) where
  type PandocRawConstraints (HydratableT m) = PandocRawConstraints m
  elPandocRaw x = HydratableT $ elPandocRaw x
