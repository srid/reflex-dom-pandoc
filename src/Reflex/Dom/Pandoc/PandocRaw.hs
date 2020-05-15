{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.Dom.Pandoc.PandocRaw
  ( PandocRaw (..),
    elPandocRawSafe,
  )
where

import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (..), lift)
import Control.Monad.Ref (MonadRef, Ref)
import Control.Monad.State (modify)
import Data.Constraint
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8Builder)
import GHC.IORef
import Reflex.Dom.Core hiding (Link, Space)
import Reflex.Host.Class
import Text.Pandoc.Definition

-- | Class to define how to render pandoc raw nodes
class PandocRaw m where
  -- | The constraints required to render
  type PandocRawConstraints m :: Constraint

  -- | Render a raw content of the given format
  -- TODO: Distinguish between inline vs block
  elPandocRaw :: PandocRawConstraints m => Format -> Text -> m ()

-- | In a static builder, we accept whatever raw html that comes through.
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
  elPandocRaw f@(Format format) s =
    case format of
      x
        | x `elem` ["html", "html4", "html5"] ->
          StaticDomBuilderT $ lift $ modify $ (:) $ fmap encodeUtf8Builder $ current $ constDyn s
      _ ->
        elPandocRawSafe f s

elPandocRawSafe :: DomBuilder t m => Format -> Text -> m ()
elPandocRawSafe (Format format) s =
  elClass "pre" ("pandoc-raw " <> format) $ text s

instance PandocRaw m => PandocRaw (ReaderT a m) where
  type PandocRawConstraints (ReaderT a m) = PandocRawConstraints m
  elPandocRaw f s = ReaderT $ \_ -> elPandocRaw f s

instance PandocRaw m => PandocRaw (PostBuildT t m) where
  type PandocRawConstraints (PostBuildT t m) = PandocRawConstraints m
  elPandocRaw f s = PostBuildT $ ReaderT $ \_ ->
    elPandocRaw f s
