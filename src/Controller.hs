{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Controller where

import           Protolude
import           Control.Lens ((^.), (.~))
import           Control.Lens.TH (makeLenses)
import qualified Data.Text as Txt
-- import qualified Data.Vector as Vec
import           Control.Monad.Free.TH
import           Control.Monad.Free.Church
import qualified Graphics.Vty.Input.Events as K

import qualified Lib

data DetailLine = DetailLine { dlOriginal :: Text
                             , dlKey :: Text
                             , dlValue :: Text
                             }

data Name = FoldersControl
          | FilesControl
          deriving (Show, Eq)

data AppState ui = AppState { _stPassRoot :: Lib.PassDir
                            , _stDetail :: [DetailLine]
                            , _stFocus :: Name
                            , _stUi :: ui
                            }

makeLenses ''AppState

data ActionF ui next = Halt
                     | GetSelectedDir (Maybe Lib.PassDir -> next)
                     | GetSelectedFile (Maybe Lib.PassFile -> next)
                     | GetPassDetail Lib.PassFile (Either Text Text -> next)
                     | LogError Text next
                     | EditFile Lib.PassFile next
                     | ClearFiles next
                     | ShowFiles [Lib.PassFile] next
                     | RunBaseHandler (AppState ui) (AppState ui -> next)
                     deriving (Functor)

makeFree ''ActionF
type Action ui = F (ActionF ui)


handleKeyPress :: AppState ui -> (K.Key, [K.Modifier]) -> Action ui (AppState ui)
handleKeyPress st (key, ms) =
  case key of
    K.KChar 'q' ->
      halt
    _ ->
      case st ^. stFocus of
        FoldersControl -> handleFoldersKey st (key, ms)
        FilesControl -> handleFilesKey st (key, ms)


handleFoldersKey :: AppState ui -> (K.Key, [K.Modifier]) -> Action ui (AppState ui)
handleFoldersKey st (key, _) = do
  st' <- case key of
           K.KChar 'l'  -> focusFile
           K.KChar '\t' -> focusFile
           K.KLeft      -> focusFile
           _ -> runBaseHandler st

  getSelectedDir >>= \case
    Nothing -> do
      clearFiles
      pure st'
    Just d -> do
      showFiles $ Lib.pdFiles d
      pure st'

  where
    focusFile = pure $ st & stFocus .~ FilesControl
                          & stDetail .~ []


handleFilesKey :: AppState ui -> (K.Key, [K.Modifier]) -> Action ui (AppState ui)
handleFilesKey st (key, _) =
  case key of
    K.KChar 'h'  -> focusFolder
    K.KChar '\t' -> focusFolder
    K.KRight     -> focusFolder

    K.KChar 'l'  -> 
      getSelectedFile >>= \case
        Nothing -> pure st
        Just f ->
          getPassDetail f >>= \case
            Left e -> do
              logError e
              pure st
            Right d -> 
              pure $ st & stDetail .~ parseDetail d

    K.KChar 'e' ->
      getSelectedFile >>= \case
        Nothing -> pure st
        Just f -> do
          editFile f
          getPassDetail f >>= \case
            Right d ->
              pure $ st & stDetail .~ parseDetail d
            Left e -> do
              logError e
              pure $ st & stDetail .~ []

    _ -> runBaseHandler st

  where
    focusFolder = pure $ st & stFocus .~ FoldersControl
                            & stDetail .~ []

parseDetail :: Text -> [DetailLine]
parseDetail d =
  let ls = Txt.lines d in
  let pwd = case ls of
              [] -> ""
              (a:_) -> a in

  go . noPwd pwd <$> ls

  where
    go s =
      case Txt.breakOn ":" s of
        (v, "") -> DetailLine s "" v
        (k, v)  -> DetailLine s k v

    noPwd pwd s =
      if Txt.null pwd
        then s
        else Txt.replace pwd "*****" s
