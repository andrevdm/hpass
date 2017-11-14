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
import           Control.Monad.Free.TH
import           Control.Monad.Free
--import           Control.Monad.Free.Church
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
                            , _stDebug :: Text
                            }

makeLenses ''AppState

data ActionF ui next = Halt (AppState ui)
                     | GetSelectedDir (AppState ui) (Maybe Lib.PassDir -> next)
                     | GetSelectedFile (AppState ui) (Maybe Lib.PassFile -> next)
                     | GetPassDetail Lib.PassFile (Either Text Text -> next)
                     | LogError Text next
                     | EditFile (AppState ui) Lib.PassFile next
                     | ClearFiles (AppState ui) (AppState ui -> next)
                     | ShowFiles (AppState ui) [Lib.PassFile] (AppState ui -> next)
                     | RunBaseHandler (AppState ui) (AppState ui -> next)
                     deriving (Functor)

makeFree ''ActionF
--type Action ui = F (ActionF ui)
type Action ui = Free (ActionF ui)


handleKeyPress :: AppState ui -> (K.Key, [K.Modifier]) -> Action ui (AppState ui)
handleKeyPress st (key, ms) =
  case key of
    K.KEsc      -> halt st
    K.KChar 'q' -> halt st
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

  getSelectedDir st' >>= \case
    Nothing -> clearFiles st'
    Just d -> showFiles st' $ Lib.pdFiles d

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
      getSelectedFile st >>= \case
        Nothing -> pure st
        Just f ->
          getPassDetail f >>= \case
            Left e -> do
              logError e
              pure st
            Right d -> 
              pure $ st & stDetail .~ parseDetail d

    K.KChar 'e' ->
      getSelectedFile st >>= \case
        Nothing -> pure st
        Just f -> do
          editFile st f
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
