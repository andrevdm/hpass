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
import qualified CreateNew as CN

version :: Text
version = "0.1.2.0"

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
                            , _stLastGenPassState :: Maybe CN.PrevState
                            , _stDebug :: Text
                            }

makeLenses ''AppState

data ActionF ui next = Halt (AppState ui)
                     | GetSelectedDir (AppState ui) (Maybe Lib.PassDir -> next)
                     | GetSelectedFile (AppState ui) (Maybe Lib.PassFile -> next)
                     | GetPassDetail Lib.PassFile (Either Text Text -> next)
                     | LogError Text next
                     | ClearFiles (AppState ui) (AppState ui -> next)
                     | ShowFiles (AppState ui) [Lib.PassFile] (AppState ui -> next)
                     | RunBaseHandler (AppState ui) (AppState ui -> next)
                     | ClipLine Int Lib.PassFile (Either Int () -> next)

                     -- | Note that ExitAnd*** actions terminate the DSL
                     | ExitAndEditFile Lib.PassFile (Either Text Text -> AppState ui)
                     | ExitAndGenPassword (AppState ui) Text (CN.CreatePasswordResult -> AppState ui)
                     deriving (Functor)

makeFree ''ActionF
--type Action ui = F (ActionF ui)
type Action ui = Free (ActionF ui)


handleKeyPress :: AppState ui -> (K.Key, [K.Modifier]) -> Action ui (AppState ui)
handleKeyPress st (key, ms) =
  case key of
    K.KEsc      -> halt st
    K.KChar 'q' -> halt st
    K.KChar 'n'  -> 
      getSelectedDir st >>= \case
        Nothing -> pure st
        Just d -> exitAndGenPassword st (Lib.pdPassPath d) (createPassword st)

    _ ->
      case st ^. stFocus of
        FoldersControl -> handleFoldersKey st (key, ms)
        FilesControl -> handleFilesKey st (key, ms)


handleFoldersKey :: AppState ui -> (K.Key, [K.Modifier]) -> Action ui (AppState ui)
handleFoldersKey st (key, _) = do
  st' <- case key of
           K.KChar 'l'  -> focusFile
           K.KChar '\t' -> focusFile
           K.KRight     -> focusFile
           _ -> runBaseHandler st

  getSelectedDir st' >>= \case
    Nothing -> clearFiles st'
    Just d -> showFiles st' $ Lib.pdFiles d

  where
    focusFile = pure $ st & stFocus .~ FilesControl
                          & stDetail .~ []


handleFilesKey :: AppState ui -> (K.Key, [K.Modifier]) -> Action ui (AppState ui)
handleFilesKey st (key, []) =
  case key of
    K.KChar 'h'  -> focusFolder
    K.KChar '\t' -> focusFolder
    K.KLeft      -> focusFolder

    K.KChar 'l'  -> showPass
    K.KRight     -> showPass

    K.KChar 'e' ->
      getSelectedFile st >>= \case
        Nothing -> pure st
        Just f -> 
          exitAndEditFile f (\case
                                Right d -> st & stDetail .~ parseDetail d
                                Left e -> st & stDetail .~ []
                                             & stDebug .~ e
                            )

    K.KChar c | (c `elem` ("0123456789" :: [Char])) ->
      getSelectedFile st >>= \case
        Nothing -> pure st
        Just f -> case readMaybe [c] :: Maybe Int of
                    Just i -> clipLine (i + 1) f >>= \case
                      Right _ -> pure $ st & stDebug .~ "copied: " <> show i
                      Left e -> pure $ st & stDebug .~ "error: " <> show e
                    Nothing -> pure st
          
    _ -> runBaseHandler $ st & stDetail .~ []

  where
    focusFolder = pure $ st & stFocus .~ FoldersControl
                            & stDetail .~ []

    showPass =
      getSelectedFile st >>= \case
        Nothing -> pure st
        Just f ->
          getPassDetail f >>= \case
            Left e -> do
              logError e
              pure st
            Right d -> 
              pure $ st & stDetail .~ parseDetail d

handleFilesKey st _ = pure st

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


createPassword :: AppState ui -> CN.CreatePasswordResult -> AppState ui
createPassword st pr =
  let msg = if CN.rSuccess pr
               then "Password created: " <> (CN.rFolder pr <> "/" <> CN.rName pr)
               else "Password creation aborted" in
  
  st & stLastGenPassState .~ (Just . CN.rState $ pr)
     & stDebug .~ msg
