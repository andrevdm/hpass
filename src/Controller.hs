{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Controller where

import           Protolude
import           Control.Lens ((^.), (.~), (%~))
import           Control.Lens.TH (makeLenses)
import qualified Data.Text as Txt
import qualified Data.Time as Tm
import           Control.Monad.Free.TH
import           Control.Monad.Free
--import           Control.Monad.Free.Church
import qualified Graphics.Vty.Input.Events as K

import qualified Lib
import qualified CreateNew as CN

version :: Text
version = "0.1.3.2"

data Level = LevelInfo
           | LevelWarn
           | LevelError

data Message = Message { mText :: Text
                       , mLevel :: Level
                       , mTtl :: Int
                       }

defaultMessageTtl :: Int
defaultMessageTtl = 2

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
                            , _stMessage :: Maybe Message
                            , _stShowHelp :: Bool
                            }

makeLenses ''AppState

data UiActionF ui next = Halt (AppState ui)
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
                     | ExitAndGenPassword (AppState ui)
                                          Text
                                          (CN.CreatePasswordResult -> Maybe Text)
                                          (CN.CreatePasswordResult -> AppState ui)
                     deriving (Functor)

makeFree ''UiActionF
type UiAction ui = Free (UiActionF ui)


handleKeyPress :: AppState ui -> (K.Key, [K.Modifier]) -> UiAction ui (AppState ui)
handleKeyPress st (key, ms) =
  case key of
    K.KEsc      -> halt st
    K.KChar 'q' -> halt st
    K.KChar '?' -> pure $ st & stShowHelp %~ not
    K.KChar 'n'  -> 
      getSelectedDir st >>= \case
        Nothing -> pure st
        Just d -> exitAndGenPassword st (Lib.pdPassPath d) validatePassword (createPassword st)

    _ ->
      case st ^. stFocus of
        FoldersControl -> handleFoldersKey st (key, ms)
        FilesControl -> handleFilesKey st (key, ms)


handleFoldersKey :: AppState ui -> (K.Key, [K.Modifier]) -> UiAction ui (AppState ui)
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


handleFilesKey :: AppState ui -> (K.Key, [K.Modifier]) -> UiAction ui (AppState ui)
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
                                             & stMessage .~ Just (Message e LevelError defaultMessageTtl)
                            )

    K.KChar c | (c `elem` ("0123456789" :: [Char])) ->
      getSelectedFile st >>= \case
        Nothing -> pure st
        Just f -> case readMaybe [c] :: Maybe Int of
                    Just i -> clipLine (i + 1) f >>= \case
                      Right _ -> pure $ st & stMessage .~ Just (Message ("copied: " <> show i) LevelInfo defaultMessageTtl)
                      Left e -> pure $ st & stMessage .~ Just (Message ("error: " <> show e) LevelInfo defaultMessageTtl)
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
                        & stShowHelp .~ False

handleFilesKey st _ = pure st


handleTick :: AppState ui -> Tm.UTCTime -> UiAction ui (AppState ui)
handleTick st _ =
  case st ^. stMessage of
    Nothing ->
      pure st
    Just (Message msg lvl ttl) ->
      if ttl <= 1
         then pure $ st & stMessage .~ Nothing
         else pure $ st & stMessage .~ Just (Message msg lvl (ttl - 1))

  
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


validatePassword :: CN.CreatePasswordResult -> Maybe Text
validatePassword pr 
  | not (CN.rSuccess pr) = Just "Password creation cancelled"
  | Txt.length (CN.rPassword pr) < 3 = Just "Password creation aborted. Password must be longer than 3 chars"
  | Txt.null (CN.rName pr) = Just "Password creation aborted. Name is required"
  | otherwise = Nothing


createPassword :: AppState ui -> CN.CreatePasswordResult -> AppState ui
createPassword st pr =
  let msg = if CN.rSuccess pr
               then Message ("Password created: " <> (CN.rFolder pr <> "/" <> CN.rName pr)) LevelInfo defaultMessageTtl
               else Message (fromMaybe "Password creation aborted" $ CN.rErrorMessage pr) LevelError defaultMessageTtl in
  
  st & stLastGenPassState .~ (Just . CN.rState $ pr)
     & stMessage .~ Just msg
