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
import qualified Data.List as Lst
import qualified Data.Vector as Vec
import           Control.Monad.Free.TH
import           Control.Monad.Free
--import           Control.Monad.Free.Church
import qualified Graphics.Vty.Input.Events as K

import qualified Lib
import qualified CreateNew as CN

version :: Text
version = "0.1.5.5"

data Level = LevelInfo
           | LevelWarn
           | LevelError

data Name = FoldersControl
          | FilesControl
          | SearchControl
          deriving (Show, Eq, Ord)


data Message = Message { mText :: !Text
                       , mLevel :: !Level
                       , mTtl :: !Int
                       }

data DetailLine = DetailLine { dlOriginal :: !Text
                             , dlKey :: !Text
                             , dlValue :: !Text
                             }

data AppState ui = AppState { _stRoot :: !FilePath
                            , _stDetail :: ![DetailLine]
                            , _stFocus :: !Name
                            , _stUi :: !ui
                            , _stLastGenPassState :: !(Maybe CN.PrevState)
                            , _stMessage :: !(Maybe Message)
                            , _stShowHelp :: !Bool
                            , _stAutoCloseTtl :: !Int
                            , _stTime :: !Tm.UTCTime
                            , _stSearching :: !Bool
                            , _stDirsCache :: !(Vec.Vector Lib.PassDir)
                            }
makeLenses ''AppState


data StateActionF ui next = StGetSelectedDir (AppState ui) (Maybe Lib.PassDir -> next)
                          | StGetSelectedFile (AppState ui) (Maybe Lib.PassFile -> next)
                          | StLogError (AppState ui) Text (AppState ui -> next)
                          | StClearFiles (AppState ui) (AppState ui -> next)
                          | StShowFiles (AppState ui) [Lib.PassFile] (AppState ui -> next)
                          | StGetDirs (AppState ui) ([Lib.PassDir] -> next)
                          | StReloadDirs (AppState ui) ([Lib.PassDir] -> next)
                          | StUseDirs (AppState ui) [Lib.PassDir] (AppState ui -> next)
                          | StSelectDir (AppState ui) Lib.PassDir (AppState ui -> next)
                          | StGetPassDetail (AppState ui) Lib.PassFile (Either Text Text -> next)
                          | StUpdatePassDetail (AppState ui) Lib.PassFile Text (Maybe Text -> next)
                          | StEditPass (AppState ui) Lib.PassFile (Maybe Text -> next)
                          | StGetSearchText (AppState ui) (Text -> next)
                          | StClearSearch (AppState ui) (AppState ui -> next)
                          deriving (Functor)

makeFree ''StateActionF
type StateAction ui = Free (StateActionF ui)


data EventActionF ui next = Halt (AppState ui)
                          | GetSelectedDir (AppState ui) (Maybe Lib.PassDir -> next)
                          | GetSelectedFile (AppState ui) (Maybe Lib.PassFile -> next)
                          | RunBaseHandler (AppState ui) (AppState ui -> next)
                          | GetPassDetail (AppState ui) Lib.PassFile (Either Text Text -> StateAction ui (AppState ui))
                          | ClipLine (AppState ui) Int Lib.PassFile (Either Int () -> StateAction ui (AppState ui))
                          | RunEditFile (AppState ui) Lib.PassFile (Either Text Text -> StateAction ui (AppState ui))
                          | RunGenPassword (AppState ui) Text (CN.CreatePasswordResult -> StateAction ui (AppState ui))
                          | RunUpdatePassword (AppState ui) Text (CN.CreatePasswordResult -> StateAction ui (AppState ui))
                          | LiftSt (StateAction ui (AppState ui)) (AppState ui -> next)
                          deriving (Functor)

makeFree ''EventActionF
type EventAction ui = Free (EventActionF ui)


initState :: AppState ui -> [Lib.PassDir] -> StateAction ui (AppState ui)
initState st items = 
  -- Show the files for the first folder
  case items of
    (d:_) -> stShowFiles st $ Lib.pdFiles d
    _ -> pure st


handleKeyPress :: AppState ui -> (K.Key, [K.Modifier]) -> EventAction ui (AppState ui)
handleKeyPress st' (key, ms) =
  --Reset auto close TTL
  let st = st' & stAutoCloseTtl .~ defaultAutoCloseTtl in

  if st ^. stSearching
    then handleSearch st
    else handleNormal st
         
  where
    handleNormal st = 
      case key of
        K.KEsc      -> halt st
        K.KChar 'q' -> halt st
        K.KChar '?' -> pure $ st & stShowHelp %~ not
        K.KChar '/' -> pure $ st & stSearching .~ True
                                 & stFocus .~ SearchControl

        K.KChar 'n'  -> 
          getSelectedDir st >>= \case
            Nothing -> pure st
            Just d -> handleCreatePassword st d

        _ ->
          case st ^. stFocus of
            FoldersControl -> handleFoldersKey st (key, ms)
            FilesControl -> handleFilesKey st (key, ms)
            SearchControl -> pure st

    handleSearch st =
      case key of
        K.KEsc -> 
          liftSt $ applySearch =<< stClearSearch (st & stSearching .~ False
                                                     & stFocus .~ FoldersControl
                                                     & stDetail .~ [])


        K.KEnter ->
          liftSt . applySearch $ st & stSearching .~ False
                                    & stFocus .~ FilesControl
                                    & stDetail .~ []

        _ -> do
          st1 <- runBaseHandler st
          liftSt $ applySearch st1
    

handleFoldersKey :: AppState ui -> (K.Key, [K.Modifier]) -> EventAction ui (AppState ui)
handleFoldersKey st (key, _) = do
  st' <- case key of
           K.KChar 'l'  -> focusFile
           K.KChar '\t' -> focusFile
           K.KRight     -> focusFile
           _ -> runBaseHandler st

  getSelectedDir st' >>= \case
    Nothing -> liftSt $ stClearFiles st'
    Just d -> liftSt $ stShowFiles st' $ Lib.pdFiles d

  where
    focusFile = pure $ st & stFocus .~ FilesControl
                          & stDetail .~ []

handleFilesKey :: AppState ui -> (K.Key, [K.Modifier]) -> EventAction ui (AppState ui)
handleFilesKey st (key, []) =
  case key of
    K.KChar 'h'  -> focusFolder
    K.KChar '\t' -> focusFolder
    K.KLeft      -> focusFolder

    K.KChar 'l'  -> showPass 
    K.KRight     -> showPass

    K.KChar 'u' ->
      getSelectedFile st >>= \case
        Nothing -> pure st
        Just f -> handleUpdatePassword st f

    K.KChar 'e' ->
      getSelectedFile st >>= \case
        Nothing -> pure st
        Just f -> 
          runEditFile st f (\case
                               Right d -> pure $ st & stDetail .~ parseDetail d
                               Left e -> pure $ st & stDetail .~ []
                                                   & stMessage .~ Just (Message e LevelError defaultMessageTtl))

    K.KChar c | c `elem` ("0123456789" :: [Char]) ->
      getSelectedFile st >>= \case
        Nothing -> pure st
        Just f -> case readMaybe [c] :: Maybe Int of
                    Just i -> clipLine st (i + 1) f $ \case
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
          getPassDetail st f $ \case
            Left e -> stLogError st e
            Right d -> pure $ st & stDetail .~ parseDetail d
                                 & stShowHelp .~ False

handleFilesKey st _ = pure st


handleTick :: AppState ui -> Tm.UTCTime -> EventAction ui (AppState ui)
handleTick st' tm =
  checkAutoClose $ checkMessage st'

  where
    checkAutoClose st = do
      let s = st & stAutoCloseTtl %~ flip (-) 1
      let ttl = st ^. stAutoCloseTtl
      if ttl <= 0
        then halt s
        else pure $ s & stTime .~ tm
    
    checkMessage st =
      case st ^. stMessage of
        Nothing ->
          st
        Just (Message msg lvl ttl) ->
          if ttl <= 1
            then st & stMessage .~ Nothing
            else st & stMessage .~ Just (Message msg lvl (ttl - 1))

  
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
        (v, "") ->
          DetailLine s "" v

        (k, v) ->
          if Txt.isPrefixOf "password_" k
            then DetailLine s k "*****"
            else DetailLine s k $ Txt.drop 1 v

    noPwd pwd s =
      if Txt.null pwd
        then s
        else Txt.replace pwd "*****" s


handleUpdatePassword :: MonadFree (EventActionF ui) m => AppState ui -> Lib.PassFile -> m (AppState ui)
handleUpdatePassword st f =
  runUpdatePassword st (Lib.pfPassPath f) $ \pr -> do
    let validated = validatePassword False pr
    msg <- case validated of
             Just e -> pure $ Message e LevelError defaultMessageTtl
             Nothing ->
               stGetPassDetail st f >>= \case
                 Left e -> pure $ Message ("Failed to read password text. " <> e) LevelInfo defaultMessageTtl
                 Right pwd -> do
                   let ls = Txt.lines pwd
                   let old = case ls of
                               (p:_) -> p
                               _ -> ""

                   let dt = Txt.pack $ Tm.formatTime Tm.defaultTimeLocale "%Y-%m-%d" $ st ^. stTime
                   let new = Txt.intercalate "\n" $ [CN.rPassword pr] <> drop 1 ls <> ["password_" <> dt <> ": " <> old]

                   stUpdatePassDetail st f new >>= \case
                     Just e ->
                       pure $ Message ("Failed to read password text. " <> e) LevelInfo defaultMessageTtl
                     Nothing -> do
                       if CN.rEditAfter pr
                         then void $ stEditPass st f
                         else pass
                       pure $ Message ("Password updated: " <> (CN.rFolder pr <> "/" <> CN.rName pr)) LevelInfo defaultMessageTtl

    pure $ st & stLastGenPassState .~ (Just . CN.rState $ pr)
              & stMessage .~ Just msg
              & stDetail .~ []


handleCreatePassword :: MonadFree (EventActionF ui) m => AppState ui -> Lib.PassDir -> m (AppState ui)
handleCreatePassword st dir = 
  runGenPassword st (Lib.pdPassPath dir) $ \pr -> do
    let validated = validatePassword True pr
    let msg = case validated of
                Nothing -> Message ("Password created: " <> (CN.rFolder pr <> "/" <> CN.rName pr)) LevelInfo defaultMessageTtl
                Just e -> Message e LevelError defaultMessageTtl

    st' <- if isNothing validated
             then reloadDirs st
             else pure st
      
    pure $ st' & stLastGenPassState .~ (Just . CN.rState $ pr)
               & stMessage .~ Just msg


validatePassword :: Bool -> CN.CreatePasswordResult -> Maybe Text
validatePassword newPassword pr 
  | not (CN.rSuccess pr) = Just "Password creation cancelled"
  | Txt.length (CN.rPassword pr) < 3 = Just "Password creation aborted. Password must be longer than 3 chars"
  | newPassword && Txt.null (CN.rName pr) = Just "Password creation aborted. Name is required"
  | otherwise = Nothing


reloadDirs :: MonadFree (StateActionF ui) m => AppState ui -> m (AppState ui)
reloadDirs st1 =
  stGetSelectedDir st1 >>= \case
    Just d -> do
      dirs <- stReloadDirs st1
      st2' <- stUseDirs st1 dirs
      let st3 = st2' & stDirsCache .~ Vec.fromList dirs

      case filter (\search -> Lib.pdPath d == Lib.pdPath search) dirs of
        (found : _) -> do
          -- Select the directory
          st4 <- stSelectDir st3 found
          -- Show the selected dir's files
          stShowFiles st4 $ Lib.pdFiles found

        _ ->
          pure st3

    Nothing ->
      pure st1
  

applySearch :: AppState ui -> StateAction ui (AppState ui)
applySearch st = do
  search <- Txt.toLower . Txt.strip <$> stGetSearchText st
  let ds1 = Vec.toList $ st ^. stDirsCache

  let ds2 = if Txt.null search
              then ds1
              else filterDir search <$> ds1
  
  let ds3 = filter (not . Lst.null . Lib.pdFiles) ds2

  st2 <- stUseDirs st ds3
  
  case ds3 of
    (a:_) -> stShowFiles st2 $ Lib.pdFiles a
    _ -> pure st2

  where
    filterDir s d =
      if Txt.isInfixOf s . Txt.toLower $ Lib.pdName d
        then d
        else d { Lib.pdFiles = filter (Txt.isInfixOf s . Txt.toLower . Lib.pfName) $ Lib.pdFiles d }



defaultTickPeriodMicroSeconds :: Int
defaultTickPeriodMicroSeconds = 2 * 1000000

defaultMessageTtl :: Int
defaultMessageTtl = 4

defaultAutoCloseTtl :: Int
defaultAutoCloseTtl = 60
