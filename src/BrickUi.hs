{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module BrickUi where

import           Protolude
import           Control.Lens ((^.), (.~), (%~))
import           Control.Lens.TH (makeLenses)
import           Data.Time (UTCTime)
import qualified Data.Time as Tm
import qualified Data.Text as Txt
import qualified Data.Vector as Vec
import           Brick ((<+>), (<=>))
import qualified Brick as B
import qualified Brick.BChan as BCh
import qualified Brick.Markup as BM
import           Brick.Markup ((@?))
import qualified Brick.AttrMap as BA
import qualified Brick.Widgets.List as BL
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.Border.Style as BBS
import           Control.Monad.Free (Free(..))
import           Control.Concurrent (threadDelay, forkIO)
--import           Control.Monad.Free.Church as Fr
import qualified Graphics.Vty as V

import qualified Lib
import qualified CreateNew as CN
import qualified Controller as C

newtype Event = EventTick UTCTime

data BrickState = BrickState { _bListDir :: !(BL.List C.Name Lib.PassDir)
                             , _bListFile :: !(BL.List C.Name Lib.PassFile)
                             , _bEditSearch :: !(BE.Editor Text C.Name)
                             }

makeLenses ''BrickState

type UIState = C.AppState BrickState

main :: IO ()
main = 
  Lib.getPassRoot >>= \case
    Just root -> do
      ps <- Lib.loadPass 0 "/" root
      let items' = Lib.flattenDirs ps
      let items = Vec.fromList items'

      chan <- BCh.newBChan 10

      -- Send a tick event every 2 seconds
      void . forkIO $ forever $ do
        t <- Tm.getCurrentTime
        BCh.writeBChan chan $ EventTick t
        threadDelay C.defaultTickPeriodMicroSeconds

      dt <- Tm.getCurrentTime

      --TODO move to controller
      let st = C.AppState { C._stRoot = root
                          , C._stDetail = []
                          , C._stFocus = C.FoldersControl
                          , C._stLastGenPassState = Nothing
                          , C._stUi = BrickState { _bListDir = BL.list C.FoldersControl items 1
                                                 , _bListFile = BL.list C.FilesControl Vec.empty 1
                                                 , _bEditSearch = BE.editor C.SearchControl (Just 1) ""
                                                 }
                          , C._stMessage = Nothing
                          , C._stShowHelp = True
                          , C._stAutoCloseTtl = C.defaultAutoCloseTtl
                          , C._stTime = dt
                          , C._stSearching = False
                          , C._stDirsCache = items
                          }
              
      st' <- runStateIODsl $ C.initState st items'
      void $ B.customMain (V.mkVty V.defaultConfig) (Just chan) app st'

    Nothing ->
      putText "Pass root path not found"


app :: B.App UIState Event C.Name
app = B.App { B.appDraw = drawUI
            , B.appChooseCursor = B.showFirstCursor
            , B.appHandleEvent = handleEvent
            , B.appStartEvent = pure
            , B.appAttrMap = const theMap
            }


handleEvent :: UIState -> B.BrickEvent C.Name Event -> B.EventM C.Name (B.Next UIState)
handleEvent st ev =
  case ev of
    (B.VtyEvent ve@(V.EvKey k ms)) -> do
      let a = C.handleKeyPress st (k, ms)
      runEventDsl (handleBaseEvent ve) a

    (B.AppEvent (EventTick time)) -> do
      let a = C.handleTick st time
      runEventDsl pure a
      
    _ -> B.continue st

  where
    handleBaseEvent :: V.Event -> UIState -> B.EventM C.Name UIState
    handleBaseEvent ve st' = 
      case st ^. C.stFocus of
        C.FilesControl -> do
          r <- BL.handleListEventVi BL.handleListEvent ve $ st' ^. (C.stUi . bListFile)
          pure $ st' & (C.stUi . bListFile) .~ r

        C.FoldersControl -> do
          r <- BL.handleListEventVi BL.handleListEvent ve $ st' ^. (C.stUi . bListDir)
          pure $ st' & (C.stUi . bListDir) .~ r
  
        C.SearchControl -> do
          r <- BE.handleEditorEvent ve $ st' ^. (C.stUi . bEditSearch)
          pure $ st' & (C.stUi . bEditSearch) .~ r
  

drawUI :: UIState -> [B.Widget C.Name]
drawUI st =
  [ B.padTop (B.Pad 1) (drawListDir st <+> drawListFile st <+> drawDetail st <+> drawHelp st)
    <=>
    drawFooter st
  ] 


drawFooter :: UIState -> B.Widget C.Name
drawFooter st =
  B.padTop (B.Pad 1) $
  B.vLimit 1 $
  drawMessage (st ^. C.stMessage)


drawMessage :: Maybe C.Message -> B.Widget C.Name
drawMessage Nothing =
  let v = "hpass version: " <> C.version in
  drawMessage . Just $ C.Message v C.LevelInfo 1
drawMessage (Just (C.Message txt lvl _)) =
  let attr = case lvl of
               C.LevelError -> "messageError"
               C.LevelWarn -> "messageWarn"
               C.LevelInfo -> "messageInfo" in

  B.withAttr attr $
  B.txt txt
  
drawListDir :: UIState -> B.Widget C.Name
drawListDir st =
  B.hLimit 30 $ --TODO calc max width
  B.withBorderStyle BBS.unicodeRounded $
  BB.borderWithLabel (B.str "folders") $
  B.padAll 1 $
  BL.renderList listDrawDir ((st ^. C.stFocus) == C.FoldersControl) (st ^. (C.stUi . bListDir))

  
drawListFile :: UIState -> B.Widget C.Name
drawListFile st =
  B.hLimit 50 $ --TODO calc max width across all
  B.padTop (B.Pad 1) $
  B.padBottom (B.Pad 1) $
  B.vBox [ files, search]
  
  where
    files =
      B.withBorderStyle BBS.unicodeRounded $
      BB.borderWithLabel (B.str "passwords") $
      B.padAll 1 $
      BL.renderList listDrawFile ((st ^. C.stFocus) == C.FilesControl) (st ^. (C.stUi . bListFile))

    search = 
      B.hLimit 49 $
      B.vLimit 1 $
      B.padLeft (B.Pad 1) $
      BE.renderEditor (B.txt . Txt.unlines) (st ^. C.stSearching) (st ^. C.stUi ^. bEditSearch)
  
drawDetail :: UIState -> B.Widget C.Name
drawDetail st =
  if (not . null) $ st ^. C.stDetail
  then
    B.hLimit 80 $
    B.padTop (B.Pad 8) $
    B.padBottom (B.Pad 2) $
    B.vLimit 20 $
    B.withBorderStyle BBS.unicodeRounded $
    BB.borderWithLabel (B.str "detail") $
    B.padAll 1 $
    formatPassData $ st ^. C.stDetail
  else
    B.emptyWidget


formatPassData :: [C.DetailLine] -> B.Widget a
formatPassData ls =
  let ms = zipWith format [0,1..] ls in
  foldl' (<=>) B.emptyWidget ms

  where
    format :: Int -> C.DetailLine -> B.Widget a
    format i (C.DetailLine _ k v) =
      case i of
        x | x == 0 -> BM.markup $ ("0"    @? "detailNum") <> ": " <> ("*****" @? "detailData")
        x | x < 10 -> BM.markup $ (show x @? "detailNum") <> ": " <> keyValue k v
        _          -> BM.markup $ ("   "  @? "detailNum") <> keyValue k v

    keyValue k v =
      if Txt.null k
        then v @? "detailData"
        else (k @? "detailKey") <> ": " <> (v @? "detailData")


listDrawDir :: Bool -> Lib.PassDir -> B.Widget a
listDrawDir _ d =
  B.str . Txt.unpack $ Txt.replicate (Lib.pdDepth d) " " <> Lib.pdName d


listDrawFile :: Bool -> Lib.PassFile -> B.Widget a
listDrawFile _ f =
  B.str . Txt.unpack $ Lib.pfName f


drawHelp :: UIState -> B.Widget C.Name
drawHelp st =
  if st ^. C.stShowHelp
  then
    B.hLimit 80 $
    B.padTop (B.Pad 8) $
    B.padBottom (B.Pad 2) $
    B.withBorderStyle BBS.unicodeRounded $
    BB.borderWithLabel (B.str "help") $
    B.padAll 1 $


    header "Global" <=>
    help "?" "toggle help" <=>
    help "n" "create new password" <=>
    help "/" "search" <=>
    help "q/esc" "exit / quit search" <=>
    B.str " " <=>
    header "Folders" <=>
    help "down/up" "next/prev folder" <=>
    help "j/k" "next/prev folder" <=>
    help "tab/right/l" "go to passwords" <=>
    B.str " " <=>
    header "Passwords" <=>
    help "down/up" "next/prev password file" <=>
    help "j/k" "next/prev password file" <=>
    help "right" "show selected password info" <=>
    help "tab/left/h" "go to folders" <=>
    help "e" "edit selected password file in vim" <=>
    help "u" "update - create new password" <=>
    B.str " " <=>
    header "Selected password file" <=>
    help "0-9" "copy line to clipboard" <=>
    help "" "(cleard after 45 seconds)"
  
  else
    B.emptyWidget

  where
    header h =
      BM.markup $ h @? "helpHeader"

    help l r =
      B.txt "  " 
      <+>
      BM.markup (l @? "helpKey")
      <+>
      B.txt (Txt.replicate (15 - Txt.length l) " " <> if Txt.null l then "  " else "- ")
      <+>
      BM.markup (r @? "helpText")
  

customAttr :: BA.AttrName
customAttr = BL.listSelectedAttr <> "custom"


theMap :: BA.AttrMap
theMap = BA.attrMap V.defAttr [ (BL.listAttr               , V.white `B.on` V.blue)
                              , (BL.listSelectedAttr       , V.blue  `B.on` V.white)
                              , (BL.listSelectedFocusedAttr, V.black `B.on` V.yellow)
                              , (BE.editAttr               , V.black `B.on` V.cyan)
                              , (BE.editFocusedAttr        , V.black `B.on` V.yellow)
                              , (customAttr                , B.fg V.cyan)
                              , ("detailNum"               , B.fg V.red)
                              , ("detailData"              , B.fg V.yellow)
                              , ("detailKey"               , B.fg V.blue)
                              , ("messageError"            , B.fg V.red)
                              , ("messageWarn"             , B.fg V.brightYellow)
                              , ("messageInfo"             , B.fg V.cyan)
                              , ("helpHeader"              , V.withStyle (V.withStyle (B.fg V.green) V.bold) V.underline)
                              , ("helpKey"                 , B.fg V.green)
                              , ("helpText"                , B.fg V.green)
                              ]

------------------------
runEventDsl :: (UIState -> B.EventM C.Name UIState)
            -> C.EventAction BrickState UIState
            -> B.EventM C.Name (B.Next UIState)
runEventDsl h a =
  case a of
    (Pure st) -> B.continue st
    (Free (C.Halt st)) -> B.halt st
    (Free (C.GetSelectedDir st n)) -> runEventDsl h (n $ stateGetSelectedDir st)
    (Free (C.GetSelectedFile st n)) -> runEventDsl h (n $ stateGetSelectedFile st)

    (Free (C.RunBaseHandler st n)) -> do
      st' <- h st 
      runEventDsl h (n st')
 

    (Free (C.ClipLine _ line file n)) ->
      if True
        then do
          r <- liftIO $ do
            res <- runClip line file
            runStateIODsl $ n res

          B.continue r
        else
          B.suspendAndResume $ do
            res <- runClip line file
            runStateIODsl $ n res


    (Free (C.GetPassDetail _ file n)) ->
      if True
        then do
          res <- liftIO $ do
            r <- runPassShow file
            runStateIODsl $ n r

          B.continue res
        else
          B.suspendAndResume $ do
            r <- runPassShow file
            runStateIODsl $ n r


    (Free (C.RunEditFile _ file n)) -> 
      B.suspendAndResume $ do
        void $ Lib.shell "pass" ["edit", Lib.pfPassPath file]
        showRes <- runPassShow file
        runStateIODsl $ n showRes


    (Free (C.RunGenPassword st dir n)) -> 
      B.suspendAndResume $ do
        r <- runGenPassword st dir
        runStateIODsl $ n r


    (Free (C.RunUpdatePassword st file n)) -> 
      B.suspendAndResume $ do
        r <- CN.runCreatePassword CN.UpdateExisting (st ^. C.stLastGenPassState) file
        runStateIODsl $ n r
      

    (Free (C.LiftSt as n)) -> do
      st <- liftIO $ runStateIODsl as
      runEventDsl h $ n st
      
  where
    runGenPassword :: C.AppState ui -> Text -> IO CN.CreatePasswordResult
    runGenPassword st dir = do
      r <- CN.runCreatePassword CN.NewPassword (st ^. C.stLastGenPassState) dir
    
      if CN.rSuccess r --TODO this should be in the controller
        then do
          let path = CN.rFolder r <> "/" <> CN.rName r
          void $ Lib.runProc "pass" ["insert", "-f", "-m", path] $ Just (CN.rPassword r)
    
          if CN.rEditAfter r
            then void $ Lib.shell "pass" ["edit", path]
            else pass
    
        else
          pass
    
      pure r
    
    runClip :: Show a => a -> Lib.PassFile -> IO (Either Int ())
    runClip line file = do
      r <- Lib.runProc "pass" ["show", "--clip=" <> show line, Lib.pfPassPath file] Nothing
      pure $ case r of
               Right _ -> Right ()
               Left (e, _, _) -> Left e
------------------------


------------------------
runStateIODsl :: C.IOStateAction BrickState UIState
              -> IO UIState
runStateIODsl a =
  case a of
    (Pure st) -> pure st
    (Free (C.StGetDirs st n)) -> runStateIODsl $ n . Vec.toList $ st ^. (C.stUi . bListDir . BL.listElementsL)
    (Free (C.StLogError st e n)) -> runStateIODsl (n $ stateLogError st e)
    (Free (C.StClearFiles st n)) -> runStateIODsl (n $ stateClearFiles st)
    (Free (C.StShowFiles st fs n)) -> runStateIODsl (n $ stateShowFiles st fs)
    (Free (C.StGetSelectedDir st n)) -> runStateIODsl (n $ stateGetSelectedDir st)
    (Free (C.StGetSelectedFile st n)) -> runStateIODsl (n $ stateGetSelectedFile st)
    (Free (C.StGetSearchText st n)) -> runStateIODsl (n $ Txt.strip . Txt.unlines $ BE.getEditContents $ st ^. C.stUi ^. bEditSearch)

    (Free (C.StUseDirs st ds n)) -> do
      let items = Vec.fromList ds
      let st' = st & C.stDetail .~ []
                   & (C.stUi . bListDir) .~ BL.list C.FoldersControl items 1
                   & (C.stUi . bListFile) .~ BL.list C.FilesControl Vec.empty 1
      runStateIODsl $ n st'

    (Free (C.StReloadDirs st n)) -> do
      ps <- Lib.loadPass 0 "/" $ st ^. C.stRoot
      let ds = Lib.flattenDirs ps
      runStateIODsl $ n ds

    (Free (C.StSelectDir st dir n)) -> do
      let dirs = st ^. (C.stUi . bListDir . BL.listElementsL)

      case Vec.findIndex (\s -> Lib.pdPath dir == Lib.pdPath s) dirs of
        Nothing -> runStateIODsl $ n st
        Just i -> runStateIODsl (n $ st & (C.stUi . bListDir) %~ BL.listMoveTo i)

    (Free (C.StGetPassDetail _ file n)) -> do
      r <- runPassShow file
      runStateIODsl $ n r

    (Free (C.StEditPass _ file n)) -> do
      r <- Lib.shell "pass" ["edit", Lib.pfPassPath file]
      let r' = case r of
                 Right _ -> Nothing
                 Left e -> Just $ show e

      runStateIODsl $ n r'

    (Free (C.StUpdatePassDetail _ file pwd n)) -> do

      r <- Lib.runProc "pass" ["insert", "-m", "-f", Lib.pfPassPath file] $ Just pwd
      let r' = case r of
                 Right _ -> Nothing
                 Left (e, _, _) -> Just $ show e
      runStateIODsl $ n r'

    (Free (C.StClearSearch st n)) -> do
      let st' = st & (C.stUi . bEditSearch) .~ BE.editor C.SearchControl (Just 1) ""
      runStateIODsl $ n st'
------------------------

stateClearFiles :: UIState -> UIState
stateClearFiles st =
  st & (C.stUi . bListFile) %~ BL.listClear

stateShowFiles :: UIState -> [Lib.PassFile] -> UIState
stateShowFiles st fs =
  let items = Vec.fromList fs in
  st & (C.stUi . bListFile) .~ BL.list C.FilesControl items 1

stateLogError :: C.AppState ui0 -> Text -> C.AppState ui0
stateLogError st e = 
  st & C.stMessage .~ Just (C.Message e C.LevelError C.defaultMessageTtl)

stateGetSelectedDir :: UIState -> Maybe Lib.PassDir
stateGetSelectedDir st =
  case BL.listSelectedElement (st ^. (C.stUi . bListDir)) of
    Just (_, d) -> Just d
    Nothing -> Nothing

stateGetSelectedFile :: UIState -> Maybe Lib.PassFile
stateGetSelectedFile st =
  case BL.listSelectedElement (st ^. (C.stUi . bListFile)) of
    Just (_, f) -> Just f
    Nothing -> Nothing

runPassShow :: Lib.PassFile -> IO (Either Text Text)
runPassShow file = do
  r <- Lib.runProc "pass" ["show", Lib.pfPassPath file] Nothing

  pure $ case r of
           Right (t, _) -> Right t
           Left (_, o, err) -> Left $ o <> "\n\n" <> err
