{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module BrickUi where

import           Prelude (error) --TODO remove
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
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.Border.Style as BBS
import           Control.Monad.Free (Free(..))
import           Control.Concurrent (threadDelay, forkIO)
--import           Control.Monad.Free.Church as Fr
import qualified Graphics.Vty as V

import qualified Lib
import qualified CreateNew as CN
import qualified Controller as C

data Name = ListDir
          | ListFile
          deriving (Show, Ord, Eq)
          
newtype Event = EventTick UTCTime

data BrickState = BrickState { _bListDir :: BL.List Name Lib.PassDir
                             , _bListFile :: BL.List Name Lib.PassFile
                             }

makeLenses ''BrickState

type UIState = C.AppState BrickState

main :: IO ()
main = 
  Lib.getPassRoot >>= \case
    Just root -> do
      ps <- Lib.loadPass 0 "/" root
      let items = Vec.fromList $ Lib.flattenDirs ps

      chan <- BCh.newBChan 10

      -- Send a tick event every 2 seconds
      void . forkIO $ forever $ do
        t <- Tm.getCurrentTime
        BCh.writeBChan chan $ EventTick t
        threadDelay 2000000

      let st = C.AppState { C._stRoot = root
                          , C._stDetail = []
                          , C._stFocus = C.FoldersControl
                          , C._stLastGenPassState = Nothing
                          , C._stUi = BrickState { _bListDir = BL.list ListDir items 1
                                                 , _bListFile = BL.list ListFile Vec.empty 1
                                                 }
                          , C._stMessage = Nothing
                          , C._stShowHelp = True
                          }
              
      void $ B.customMain (V.mkVty V.defaultConfig) (Just chan) app st

    Nothing ->
      putText "Pass root path not found"


app :: B.App UIState Event Name
app = B.App { B.appDraw = drawUI
            , B.appChooseCursor = B.showFirstCursor
            , B.appHandleEvent = handleEvent
            , B.appStartEvent = pure
            , B.appAttrMap = const theMap
            }


handleEvent :: UIState -> B.BrickEvent Name Event -> B.EventM Name (B.Next UIState)
handleEvent st ev =
  case ev of
    (B.VtyEvent ve@(V.EvKey k ms)) -> do
      let a = C.handleKeyPress st (k, ms)
      runUiDsl (handleBaseEvent ve) a

    (B.AppEvent (EventTick time)) -> do
      let a = C.handleTick st time
      runUiDsl pure a
      
    _ -> B.continue st

  where
    handleBaseEvent :: V.Event -> UIState -> B.EventM Name UIState
    handleBaseEvent ve st' = 
      case st ^. C.stFocus of
        C.FilesControl -> do
          r <- BL.handleListEventVi BL.handleListEvent ve $ st' ^. (C.stUi . bListFile)
          pure $ st' & (C.stUi . bListFile) .~ r

        C.FoldersControl -> do
          r <- BL.handleListEventVi BL.handleListEvent ve $ st' ^. (C.stUi . bListDir)
          pure $ st' & (C.stUi . bListDir) .~ r
  

drawUI :: UIState -> [B.Widget Name]
drawUI st =
  [ B.padTop (B.Pad 1) (drawListDir st <+> drawListFile st <+> drawDetail st <+> drawHelp st)
    <=>
    drawFooter st
  ] 


drawFooter :: UIState -> B.Widget Name
drawFooter st =
  B.padTop (B.Pad 1) $
  B.vLimit 1 $
  drawMessage (st ^. C.stMessage)


drawMessage :: Maybe C.Message -> B.Widget Name
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
  
drawListDir :: UIState -> B.Widget Name
drawListDir st =
  B.hLimit 30 $ --TODO calc max width
  B.withBorderStyle BBS.unicodeRounded $
  BB.borderWithLabel (B.str "folders") $
  B.padAll 1 $
  BL.renderList listDrawDir ((st ^. C.stFocus) == C.FoldersControl) (st ^. (C.stUi . bListDir))

  
drawListFile :: UIState -> B.Widget Name
drawListFile st =
  B.hLimit 50 $ --TODO calc max width across all
  B.padTop (B.Pad 1) $
  B.padBottom (B.Pad 1) $
  B.withBorderStyle BBS.unicodeRounded $
  BB.borderWithLabel (B.str "passwords") $
  B.padAll 1 $
  BL.renderList listDrawFile ((st ^. C.stFocus) == C.FilesControl) (st ^. (C.stUi . bListFile))

  
drawDetail :: UIState -> B.Widget Name
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


drawHelp :: UIState -> B.Widget Name
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
    help "q/esc" "exit" <=>
    B.str " " <=>
    header "Folders" <=>
    help "down/up" "next/prev folder" <=>
    help "j/k" "next/prev folder" <=>
    help "tab/right/l" "go to passwords" <=>
    B.str " " <=>
    header "Passwords" <=>
    help "down/up" "next/prev password file" <=>
    help "e" "edit selected password file in vim" <=>
    help "j/k" "next/prev password file" <=>
    help "right" "show selected password info" <=>
    help "tab/left/h" "go to folders" <=>
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
runUiDsl :: (UIState -> B.EventM Name UIState)
         -> C.UiAction BrickState UIState
         -> B.EventM Name (B.Next UIState)
runUiDsl h a =
  case a of
    (Pure st) -> B.continue st
    (Free (C.Halt st)) -> B.halt st
    (Free (C.LogError st e n)) -> runUiDsl h (n $ stateLogError st e)
    (Free (C.ClearFiles st n)) -> runUiDsl h (n  $ stateClearFiles st)
    (Free (C.ShowFiles st fs n)) -> runUiDsl h (n $ stateShowFiles st fs)
    (Free (C.GetSelectedDir st n)) -> runUiDsl h (n $ stateGetSelectedDir st)
    (Free (C.GetSelectedFile st n)) -> runUiDsl h (n $ stateGetSelectedFile st)

    (Free (C.RunBaseHandler st n)) -> do
      st' <- h st 
      runUiDsl h (n st')

    (Free (C.ClipLine _ line file n)) -> do
      r <- liftIO $ Lib.runProc "pass" ["show", "--clip=" <> show line, Lib.pfPassPath file] Nothing
      let res = case r of
                  Right _ -> Right ()
                  Left (e, _, _) -> Left e

      runUiDsl h $ n res

    (Free (C.GetPassDetail _ file n)) -> do
      txt <- liftIO $ runPassShow file
      runUiDsl h $ n txt
      

    (Free (C.RunEditFile _ file n)) -> do
      showRes <- liftIO $ do
        void $ Lib.shell "pass" ["edit", Lib.pfPassPath file]
        runPassShow file

      B.continue . runStateDsl $ n showRes

    (Free (C.RunGenPassword st dir n)) -> 
      B.suspendAndResume $ do
        r <- CN.runCreatePassword (st ^. C.stLastGenPassState) dir
        pure . runStateDsl $ n r
      
  where
    runPassShow :: Lib.PassFile -> IO (Either Text Text)
    runPassShow file = do
      r <- Lib.runProc "pass" ["show", Lib.pfPassPath file] Nothing

      pure $ case r of
               Right (t, _) -> Right t
               Left (_, o, err) -> Left $ o <> "\n\n" <> err
------------------------


------------------------
runStateDsl :: C.StateAction BrickState UIState
            -> UIState
runStateDsl a =
  case a of
    (Pure st) -> st
    (Free (C.StLogError st e n)) -> runStateDsl (n $ stateLogError st e)
    (Free (C.StClearFiles st n)) -> runStateDsl (n $ stateClearFiles st)
    (Free (C.StShowFiles st fs n)) -> runStateDsl (n $ stateShowFiles st fs)
    (Free (C.StGetSelectedDir st n)) -> runStateDsl (n $ stateGetSelectedDir st)
    (Free (C.StGetSelectedFile st n)) -> runStateDsl (n $ stateGetSelectedFile st)
------------------------


------------------------
stateSetDirs :: UIState -> Lib.PassDir -> UIState
stateSetDirs st dir =
  let items = Vec.fromList $ Lib.flattenDirs dir in
  st & (C.stUi . bListDir) .~ BL.list ListDir items 1

stateClearFiles :: UIState -> UIState
stateClearFiles st =
  st & (C.stUi . bListFile) %~ BL.listClear

stateShowFiles :: UIState -> [Lib.PassFile] -> UIState
stateShowFiles st fs =
  let items = Vec.fromList fs in
  st & (C.stUi . bListFile) .~ BL.list ListFile items 1

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
------------------------

