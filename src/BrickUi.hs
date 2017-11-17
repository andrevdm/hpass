{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module BrickUi where

import           Protolude
import           Control.Lens ((^.), (.~), (%~))
import           Control.Lens.TH (makeLenses)
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
import           System.FilePath ((</>))
import qualified System.Directory as Dir
import qualified System.Environment as Env
import           Control.Monad.Free (Free(..))
--import           Control.Monad.Free.Church as Fr
import qualified Graphics.Vty as V

import qualified Lib
import qualified CreateNew as CN
import qualified Controller as C

data Name = ListDir
          | ListFile
          deriving (Show, Ord, Eq)
          
data Event = Event

data BrickState = BrickState { _bListDir :: BL.List Name Lib.PassDir
                             , _bListFile :: BL.List Name Lib.PassFile
                             }

makeLenses ''BrickState

type UIState = C.AppState BrickState

main :: IO ()
main = 
  getPassRoot >>= \case
    Just root -> do
      ps <- Lib.loadPass 0 "/" root
      let items = Vec.fromList $ Lib.flattenDirs ps

      chan <- BCh.newBChan 10

      let st = C.AppState { C._stPassRoot = ps
                             , C._stDetail = []
                             , C._stFocus = C.FoldersControl
                             , C._stLastGenPassState = Nothing
                             , C._stUi = BrickState { _bListDir = BL.list ListDir items 1
                                                    , _bListFile = BL.list ListFile Vec.empty 1
                                                    }
                             , C._stDebug = ""
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
      runPassDsl (handleBaseEvent ve) a
      
    _ -> B.continue st

  where
    handleBaseEvent :: V.Event -> UIState -> B.EventM Name UIState
    handleBaseEvent ve st' = 
      case st ^. C.stFocus of
        C.FilesControl -> do
          r <- BL.handleListEventVi (const pure) ve $ st' ^. (C.stUi . bListFile)
          pure $ st' & (C.stUi . bListFile) .~ r

        C.FoldersControl -> do
          r <- BL.handleListEventVi (const pure) ve $ st' ^. (C.stUi . bListDir)
          pure $ st' & (C.stUi . bListDir) .~ r
  

drawUI :: UIState -> [B.Widget Name]
drawUI st =
  [ B.padTop (B.Pad 1) (drawListDir st <+> drawListFile st <+> drawDetail st)
    <=>
    drawFooter st
  ] 

drawFooter :: UIState -> B.Widget Name
drawFooter st =
  B.padTop (B.Pad 1) $
  B.vLimit 1 $
  B.withAttr "messageBar" $
  B.txt (st ^. C.stDebug)

  
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
                              , ("messageBar"              , B.fg V.brightYellow)
                              ]

------------------------

runPassDsl :: (UIState -> B.EventM Name UIState)
           -> C.Action BrickState (C.AppState BrickState)
           -> B.EventM Name (B.Next UIState)
runPassDsl h a =
  case a of
    (Pure st) ->
      B.continue st

    (Free (C.Halt st)) ->
      B.halt st

    (Free (C.ClearFiles st n)) -> do
      let st' = st & (C.stUi . bListFile) %~ BL.listClear
      runPassDsl h (n st')
    
    (Free (C.ShowFiles st fs n)) -> do
      let items = Vec.fromList fs
      let st' = st & (C.stUi . bListFile) .~ BL.list ListFile items 1
      runPassDsl h (n st')

    (Free (C.RunBaseHandler st n)) -> do
      st' <- h st
      runPassDsl h (n st')

    (Free (C.LogError _ n)) -> 
      runPassDsl h n

    (Free (C.GetSelectedDir st n)) -> do
      let dir = case BL.listSelectedElement (st ^. (C.stUi . bListDir)) of
                  Just (_, d) -> Just d
                  Nothing -> Nothing
      runPassDsl h (n dir)

    (Free (C.GetSelectedFile st n)) -> do
      let file = case BL.listSelectedElement (st ^. (C.stUi . bListFile)) of
                   Just (_, f) -> Just f
                   Nothing -> Nothing
      runPassDsl h (n file)

    (Free (C.ClipLine line file n)) -> do
      r <- liftIO $ Lib.runProc "pass" ["show", "--clip=" <> show line, Lib.pfPassPath file] Nothing
      let txt = case r of
                  Right _ -> Right ()
                  Left (e, _, _) -> Left e
      runPassDsl h $ n txt

    (Free (C.GetPassDetail file n)) -> do
      txt <- liftIO $ runPassShow file
      runPassDsl h $ n txt

    (Free (C.ExitAndEditFile file fn)) ->
      B.suspendAndResume . liftIO $ do
        void $ Lib.shell "pass" ["edit", Lib.pfPassPath file]

        runPassShow file >>= \case
          Left e -> pure . fn . Left $ "Error getting data: " <> show e
          Right p -> pure . fn . Right $ p

    (Free (C.ExitAndGenPassword st dir fn)) ->
      B.suspendAndResume . liftIO $ do
        r <- CN.runCreatePassword (st ^. C.stLastGenPassState) dir
        if CN.rSuccess r
          then do
            let path = CN.rFolder r <> "/" <> CN.rName r
            void $ Lib.runProc "pass" ["insert", "-f", "-m", path] $ Just (CN.rPassword r)

            if CN.rEditAfter r
              then do
                void $ Lib.shell "pass" ["edit", path]
                pure . fn $ r
              else 
                pure . fn $ r

          else
            pure . fn $ r


  where
    runPassShow :: Lib.PassFile -> IO (Either Text Text)
    runPassShow file = do
      r <- Lib.runProc "pass" ["show", Lib.pfPassPath file] Nothing

      pure $ case r of
               Right (t, _) -> Right t
               Left (_, o, err) -> Left $ o <> "\n\n" <> err


getPassRoot :: IO (Maybe FilePath)
getPassRoot = 
  Env.lookupEnv "PASSWORD_STORE_DIR" >>= \case
    Just p ->
      pure $ Just p

    Nothing -> do
      root <- Dir.getHomeDirectory

      let ps = [ root </> ".password-store"
               , root </> "password-store"
               ]

      pse <- sequenceA $ (\d -> do
                                  e <- Dir.doesDirectoryExist d
                                  pure (d, e)
                         ) <$> ps

      case filter snd pse of
        ((d, True) : _) -> pure $ Just d
        _ -> pure Nothing
