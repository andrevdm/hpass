{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module BrickUi where

import           Protolude
import           Control.Lens ((^.), (.~), (%~))
import           Control.Lens.TH (makeLenses)
import qualified Data.Text as Txt
import qualified Data.Vector as Vec
import           Brick ((<+>), (<=>))
import qualified Brick as B
import qualified Brick.BChan as BCh
import qualified Brick.Focus as BF
import qualified Brick.Markup as BM
import           Brick.Markup ((@?))
import qualified Brick.AttrMap as BA
import qualified Brick.Widgets.List as BL
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.Border.Style as BBS
import qualified Control.Monad.Free as Fr
import           Control.Monad.Free (Free(..))
--import           Control.Monad.Free.Church as Fr
import qualified Graphics.Vty as V

import qualified Lib
import qualified Controller as Ctrl

data Name = ListDir
          | ListFile
          deriving (Show, Ord, Eq)
          
data Event = Event

data BrickState = BrickState { _bListDir :: BL.List Name Lib.PassDir
                             , _bListFile :: BL.List Name Lib.PassFile
                             }

makeLenses ''BrickState

type UIState = Ctrl.AppState BrickState

main :: IO ()
main = do
  ps <- Lib.loadPass 0 "/" "/home/andre/.password-store"
  let items = Vec.fromList $ Lib.flattenDirs ps

  chan <- BCh.newBChan 10

  let st = Ctrl.AppState { Ctrl._stPassRoot = ps
                         , Ctrl._stDetail = []
                         , Ctrl._stFocus = Ctrl.FoldersControl
                         , Ctrl._stUi = BrickState { _bListDir = BL.list ListDir items 1
                                                   , _bListFile = BL.list ListFile Vec.empty 1
                                                   }
                         }
          
  void $ B.customMain (V.mkVty V.defaultConfig) (Just chan) app st

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
      let a = Ctrl.handleKeyPress st (k, ms)
      runPassDsl st (handleBaseEvent ve) a
      
    _ -> B.continue st

  where
    handleBaseEvent :: V.Event -> UIState -> B.EventM Name UIState
    handleBaseEvent ve st' = 
      case st ^. Ctrl.stFocus of
        Ctrl.FilesControl -> do
          r <- BL.handleListEventVi (const pure) ve $ st' ^. (Ctrl.stUi . bListFile)
          pure $ st & (Ctrl.stUi . bListFile) .~ r

        Ctrl.FoldersControl -> do
          r <- BL.handleListEventVi (const pure) ve $ st' ^. (Ctrl.stUi . bListDir)
          pure $ st & (Ctrl.stUi . bListDir) .~ r
  


drawUI :: UIState -> [B.Widget Name]
drawUI st =
  [ drawListDir st
    <+>
    drawListFile st
    <+>
    drawDetail st
  ] 

drawListDir :: UIState -> B.Widget Name
drawListDir st =
  B.hLimit 30 $ --TODO calc max width
  B.withBorderStyle BBS.unicodeRounded $
  BB.borderWithLabel (B.str "folders") $
  B.padAll 1 $
  BL.renderList listDrawDir ((st ^. Ctrl.stFocus) == Ctrl.FoldersControl) (st ^. (Ctrl.stUi . bListDir))

  
drawListFile :: UIState -> B.Widget Name
drawListFile st =
  B.hLimit 50 $ --TODO calc max width across all
  B.padTop (B.Pad 1) $
  B.padBottom (B.Pad 1) $
  B.withBorderStyle BBS.unicodeRounded $
  BB.borderWithLabel (B.str "passwords") $
  B.padAll 1 $
  BL.renderList listDrawFile ((st ^. Ctrl.stFocus) == Ctrl.FilesControl) (st ^. (Ctrl.stUi . bListFile))

  
drawDetail :: UIState -> B.Widget Name
drawDetail st =
  if (not . null) $ st ^. Ctrl.stDetail
  then
    B.hLimit 80 $
    B.padTop (B.Pad 8) $
    B.padBottom (B.Pad 2) $
    B.vLimit 20 $
    B.withBorderStyle BBS.unicodeRounded $
    BB.borderWithLabel (B.str "detail") $
    B.padAll 1 $
    formatPassData $ st ^. Ctrl.stDetail
  else
    B.emptyWidget


formatPassData :: [Ctrl.DetailLine] -> B.Widget a
formatPassData ls =
  let ms = zipWith format [0,1..] ls in
  foldl' (<=>) B.emptyWidget ms

  where
    format :: Int -> Ctrl.DetailLine -> B.Widget a
    format i (Ctrl.DetailLine _ k v) =
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
                              ]

------------------------

runPassDsl :: UIState
           -> (UIState -> B.EventM Name UIState)
           -> Ctrl.Action BrickState (Ctrl.AppState BrickState)
           -> B.EventM Name (B.Next UIState)
runPassDsl st_ h a =
  case a of
    (Pure r) ->
      B.continue r

    (Free Ctrl.Halt) ->
      B.halt st_

    (Free (Ctrl.ClearFiles n)) -> do
      let st' = st_ & (Ctrl.stUi . bListFile) %~ BL.listClear
      runPassDsl st' h n
    
    (Free (Ctrl.ShowFiles fs n)) -> do
      let items = Vec.fromList fs
      let st' = st_ & (Ctrl.stUi . bListFile) .~ BL.list ListFile items 1
      runPassDsl st' h n

    (Free (Ctrl.RunBaseHandler st n)) -> do
      st' <- h st
      runPassDsl st' h (n st')

    (Free (Ctrl.LogError _ n)) -> 
      runPassDsl st_ h n

    (Free (Ctrl.GetSelectedDir n)) -> do
      let dir = case BL.listSelectedElement (st_ ^. (Ctrl.stUi . bListDir)) of
                  Just (_, d) -> Just d
                  Nothing -> Nothing
      runPassDsl st_ h (n dir)

    (Free (Ctrl.GetSelectedFile n)) -> do
      let file = case BL.listSelectedElement (st_ ^. (Ctrl.stUi . bListFile)) of
                   Just (_, f) -> Just f
                   Nothing -> Nothing
      runPassDsl st_ h (n file)

    (Free (Ctrl.GetPassDetail file n)) ->
      runPassDsl st_ h (n $ Left "TODO")

    (Free (Ctrl.EditFile file n)) ->
      runPassDsl st_ h n 

-- GetPassDetail Lib.PassFile (Either Text Text -> next)
-- EditFile Lib.PassFile next
