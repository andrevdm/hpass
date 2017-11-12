{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module UI where

import           Protolude
import qualified Data.Text as Txt
import qualified Data.Vector as Vec
import           Brick ((<+>))
import qualified Brick as B
import qualified Brick.BChan as BCh
import qualified Brick.Focus as BF
import qualified Brick.AttrMap as BA
import qualified Brick.Widgets.List as BL
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.Border.Style as BBS
import qualified Graphics.Vty as V

import qualified Lib

main :: IO ()
main = do
  ps <- Lib.loadPass 0 "\\" "/home/andre/.password-store"
  let items = Vec.fromList $ Lib.flattenDirs ps

  chan <- BCh.newBChan 10

  let st = St { stPassAll = ps
              , stListDir = BL.list ListDir items 1
              , stListFile = BL.list ListFile Vec.empty 1
              , stFocusRing = BF.focusRing [ListDir, ListFile]
              }
          
  void $ B.customMain (V.mkVty V.defaultConfig) (Just chan) app st

data Name = ListDir
          | ListFile
          deriving (Show, Ord, Eq)
          
data Event = Event
data St = St { stPassAll :: Lib.PassDir
             , stListDir :: BL.List Name Lib.PassDir
             , stListFile :: BL.List Name Lib.PassFile
             , stFocusRing :: BF.FocusRing Name
             }

app :: B.App St Event Name
app = B.App { B.appDraw = drawUI
            , B.appChooseCursor = B.showFirstCursor
            , B.appHandleEvent = handleEvent
            , B.appStartEvent = pure
            , B.appAttrMap = const theMap
            }

handleEvent :: St -> B.BrickEvent Name Event -> B.EventM Name (B.Next St)
handleEvent st (B.VtyEvent (V.EvKey V.KEsc [])) = B.halt st
handleEvent st (B.VtyEvent (V.EvKey (V.KChar 'q') [])) = B.halt st
handleEvent st (B.VtyEvent e) = do
  st' <- case BF.focusGetCurrent (stFocusRing st) of
           Just ListFile ->
             case e of
                    V.EvKey (V.KChar 'k') [] -> do
                      let r = BL.listMoveBy (-1) $ stListFile st
                      pure st { stListFile = r }

                    V.EvKey (V.KChar 'j') [] -> do
                      let r = BL.listMoveBy 1 $ stListFile st
                      pure st { stListFile = r }

                    V.EvKey (V.KChar 'h') [] ->
                      pure st { stFocusRing = BF.focusPrev . stFocusRing $ st }

                    V.EvKey (V.KChar '\t') [] ->
                      pure st { stFocusRing = BF.focusPrev . stFocusRing $ st }

                    _ -> do
                      r <- BL.handleListEvent e (stListFile st)
                      pure st { stListFile = r }

           Just ListDir -> do
             st2 <- case e of
                      V.EvKey (V.KChar 'k') [] -> do
                        let r = BL.listMoveBy (-1) $ stListDir st
                        pure st { stListDir = r }

                      V.EvKey (V.KChar 'j') [] -> do
                        let r = BL.listMoveBy 1 $ stListDir st
                        pure st { stListDir = r }

                      V.EvKey (V.KChar 'l') [] ->
                        pure st { stFocusRing = BF.focusNext . stFocusRing $ st }

                      V.EvKey (V.KChar '\t') [] ->
                        pure st { stFocusRing = BF.focusNext . stFocusRing $ st }

                      _ -> do
                        r <- BL.handleListEvent e (stListDir st)
                        pure st { stListDir = r }

             case BL.listSelectedElement (stListDir st2) of
                Nothing ->
                  pure st2 { stListFile = BL.listClear (stListFile st2) }
                Just (_, dir) -> do
                  let items = Vec.fromList $ Lib.pdFiles dir
                  pure st2 { stListFile = BL.list ListFile items 1 }

           _ ->
             pure st
  
  B.continue st'
handleEvent st _ = B.continue st

-- Drawing

drawUI :: St -> [B.Widget Name]
drawUI st =
  [ (
      B.hLimit 50 $
      B.withBorderStyle BBS.unicodeRounded $
      BB.borderWithLabel (B.str "folders") $
      B.padAll 1 $
      BF.withFocusRing (stFocusRing st) (BL.renderList listDrawDir) (stListDir st)
    )
    <+>
    (
      B.hLimit 90 $
      B.withBorderStyle BBS.unicodeRounded $
      BB.borderWithLabel (B.str "passwords") $
      B.padAll 1 $
      BF.withFocusRing (stFocusRing st) (BL.renderList listDrawFile) (stListFile st))] 
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
                              ]
