{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module BrickUi where

import           Protolude
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
import qualified Graphics.Vty as V

import qualified Lib
import qualified Controller as Ctrl

data Name = ListDir
          | ListFile
          deriving (Show, Ord, Eq)
          
data Event = Event

data UIState = UIState { usState :: Ctrl.AppState
                       , usListDir :: BL.List Name Lib.PassDir
                       , usListFile :: BL.List Name Lib.PassFile
                       , usFocusRing :: BF.FocusRing Name
                       }

main :: IO ()
main = do
  ps <- Lib.loadPass 0 "/" "/home/andre/.password-store"
  let items = Vec.fromList $ Lib.flattenDirs ps

  chan <- BCh.newBChan 10

  let st = UIState { usState = Ctrl.AppState { Ctrl.stPassRoot = ps
                                             , Ctrl.stDetail = []
                                             }
                   , usListDir = BL.list ListDir items 1
                   , usListFile = BL.list ListFile Vec.empty 1
                   , usFocusRing = BF.focusRing [ListDir, ListFile]
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
handleEvent st ev = do
  case ev of
    --(B.VtyEvent (V.EvKey k ms)) -> do
      --let us = Ctrl.handleKeyPress (usState st) (k, ms) 
      --let st' = st { usState = us } 
      --B.continue st'
    --(B.AppEvent a) -> B.continue st
    _ -> B.continue st
  


drawUI :: UIState -> [B.Widget Name]
drawUI st =
  [] 

  
theMap :: BA.AttrMap
theMap = BA.attrMap V.defAttr []
