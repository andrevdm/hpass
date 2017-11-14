{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module BrickUi where

import           Protolude
import           Control.Lens ((^.), (.~))
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
import qualified Graphics.Vty as V

import qualified Lib
import qualified Controller as Ctrl

data Name = ListDir
          | ListFile
          deriving (Show, Ord, Eq)
          
data Event = Event

data BrickState = BrickState { _bListDir :: BL.List Name Lib.PassDir
                             , _bListFile :: BL.List Name Lib.PassFile
                             , _bFocusRing :: BF.FocusRing Name
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
                                                   , _bFocusRing = BF.focusRing [ListDir, ListFile]
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
handleEvent st ev = do
  case ev of
    --(B.VtyEvent (V.EvKey k ms)) -> do
    --  let st' = Ctrl.handleKeyPress baseHandler st (k, ms) 
    --   --let st' = st { usState = us } 
    --  B.continue st
    -- (B.AppEvent a) -> B.continue st
    _ -> B.continue st
  


drawUI :: UIState -> [B.Widget Name]
drawUI st =
  [] 

  
theMap :: BA.AttrMap
theMap = BA.attrMap V.defAttr []
