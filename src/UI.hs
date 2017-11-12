{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module UI where

import           Protolude
import qualified Data.Text as Txt
import qualified Data.Vector as Vec
import           Brick ((<+>))
import qualified Brick as B
import qualified Brick.BChan as BCh
import qualified Brick.AttrMap as BA
import qualified Brick.Widgets.List as BL
import qualified Brick.Widgets.Center as BC
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.Border.Style as BBS
import qualified Graphics.Vty as V

main :: IO ()
main = do
  chan <- BCh.newBChan 10
  let items = Vec.fromList ["a", "b", "ccc"]
  let g = St $ BL.list () items 1
  void $ B.customMain (V.mkVty V.defaultConfig) (Just chan) app g

type Name = ()
data Event = Event
data St = St { stList :: BL.List Name Text }

app :: B.App St Event Name
app = B.App { B.appDraw = drawUI
            , B.appChooseCursor = B.showFirstCursor
            , B.appHandleEvent = handleEvent
            , B.appStartEvent = pure
            , B.appAttrMap = const theMap
            }

handleEvent :: St -> B.BrickEvent Name Event -> B.EventM Name (B.Next St)
handleEvent g (B.VtyEvent (V.EvKey V.KEsc [])) = B.halt g
handleEvent g (B.VtyEvent e) =
  case e of
    V.EvKey (V.KChar 'k') [] -> do
      let r = BL.listMoveBy (-1) $ stList g
      B.continue $ g { stList = r }
    V.EvKey (V.KChar 'j') [] -> do
      let r = BL.listMoveBy 1 $ stList g
      B.continue $ g { stList = r }

    _ -> do
      r <- BL.handleListEvent e (stList g)
      B.continue $ g { stList = r }
handleEvent g _ = B.continue g

-- Drawing

drawUI :: St -> [B.Widget Name]
drawUI g =
  [ B.hLimit 50 $
    B.withBorderStyle BBS.unicodeRounded $
    BB.borderWithLabel (B.str "menu") $
    B.padAll 1 $
    BL.renderList listDrawElement True (stList g)
  ]

listDrawElement :: Bool -> Text -> B.Widget ()
listDrawElement _ =
  B.str . Txt.unpack


customAttr :: BA.AttrName
customAttr = BL.listSelectedAttr <> "custom"

theMap :: BA.AttrMap
theMap = BA.attrMap V.defAttr [ (BL.listAttr,         V.white `B.on` V.blue)
                              , (BL.listSelectedAttr, V.blue `B.on` V.white)
                              , (customAttr,          B.fg V.cyan)
                              ]
