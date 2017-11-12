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
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.Border.Style as BBS
import qualified Graphics.Vty as V

import qualified Lib

main :: IO ()
main = do
  ps <- Lib.loadPass 0 "\\" "/home/andre/.password-store"
  let items = Vec.fromList $ Lib.flattenDirs ps

  chan <- BCh.newBChan 10
  let g = St ps (BL.list ListFolder items 1) (BL.list ListFile Vec.empty 1)
  void $ B.customMain (V.mkVty V.defaultConfig) (Just chan) app g

data Name = ListFolder
          | ListFile
          deriving (Show, Ord, Eq)
          
data Event = Event
data St = St { stPassAll :: Lib.PassDir
             , stListDir :: BL.List Name Lib.PassDir
             , stListFile :: BL.List Name Lib.PassFile
             }

app :: B.App St Event Name
app = B.App { B.appDraw = drawUI
            , B.appChooseCursor = B.showFirstCursor
            , B.appHandleEvent = handleEvent
            , B.appStartEvent = pure
            , B.appAttrMap = const theMap
            }

handleEvent :: St -> B.BrickEvent Name Event -> B.EventM Name (B.Next St)
handleEvent g (B.VtyEvent (V.EvKey V.KEsc [])) = B.halt g
handleEvent g (B.VtyEvent (V.EvKey (V.KChar 'q') [])) = B.halt g
handleEvent g (B.VtyEvent e) = do
  g' <- case e of
          V.EvKey (V.KChar 'k') [] -> do
            let r = BL.listMoveBy (-1) $ stListDir g
            pure g { stListDir = r }
          V.EvKey (V.KChar 'j') [] -> do
            let r = BL.listMoveBy 1 $ stListDir g
            pure g { stListDir = r }

          _ -> do
            r <- BL.handleListEvent e (stListDir g)
            pure g { stListDir = r }

  r <- case BL.listSelectedElement (stListDir g') of
         Nothing ->
           pure g { stListFile = BL.listClear (stListFile g') }
         Just (_, dir) -> do
           let items = Vec.fromList $ Lib.pdFiles dir
           pure g' { stListFile = BL.list ListFile items 1 }
  
  B.continue r
handleEvent g _ = B.continue g

-- Drawing

drawUI :: St -> [B.Widget Name]
drawUI g =
  [ (
      B.hLimit 50 $
      B.withBorderStyle BBS.unicodeRounded $
      BB.borderWithLabel (B.str "folders") $
      B.padAll 1 $
      BL.renderList listDrawDir True (stListDir g)
    )
    <+>
    (
      B.hLimit 90 $
      B.withBorderStyle BBS.unicodeRounded $
      BB.borderWithLabel (B.str "passwords") $
      B.padAll 1 $
      BL.renderList listDrawFile False (stListFile g)
    )
  ]

listDrawDir :: Bool -> Lib.PassDir -> B.Widget a
listDrawDir _ d =
  B.str . Txt.unpack $ Txt.replicate (Lib.pdDepth d) " " <> Lib.pdName d

listDrawFile :: Bool -> Lib.PassFile -> B.Widget a
listDrawFile _ f =
  B.str . Txt.unpack $ Lib.pfName f


customAttr :: BA.AttrName
customAttr = BL.listSelectedAttr <> "custom"

theMap :: BA.AttrMap
theMap = BA.attrMap V.defAttr [ (BL.listAttr,         V.white `B.on` V.blue)
                              , (BL.listSelectedAttr, V.blue `B.on` V.white)
                              , (customAttr,          B.fg V.cyan)
                              ]
