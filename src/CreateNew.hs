{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module CreateNew where

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
import qualified Brick.Focus as BF
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Widgets.List as BL
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.Center as BC
import qualified Brick.Widgets.Border.Style as BBS
import           Control.Monad.Free (Free(..))
--import           Control.Monad.Free.Church as Fr
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as K

data Name = EditPass
          | EditFolder
          | EditName
          | EditLen
          | CboxEditAfter
          | CboxCaps
          | CboxLower
          | CboxNum
          | CboxSymbol
          | CboxRemoveAmbig
          | ButOk
          | ButCancel
          deriving (Eq, Ord, Show)
data Event = Event
           
data St = St { _stEditPassword :: BE.Editor Text Name
             , _stEditFolder :: BE.Editor Text Name
             , _stEditName :: BE.Editor Text Name
             , _stEditLength :: BE.Editor Text Name
             , _stFocus :: BF.FocusRing Name
             }

makeLenses ''St


main :: IO ()
main = do
  chan <- BCh.newBChan 10
  let g = St { _stEditPassword = BE.editor EditPass Nothing ""
             , _stEditFolder = BE.editor EditFolder Nothing ""
             , _stEditName = BE.editor EditName Nothing ""
             , _stEditLength = BE.editor EditLen (Just 4) "21"
             , _stFocus = BF.focusRing [ EditFolder
                                       , EditName
                                       , EditPass
                                       , EditLen
                                       , CboxCaps
                                       , CboxLower
                                       , CboxNum
                                       , CboxSymbol
                                       , CboxRemoveAmbig
                                       , CboxEditAfter
                                       , ButOk
                                       , ButCancel
                                       ]
             }
  void $ B.customMain (V.mkVty V.defaultConfig) (Just chan) app g



app :: B.App St Event Name
app = B.App { B.appDraw = drawUI
            , B.appChooseCursor = B.showFirstCursor
            , B.appHandleEvent = handleEvent
            , B.appStartEvent = pure
            , B.appAttrMap = const theMap
            }

handleEvent :: St -> B.BrickEvent Name Event -> B.EventM Name (B.Next St)
handleEvent g (B.VtyEvent (V.EvKey V.KEsc [])) = B.halt g
handleEvent st ev =
  case ev of
    (B.VtyEvent ek@(V.EvKey k [])) ->
      case k of
        (K.KChar '\t') ->
          B.continue $ st & stFocus %~ BF.focusNext
        _ ->
          case BF.focusGetCurrent $ st  ^. stFocus of
            Just EditFolder -> do
              r <- BE.handleEditorEvent ek (st ^. stEditFolder)
              B.continue $ st & stEditFolder .~ r
            _ -> B.continue st
    _ -> B.continue st
--handleEvent g _ = B.continue g

-- Drawing

drawUI :: St -> [B.Widget Name]
drawUI st =
  [ BC.center ( B.hLimit 83 $
    B.vLimit 20 $
    B.withBorderStyle BBS.unicodeRounded $
    BB.border $
    B.padAll 1 $
    body)
  ]

  where
    body = 
      topEditBoxesAndLables
      <=>
      (B.padLeft (B.Pad 2) $ B.padTop (B.Pad 1) $ genPasswordBlock)
      <=>
      (B.padLeft (B.Pad 2) $ B.padTop (B.Pad 1) $ editAfter)
      <=>
      (BC.center $ acceptButtons) 

    acceptButtons =
      button ButOk ("","O","k")
      <+>
      button ButCancel ("Cance","l","")

    button n (pre, hi, post) =
      let f = BF.focusGetCurrent (st ^. stFocus) == Just n in
      let a = if f then "buttonFocus" else "button" in
      B.withAttr a $
      B.withBorderStyle (if f then BBS.unicodeBold else BBS.unicodeRounded) $
      BB.border $
      if f 
        then B.txt $ pre <> hi <> post
        else title pre hi post

    genPasswordBlock =
      title' "G" "enerate Password" 
      <=>
      (B.padLeft (B.Pad 3) $ getPasswordOptions)

    getPasswordOptions =
      (title' "L" "ength:" <+> (B.padLeft (B.Pad 2) $ editor EditLen (st ^. stEditLength) 4))
      <=>
      (cbox CboxCaps $ title' "C" "apital letters")
      <=>
      (cbox CboxLower $ title' "L" "ower case letters")
      <=>
      (cbox CboxNum $ title "N" "u" "mbers")
      <=>
      (cbox CboxSymbol $ title' "S" "ymbols")
      <=>
      (cbox CboxRemoveAmbig $ title "Remove " "a" "mbiguous characters")
    
    editAfter =
      cbox CboxEditAfter $ (title' "E" "dit After")
      
    cbox n w = 
      let checked = True in
      let attr = if (BF.focusGetCurrent (st ^. stFocus) == Just n) then "cboxFocus" else "cbox" in
      (B.withAttr attr $ B.txt (if checked then "X" else "."))
      <+>
      (B.padLeft (B.Pad 1) w)
    
    topEditBoxesAndLables =
      titlesTL
      <+>
      (B.padLeft (B.Pad 2) $ controlsTR)

    controlsTR =
      editor EditFolder (st ^. stEditFolder) 90
      <=>
      editor EditName (st ^. stEditName) 90
      <=>
      editor EditPass (st ^. stEditPassword) 90

    titlesTL =
      title' "F" "older:" <=> title' "N" "ame:" <=> title' "P" "assword:"

    editor n e size =
      B.hLimit size $
      B.vLimit 1 $
      BE.renderEditor (B.txt . Txt.unlines) (BF.focusGetCurrent (st ^. stFocus) == Just n) e

    title' und post =
      title "" und post
      
    title pre und post =
      (BM.markup $ pre @? "titleText")
      <+>
      (BM.markup $ und @? "titleUnderline")
      <+>
      (BM.markup $ post @? "titleText")

customAttr :: BA.AttrName
customAttr = BL.listSelectedAttr <> "custom"

theMap :: BA.AttrMap
theMap = BA.attrMap V.defAttr [ (BL.listAttr,         V.white `B.on` V.blue)
                              , (BL.listSelectedAttr, V.blue `B.on` V.white)
                              , (BE.editAttr,         V.white `B.on` V.blue)
                              , (BE.editFocusedAttr,  V.black `B.on` V.yellow)
                              , (customAttr,          B.fg V.cyan)
                              , ("titleText",         B.fg V.green)
                              , ("titleUnderline",    V.withStyle (V.withForeColor V.defAttr V.brightGreen) V.underline)
                              , ("cbox",              V.white `B.on` V.blue)
                              , ("cboxFocus",         V.black `B.on` V.yellow)
                              , ("button",            V.defAttr)
                              , ("buttonFocus",       V.black `B.on` V.yellow)
                              ]
