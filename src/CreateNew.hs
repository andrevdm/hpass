{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module CreateNew where

import           Protolude
import           Control.Lens ((^.), (.~), (%~))
import           Control.Lens.TH (makeLenses)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Txt
import qualified Data.Char as Char
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
             , _stEditLen :: BE.Editor Text Name
             , _stFocus :: BF.FocusRing Name
             , _stTexts :: Map.Map Name Text
             , _stSuccess :: Bool
             }

makeLenses ''St


data CreatePasswordResult = CreatePasswordResult { rPassword :: Text
                                                 , rFolder :: Text
                                                 , rName :: Text
                                                 , rLen :: Int
                                                 , rSuccess :: Bool
                                                 , rUseCaps :: Bool
                                                 , rUseLower :: Bool
                                                 , rUseNum :: Bool
                                                 , rUseSymbol :: Bool
                                                 , rRemoveAmbig :: Bool
                                                 }
                          deriving (Show)


createResult :: St -> CreatePasswordResult
createResult st =
  CreatePasswordResult { rPassword = Txt.unlines $ BE.getEditContents $ st ^. stEditPassword
                       , rFolder = Txt.unlines $ BE.getEditContents $ st ^. stEditFolder
                       , rName = Txt.unlines $ BE.getEditContents $ st ^. stEditName
                       , rLen = fromMaybe 21 . readMaybe . Txt.unpack . Txt.unlines $ BE.getEditContents $ st ^. stEditName
                       , rSuccess = st ^. stSuccess
                       , rUseCaps = Map.findWithDefault "X" CboxCaps (st ^. stTexts) == "X"
                       , rUseLower = Map.findWithDefault "X" CboxLower (st ^. stTexts) == "X"
                       , rUseNum = Map.findWithDefault "X" CboxNum (st ^. stTexts) == "X"
                       , rUseSymbol = Map.findWithDefault "X" CboxSymbol (st ^. stTexts) == "X"
                       , rRemoveAmbig = Map.findWithDefault "X" CboxRemoveAmbig (st ^. stTexts) == "X"
                       }

runCreatePassword :: IO CreatePasswordResult
runCreatePassword = do
  chan <- BCh.newBChan 10
  let st = St { _stEditPassword = BE.editor EditPass (Just 1) ""
              , _stEditFolder = BE.editor EditFolder (Just 1) ""
              , _stEditName = BE.editor EditName (Just 1) ""
              , _stEditLen = BE.editor EditLen (Just 1) "21"
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
              , _stTexts = Map.fromList [ (CboxCaps,        "X")
                                        , (CboxLower,       "X")
                                        , (CboxNum,         "X")
                                        , (CboxSymbol,      "X")
                                        , (CboxRemoveAmbig, ".")
                                        , (CboxEditAfter,   "X")
                                        ]
              , _stSuccess = False
              }
  st' <- B.customMain (V.mkVty V.defaultConfig) (Just chan) app st
  pure $ createResult st'


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
    (B.VtyEvent ek@(V.EvKey k ms)) ->
      case (k, ms) of
        (K.KChar '\t', []) -> B.continue $ st & stFocus %~ BF.focusNext
        (K.KChar '\t', [K.MShift]) -> B.continue $ st & stFocus %~ BF.focusPrev --TODO not working
        _ ->
          case BF.focusGetCurrent $ st  ^. stFocus of
            Just EditFolder -> handleEdit ek stEditFolder stEditFolder 
            Just EditName -> handleEdit ek stEditName stEditName
            Just EditPass -> handleEdit ek stEditPassword stEditPassword
            Just EditLen  | isValidNumericKey k -> handleEdit ek stEditLen stEditLen

            Just ButOk -> handleButOk k
            Just ButCancel -> handlButCancel k

            Just c | c `elem` [CboxCaps
                              , CboxLower
                              , CboxNum
                              , CboxSymbol
                              , CboxRemoveAmbig
                              , CboxEditAfter
                              ] -> handleCbox c k
  
            _ -> B.continue st
    _ -> B.continue st

  where
    handleButOk k =
      if k == K.KEnter
      then B.halt $ st & stSuccess .~ True
      else B.continue st
    
    handlButCancel k =
      if k == K.KEnter
      then B.halt $ st & stSuccess .~ False
      else B.continue st
    
    handleCbox n k =
      if (k == K.KEnter) || (k == K.KChar ' ') 
      then
        let toggle = Map.alter (\case
                                   Just "." -> Just "X"
                                   _ -> Just ".")
                     n in

        B.continue $ st & stTexts %~ toggle
      else B.continue st
    
    handleEdit ek get' set = do  --TODO fix needing get and set
      r <- BE.handleEditorEvent ek (st ^. get')
      B.continue $ st & set .~ r

    isValidNumericKey k = 
      case k of
        K.KChar c -> Char.isDigit c
        K.KLeft -> True
        K.KRight -> True
        K.KBackTab -> True
        K.KDel -> True
        K.KBS -> True
        _ -> False
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
      (title' "L" "ength:" <+> (B.padLeft (B.Pad 2) $ editor EditLen (st ^. stEditLen) 4))
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
      let checked = Map.findWithDefault "X" n $ st ^. stTexts in
      let attr = if BF.focusGetCurrent (st ^. stFocus) == Just n then "cboxFocus" else "cbox" in
      (B.withAttr attr $ B.txt checked)
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
