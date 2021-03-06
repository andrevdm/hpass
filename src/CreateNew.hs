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
import qualified Data.CircularList as CLst
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

import qualified Crypto 

data CreateType = NewPassword
                | UpdateExisting
                deriving (Eq)

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


data PrevState = PrevState { pLen :: Int
                           , pUseNum :: Bool
                           , pUseCaps :: Bool
                           , pUseLower :: Bool
                           , pUseSymbol :: Bool
                           , pEditAfter :: Bool
                           , pRemoveAmbig :: Bool
                           }

data Event = Event
  
           
data St = St { _stEditPassword :: BE.Editor Text Name
             , _stEditFolder :: BE.Editor Text Name
             , _stCreateType :: CreateType
             , _stFocusable :: [Name]
             , _stEditName :: BE.Editor Text Name
             , _stEditLen :: BE.Editor Text Name
             , _stSuccess :: Bool
             , _stFocus :: BF.FocusRing Name
             , _stTexts :: Map.Map Name Text
             , _stSeed :: Crypto.Seed
             , _stDebug :: Text
             }

makeLenses ''St


data CreatePasswordResult = CreatePasswordResult { rPassword :: Text
                                                 , rSuccess :: Bool
                                                 , rEditAfter :: Bool
                                                 , rFolder :: Text
                                                 , rName :: Text
                                                 , rState :: PrevState
                                                 , rErrorMessage :: Maybe Text
                                                 }

checkboxes :: [Name]
checkboxes = [ CboxCaps
             , CboxLower
             , CboxNum
             , CboxSymbol
             , CboxRemoveAmbig
             , CboxEditAfter
             ]


runCreatePassword :: CreateType -> Maybe PrevState -> Text -> IO CreatePasswordResult
runCreatePassword ct pv folder = do
  chan <- BCh.newBChan 10
  seed' <- Crypto.genSeed
  let focusable = (if ct == NewPassword then [EditFolder, EditName] else [])
                  <> [ EditPass
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
  
  let st' = St { _stEditPassword = BE.editor EditPass (Just 1) ""
               , _stEditFolder = BE.editor EditFolder (Just 1) folder
               , _stCreateType = ct
               , _stFocusable = focusable
               , _stEditName = BE.editor EditName (Just 1) ""
               , _stEditLen = BE.editor EditLen (Just 1) $ maybe "21" (show . pLen) pv
               , _stSuccess = False
               , _stFocus = BF.focusRing focusable
               , _stDebug = ""
               , _stSeed = seed'
               , _stTexts = Map.fromList [ (CboxNum, maybe "X" (cb . pUseNum) pv)
                                         , (CboxCaps, maybe "X" (cb . pUseCaps) pv)
                                         , (CboxLower, maybe "X" (cb . pUseLower) pv)
                                         , (CboxSymbol, maybe "X" (cb . pUseSymbol) pv)
                                         , (CboxEditAfter, maybe "X" (cb . pEditAfter) pv)
                                         , (CboxRemoveAmbig, maybe "." (cb . pRemoveAmbig) pv)
                                         ]
               }

  let (pwd, seed) = Crypto.genPassword (createOptions st') seed'

  let st = st' { _stEditPassword = BE.editor EditPass (Just 1) pwd
               , _stSeed = seed
               , _stFocus = BF.focusNext (_stFocus st')
               }

  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  stResult <- B.customMain initialVty buildVty (Just chan) app st
  pure $ createResult stResult

  where
    cb b = if b then "X" else "."


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
        (K.KChar 'g', [K.MAlt]) -> B.continue $ withNewPassword st
        (K.KChar 'g', [K.MMeta]) -> B.continue $ withNewPassword st
        (K.KChar '\t', []) -> B.continue $ st & stFocus %~ BF.focusNext
        (K.KDown, []) -> B.continue $ st & stFocus %~ BF.focusNext
        (K.KUp, []) -> B.continue $ st & stFocus %~ BF.focusPrev
        (K.KBackTab, []) -> B.continue $ st & stFocus %~ BF.focusPrev
        (_, []) ->
          case BF.focusGetCurrent $ st ^. stFocus of
            Just EditFolder -> handleEdit ek stEditFolder stEditFolder False
            Just EditPass -> handleEdit ek stEditPassword stEditPassword False
            Just EditName | isValidNameKey k -> handleEdit ek stEditName stEditName False
            Just EditLen  | isValidNumericKey k -> handleEdit ek stEditLen stEditLen True

            Just ButOk -> handleButOk k
            Just ButCancel -> handlButCancel k

            Just c | c `elem` checkboxes -> handleCbox c k
  
            _ -> B.continue st

        (K.KChar c, [m]) | m `elem` [K.MMeta, K.MAlt] ->
          case Map.lookup c (focusMap st) of
            Just (n, _) -> case n of
                             x | x `elem` checkboxes -> B.continue $ toggleCbox st n
                             _ -> B.continue $ setFocus n
            Nothing -> B.continue st

        _ ->
          B.continue st -- & stDebug .~ (show k <> " | " <> show ms)

    _ -> B.continue st

  where
    setFocus n =
      st & stFocus %~ BF.focusRingModify (\r -> fromMaybe r $ CLst.rotateTo n r)
    
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
      then B.continue $ toggleCbox st n
      else B.continue st
    
    handleEdit ek get' set updatePwd = do  --TODO fix needing get and set
      r <- BE.handleEditorEvent ek (st ^. get')
      let update = if updatePwd then withNewPassword else identity
      B.continue . update $ st & set .~ r

    isValidNameKey k = 
      case k of
        K.KChar c -> c `notElem` ['\\', '*', '/', '?', '>' ,'<', ':', '"']
        _ -> True

    isValidNumericKey k = 
      case k of
        K.KChar c -> Char.isDigit c
        K.KLeft -> True
        K.KRight -> True
        K.KBackTab -> True
        K.KDel -> True
        K.KBS -> True
        _ -> False


toggleCbox :: St -> Name -> St
toggleCbox st n =
  let toggle = Map.alter (\case
                             Just "." -> Just "X"
                             _ -> Just ".")
               n in

  let update = if n /= CboxEditAfter then withNewPassword else identity in
  update $ st & stTexts %~ toggle


drawUI :: St -> [B.Widget Name]
drawUI st =
  [ BC.center ( B.hLimit 83 $
                B.vLimit (if st ^. stCreateType == NewPassword then 20 else 18) $
                B.withBorderStyle BBS.unicodeRounded $
                BB.border $
                B.padAll 1 $
                body
              )
    <=>
    B.txt (st ^. stDebug)
  ]

  where
    body = 
      topEditBoxesAndLables
      <=>
      (B.padLeft (B.Pad 2) $ B.padTop (B.Pad 1) $ genPasswordBlock)
      <=>
      (B.padLeft (B.Pad 2) $ B.padTop (B.Pad 1) $ editAfter)
      <=>
      (BC.center acceptButtons) 

    acceptButtons =
      button ButOk
      <+>
      button ButCancel

    button n =
      let f = BF.focusGetCurrent (st ^. stFocus) == Just n in
      let a = if f then "buttonFocus" else "button" in
      B.withAttr a $
      B.withBorderStyle (if f then BBS.unicodeBold else BBS.unicodeRounded) $
      BB.border $
      if f 
        then
          let (x, y, z) = getFocusKey n in
          title' False x (Txt.singleton y) z
        else
          title n

    genPasswordBlock =
      title' True "" "G" "enerate Password" 
      <=>
      (B.padLeft (B.Pad 3) $ getPasswordOptions)

    getPasswordOptions =
      (title EditLen <+> (B.padLeft (B.Pad 2) $ editor EditLen (st ^. stEditLen) 4))
      <=>
      (cbox CboxCaps)
      <=>
      (cbox CboxLower)
      <=>
      (cbox CboxNum)
      <=>
      (cbox CboxSymbol)
      <=>
      (cbox CboxRemoveAmbig)
    
    editAfter =
      cbox CboxEditAfter
      
    cbox n = 
      let checked = Map.findWithDefault "X" n $ st ^. stTexts in
      let attr = if BF.focusGetCurrent (st ^. stFocus) == Just n then "cboxFocus" else "cbox" in
        (B.withAttr attr $ B.txt checked)
        <+>
        (B.padLeft (B.Pad 1) $ title n)
    
    topEditBoxesAndLables =
      titlesTL
      <+>
      (B.padLeft (B.Pad 2) $ controlsTR)

    controlsTR =
      (if st ^. stCreateType == NewPassword then editor EditFolder (st ^. stEditFolder) 90 else B.emptyWidget)
      <=>
      (if st ^. stCreateType == NewPassword then editor EditName (st ^. stEditName) 90 else B.emptyWidget)
      <=>
      editor EditPass (st ^. stEditPassword) 90

    titlesTL =
      (if st ^. stCreateType == NewPassword then title EditFolder else B.emptyWidget)
      <=>
      (if st ^. stCreateType == NewPassword then title EditName else B.emptyWidget)
      <=>
      title EditPass

    editor n e size =
      B.hLimit size $
      B.vLimit 1 $
      BE.renderEditor (B.txt . Txt.unlines) (BF.focusGetCurrent (st ^. stFocus) == Just n) e

    title n =
      let (pre, accel', post) = getFocusKey n in
      let accel = if Txt.null pre then Txt.toUpper (Txt.singleton accel') else Txt.singleton accel' in
      title' True pre accel post
      
    title' useMarkup pre accel post =
      if useMarkup
      then
        BM.markup (pre @? "titleText")
        <+>
        BM.markup (accel @? "titleAccel")
        <+>
        BM.markup (post @? "titleText")
      else
        B.txt pre <+> B.txt (if Txt.null pre then Txt.toUpper accel else accel) <+> B.txt post
     

getFocusKey :: Name -> (Text, Char, Text)
getFocusKey n =
  case n of
    EditPass        -> ("",        'p', "assword:")
    EditFolder      -> ("",        'f', "older:")
    EditName        -> ("",        'n', "ame:")
    EditLen         -> ("Leng",    't', "h:")
    CboxEditAfter   -> ("",        'e', "dit After")
    CboxCaps        -> ("",        'c', "aps")
    CboxLower       -> ("Lo",      'w', "er")
    CboxNum         -> ("N",       'u', "m")
    CboxSymbol      -> ("",        's', "ymbols")
    CboxRemoveAmbig -> ("Remove ", 'a', "mbiguous")
    ButOk           -> ("",        'o', "k")
    ButCancel       -> ("Cance",   'l', "")


focusMap :: St -> Map.Map Char (Name, (Text, Char, Text))
focusMap st = Map.fromList $ go <$> st ^. stFocusable
  where
    go n =
      let (pre, accel, post) = getFocusKey n in
      (accel, (n, (pre, accel, post)))


withNewPassword :: St -> St
withNewPassword st =
  let (pwd, seed) = Crypto.genPassword (createOptions st) (_stSeed st) in
  st { _stEditPassword = BE.editor EditPass (Just 1) pwd
     , _stSeed = seed
     }

createResult :: St -> CreatePasswordResult
createResult st =
  CreatePasswordResult { rPassword = Txt.strip . Txt.unlines $ BE.getEditContents $ st ^. stEditPassword
                       , rSuccess = st ^. stSuccess
                       , rEditAfter = Map.findWithDefault "X" CboxEditAfter (st ^. stTexts) == "X"
                       , rFolder = Txt.strip . Txt.unlines $ BE.getEditContents $ st ^. stEditFolder
                       , rName = Txt.strip . Txt.unlines $ BE.getEditContents $ st ^. stEditName
                       , rState = createPrevState st
                       , rErrorMessage = Nothing
                       }

createOptions :: St -> Crypto.PasswordOptions
createOptions st =
  Crypto.PasswordOptins { Crypto.poUseLower = Map.findWithDefault "X" CboxLower (st ^. stTexts) == "X"
                        , Crypto.poUseCaps = Map.findWithDefault "X" CboxCaps (st ^. stTexts) == "X"
                        , Crypto.poUseNum = Map.findWithDefault "X" CboxNum (st ^. stTexts) == "X"
                        , Crypto.poUseSymbol = Map.findWithDefault "X" CboxSymbol (st ^. stTexts) == "X"
                        , Crypto.poRemoveAmbig = Map.findWithDefault "X" CboxRemoveAmbig (st ^. stTexts) == "X"
                        , Crypto.poLength = fromMaybe 21 . readMaybe . Txt.unpack . Txt.strip . Txt.unlines $ BE.getEditContents $ st ^. stEditLen
                        }


createPrevState :: St -> PrevState
createPrevState st =
 PrevState { pLen = fromMaybe 21 . readMaybe . Txt.unpack . Txt.strip . Txt.unlines $ BE.getEditContents $ st ^. stEditLen
           , pUseNum = Map.findWithDefault "X" CboxNum (st ^. stTexts) == "X"
           , pUseCaps = Map.findWithDefault "X" CboxCaps (st ^. stTexts) == "X"
           , pUseLower = Map.findWithDefault "X" CboxLower (st ^. stTexts) == "X"
           , pUseSymbol = Map.findWithDefault "X" CboxSymbol (st ^. stTexts) == "X"
           , pEditAfter = Map.findWithDefault "X" CboxEditAfter (st ^. stTexts) == "X"
           , pRemoveAmbig = Map.findWithDefault "X" CboxRemoveAmbig (st ^. stTexts) == "X"
           }


customAttr :: BA.AttrName
customAttr = BL.listSelectedAttr <> "custom"


theMap :: BA.AttrMap
theMap = BA.attrMap V.defAttr [ (BL.listAttr,         V.white `B.on` V.blue)
                              , (BL.listSelectedAttr, V.blue `B.on` V.white)
                              , (BE.editAttr,         V.white `B.on` V.blue)
                              , (BE.editFocusedAttr,  V.black `B.on` V.yellow)
                              , (customAttr,          B.fg V.cyan)
                              , ("titleText",         B.fg V.green)
                              , ("titleAccel",        V.withStyle (V.withForeColor V.defAttr V.brightGreen) V.underline)
                              , ("cbox",              V.white `B.on` V.blue)
                              , ("cboxFocus",         V.black `B.on` V.yellow)
                              , ("button",            V.defAttr)
                              , ("buttonFocus",       V.black `B.on` V.yellow)
                              ]

