{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module UI where

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

main :: IO ()
main = do
  ps <- Lib.loadPass 0 "/" "/home/andre/.password-store"
  let items = Vec.fromList $ Lib.flattenDirs ps

  chan <- BCh.newBChan 10

  let st = St { stPassAll = ps
              , stListDir = BL.list ListDir items 1
              , stListFile = BL.list ListFile Vec.empty 1
              , stFocusRing = BF.focusRing [ListDir, ListFile]
              , stDetail = []
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
             , stDetail :: [Text]
             }

app :: B.App St Event Name
app = B.App { B.appDraw = drawUI
            , B.appChooseCursor = B.showFirstCursor
            , B.appHandleEvent = handleEvent
            , B.appStartEvent = pure
            , B.appAttrMap = const theMap
            }

handleEvent :: St -> B.BrickEvent Name Event -> B.EventM Name (B.Next St)
handleEvent st ev = 
  case handleEventGlobal st ev of
    Just x ->
      x
    Nothing -> 
      case BF.focusGetCurrent (stFocusRing st) of
        Just ListFile -> handleEventFocusListFile st ev
        Just ListDir ->  handleEventFocusListDir st ev
        _ -> B.continue st


handleEventGlobal :: St -> B.BrickEvent Name Event -> Maybe (B.EventM Name (B.Next St))
handleEventGlobal st (B.VtyEvent (V.EvKey V.KEsc [])) = Just $ B.halt st
handleEventGlobal st (B.VtyEvent (V.EvKey (V.KChar 'q') [])) = Just $ B.halt st
handleEventGlobal _ _ = Nothing

handleEventFocusListFile :: St -> B.BrickEvent Name Event -> B.EventM Name (B.Next St)
handleEventFocusListFile st ev = 
  case ev of
    (B.VtyEvent e) ->
      case e of
        V.EvKey (V.KChar 'h') [] ->
          B.continue st { stFocusRing = BF.focusPrev . stFocusRing $ st 
                        , stDetail = []
                        }

        V.EvKey (V.KChar '\t') [] ->
          B.continue st { stFocusRing = BF.focusPrev . stFocusRing $ st 
                        , stDetail = []
                        }

        V.EvKey (V.KChar 'l') [] -> do
          ts <- case BL.listSelectedElement (stListFile st) of
                  Just (_, file) -> do
                    r <- liftIO $ Lib.runProc "pass" ["show", Lib.pfPassPath file]
                    case r of
                      Right (t,_) -> pure . Txt.lines $ t
                      Left (_, o, err) -> pure . Txt.lines $ o <> err

                  _ ->
                       pure []

          B.continue st { stDetail = ts }


        V.EvKey (V.KChar 'e') [] ->
          case BL.listSelectedElement (stListFile st) of
            Just (_, file) -> 
              B.suspendAndResume $ do
                void $ Lib.shell "pass" ["edit", Lib.pfPassPath file]
                r <- liftIO $ Lib.runProc "pass" ["show", Lib.pfPassPath file]

                let detail = case r of
                               Right (t,_) -> Txt.lines t
                               Left (_, o, err) -> Txt.lines $ o <> err

                pure $ st { stDetail = detail }

            _ ->
              B.continue st

        V.EvKey (V.KChar c) [] | (c `elem` ("0123456789" :: [Char])) -> do
           case readMaybe [c] :: Maybe Int of
             Just i -> 
               case take 1 . drop i $ stDetail st of
                 [""] -> B.continue st
                 [s] -> do
                   -- liftIO $ Lib.shell "pass" ["show", "--clip=" <> show i, stListFile st]
                   B.continue st
                 _ -> B.continue st
             _ ->
               B.continue st

        _ -> do
          r <- BL.handleListEventVi (const pure) e (stListFile st)
          B.continue st { stListFile = r 
                        , stDetail = []
                        }
    _ -> B.continue st


handleEventFocusListDir :: St -> B.BrickEvent Name Event -> B.EventM Name (B.Next St)
handleEventFocusListDir st ev = 
  case ev of
    (B.VtyEvent e) -> do
      st2 <- case e of
               V.EvKey (V.KChar 'l') [] ->
                 pure st { stFocusRing = BF.focusNext . stFocusRing $ st  
                         , stDetail = []
                         }

               V.EvKey (V.KChar '\t') [] ->
                 pure st { stFocusRing = BF.focusNext . stFocusRing $ st  
                         , stDetail = []
                         }

               _ -> do
                 r <- BL.handleListEventVi (const pure) e (stListDir st)
                 pure st { stListDir = r  
                         , stDetail = []
                         }

      case BL.listSelectedElement (stListDir st2) of
         Nothing ->
           B.continue st2 { stListFile = BL.listClear (stListFile st2) }
         Just (_, dir) -> do
           let items = Vec.fromList $ Lib.pdFiles dir
           B.continue st2 { stListFile = BL.list ListFile items 1 }

    _ -> B.continue st

  
drawUI :: St -> [B.Widget Name]
drawUI st =
  [ drawListDir st
    <+>
    drawListFile st
    <+>
    drawDetail st
  ] 

drawListDir :: St -> B.Widget Name
drawListDir st =
  B.hLimit 30 $ --TODO calc max width
  B.withBorderStyle BBS.unicodeRounded $
  BB.borderWithLabel (B.str "folders") $
  B.padAll 1 $
  BF.withFocusRing (stFocusRing st) (BL.renderList listDrawDir) (stListDir st)

  
drawListFile :: St -> B.Widget Name
drawListFile st =
  B.hLimit 50 $ --TODO calc max width across all
  B.padTop (B.Pad 1) $
  B.padBottom (B.Pad 1) $
  B.withBorderStyle BBS.unicodeRounded $
  BB.borderWithLabel (B.str "passwords") $
  B.padAll 1 $
  BF.withFocusRing (stFocusRing st) (BL.renderList listDrawFile) (stListFile st)

  
drawDetail :: St -> B.Widget Name
drawDetail st =
  if (not . null) $ stDetail st
  then
    B.hLimit 80 $
    B.padTop (B.Pad 8) $
    B.padBottom (B.Pad 2) $
    B.vLimit 20 $
    B.withBorderStyle BBS.unicodeRounded $
    BB.borderWithLabel (B.str "detail") $
    B.padAll 1 $
    formatPassData $ stDetail st
  else
    B.emptyWidget


listDrawDir :: Bool -> Lib.PassDir -> B.Widget a
listDrawDir _ d =
  B.str . Txt.unpack $ Txt.replicate (Lib.pdDepth d) " " <> Lib.pdName d


listDrawFile :: Bool -> Lib.PassFile -> B.Widget a
listDrawFile _ f =
  B.str . Txt.unpack $ Lib.pfName f


formatPassData :: [Text] -> B.Widget Name
formatPassData ls =
  let pwd = case take 1 ls of
              [a] -> a
              _ -> "" in
    
  let ms = zipWith (fmt pwd) [0,1..] ls in
  foldl' (<=>) B.emptyWidget ms

  where
    fmt :: Text -> Int -> Text -> B.Widget Name
    fmt pwd i s =
      case i of
        x | x == 0 -> BM.markup $ ("0"    @? "detailNum") <> ": " <> ("*****" @? "detailData")
        x | x < 10 -> BM.markup $ (show x @? "detailNum") <> ": " <> keyValue (noPwd pwd s)
        _          -> BM.markup $ ("   "  @? "detailNum") <> keyValue (noPwd pwd s)

    keyValue s =
      case Txt.breakOn ":" s of
        (v, "") -> v @? "detailData"
        (k, v) -> (k @? "detailKey") <> ": " <> (v @? "detailData")
  
    noPwd pwd s =
      if Txt.null pwd
        then s
        else Txt.replace pwd "*****" s


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
