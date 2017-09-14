{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LiberalTypeSynonyms   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

import           Control.Arrow ((&&&), (>>>))
import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import qualified Data.Map                    as M
import           Data.Monoid
import qualified Data.Text as T

import           Control.Concurrent          (forkIO, threadDelay)
import           System.IO                   (hFlush, stdout)

import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.FloatSnap

-- configs and hooks
import           XMonad.Config.Kde
import           XMonad.Config.Xfce
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.ManageHook

import qualified XMonad.Layout.Fullscreen    as F
import           XMonad.Layout.NoBorders
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.ResizableTile
import qualified XMonad.StackSet             as W
import           XMonad.Util.Run (spawnPipe)
import           XMonad.Util.Cursor
import           XMonad.Util.EZConfig


-- import Turtle hiding (stdout, (<<))

--------------------------------------------------------------------------------
----------------------------------- Commands -----------------------------------
--------------------------------------------------------------------------------

-- | Launch XMonad
main :: IO ()
main = do
  xmonadXfce
--  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
--  xmonad . ewmh . myConfig $ defaultConfig

xmonadXfce = restartXfcePanel >> xmonad (ewmh (myConfig xfceConfig))

restartXfcePanel :: IO ()
restartXfcePanel = void $ forkIO $ do
  putStrLn "Delay starting"
  threadDelay 5000000
  putStrLn "Delay done"
  putStr "Restarting xfce4-panel ... "
  hFlush stdout
  spawn "xfce4-panel -r"
  putStrLn "[DONE]"

myConfig baseCfg = docks $ baseCfg
  { borderWidth        = 2
  , normalBorderColor  = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , terminal           = myTerminal
  , focusFollowsMouse  = True
  , clickJustFocuses   = True
  , modMask            = mod4Mask
  , keys               = myKeys
  , mouseBindings      = myMouse
  , workspaces         = myWorkspaces
  , layoutHook         = myLayout
  , logHook            = myLogHook
  , startupHook        = myStartupHook
  , handleEventHook    = myHandleEventHook
  , manageHook         = manageHook baseCfg <+> myManageHook
  }

myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"

myTerminal = "konsole"
-- xF86XK_TouchpadToggle :: KeySym
-- xF86XK_TouchpadToggle = 0x1008ffa9

{-
run :: Text -> [Text] -> Shell Text
run prog args = inproc prog args (pure "")

tshow :: Show a => a -> T.Text
tshow = T.pack . show

xsh :: Shell a -> X ()
xsh = void . xfork . sh

toggleTouchpad :: Shell ()
toggleTouchpad = do
  devId:_    <- fmap (match (has (text "id=" *> decimal)))
                . grep (has (text "Touchpad"))
                $ run "xinput" ["--list"]

  devState:_ <- fmap (match (suffix decimal))
                . grep (has (text "Device Enabled"))
                $ run "xinput" ["list-props", tshow devId]

  let toggle = if devState == 1 then "--disable" else "--enable"
  void $ run "xinput" [toggle, tshow devId]
-}

-- | Separated from myKeymap so we can do a validity check at startup
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys cfg = mkKeymap cfg (myKeymap cfg)


-- | Key bindings
--   Quick guide: C = control
--              , M = alt
--              , S = shift
--              , M4 = super
--   A complete list of key names and other information
--   is available at the bottom of this file.
myKeymap :: XConfig Layout -> [(String, X ())]
myKeymap cfg =
  [ ("M4-S-<Return>",   startTerminal)
  , ("M4-S-c",          closeFocused)
  , ("M4-<Space>",      nextLayout)
  , ("M4-S-<Space>",    resetLayout)
  , ("M4-S-<Left>",     prevWS)
  , ("M4-S-<Right>",    nextWS)
  , ("M4-n",            refresh)
  , ("M4-<Tab>",        focusDown)
  , ("M4-S-<Tab>",      focusUp)
  , ("M4-j",            focusDown)
  , ("M4-k",            focusUp)
  , ("M4-<Return>",     swapMaster)
  , ("M4-S-j",          swapDown)
  , ("M4-S-k",          swapUp)
  , ("M4-h",            shrinkMaster)
  , ("M4-l",            expandMaster)
  , ("M4-t",            retileWindow)
  , ("M4-,",            incrementMaster)
  , ("M4-.",            decrementMaster)
  , ("M4-q",            restartXMonad)
  , ("M4-S-q",          logoutCmd)
  , ("M4-w",            viewMonitor 1)
  , ("M4-e",            viewMonitor 2)
--  , ("M4-r",            viewMonitor 3)
  , ("M4-S-w",          moveToMonitor 1)
  , ("M4-S-e",          moveToMonitor 2)
--  , ("M4-S-r",          moveToMonitor 3)
  , ("M1-M4-b",         toggleStruts)
  , ("C-M4-r",          dmenuCmd)
  , ("M4-z",            dmenuCmd)
--  , ("M1-M4-z",         dmenuCmd)
  , ("M1-M4-e",         spawn "emacs")
  , ("M1-M4-f",         spawn "firefox")
  , ("M1-M4-p",         spawn "control")
  , ("M4--",            shrinkTile)
  , ("M4-=",            expandTile)
--  , ("M4-<f5>",         xsh toggleTouchpad)
  ]
  <>
  (((mappend "M4-" . show) &&& viewWS) <$> allWorkspaces)
  <>
  (((mappend "M4-S-" . show) &&& moveToWS) <$> allWorkspaces)

  where
    startTerminal   = spawn (XMonad.terminal cfg)
    dmenuCmd        = spawn "dmenu_run -fn 'Droid Sans Mono-20'"
    logoutCmd
      = spawn "xfce4-session-logout"
    restartXMonad   = spawn $ unwords
      [ "if type xmonad; then"
      , "xmonad --recompile && xmonad --restart;"
      , "else"
      , "xmessage xmonad not in PATH: \"$PATH\";"
      , "fi"
      ]

    closeFocused    = kill
    nextLayout      = sendMessage NextLayout
    resetLayout     = setLayout (XMonad.layoutHook cfg)

    focusDown       = windows W.focusDown
    focusUp         = windows W.focusUp
    focusMaster     = windows W.focusMaster
    swapMaster      = windows W.swapMaster
    swapDown        = windows W.swapDown
    swapUp          = windows W.swapUp

    shrinkMaster    = sendMessage Shrink
    expandMaster    = sendMessage Expand
    incrementMaster = sendMessage (IncMasterN 1)
    decrementMaster = sendMessage (IncMasterN (-1))
    toggleStruts    = sendMessage ToggleStruts
    shrinkTile      = sendMessage MirrorShrink
    expandTile      = sendMessage MirrorExpand
    retileWindow    = withFocused (windows . W.sink)

    doWorkspace f i = windows . f $ XMonad.workspaces cfg !! (i - 1)
    doMonitor   f i = screenWorkspace i >>= flip whenJust (windows . f)

    viewWS          = doWorkspace W.greedyView . workId
    moveToWS        = doWorkspace W.shift      . workId
    viewMonitor     = doMonitor   W.view
    moveToMonitor   = doMonitor   W.shift


-- | Mouse bindings
-- > buttons: 1 = left, 2 = middle, 3 = right, 4 = scroll down, 5 = scroll up
myMouse :: t -> M.Map (KeyMask, Button) (Window -> X ())
myMouse = const $ M.fromList
  [ ((mod4Mask, button1), floatMove)
  , ((mod4Mask, button3), resizeMove)
  ]
  where
    floatMove w = do
      focus w
      mouseMoveWindow w
      snapMagicMove (Just 50) (Just 50) w
    resizeMove w = do
      focus w
      mouseResizeWindow w
      snapMagicResize [R,D] (Just 50) (Just 50) w

data MyWorkspace
  = Messages
  | Web1
  | Web2
  | Code
  | Pdf
  | Term
  | Files
  | Free1
  | Free2
  deriving (Eq, Bounded, Enum, Ord)

workId :: MyWorkspace -> Int
workId = (+1) . fromEnum

instance Show MyWorkspace where
  show = show . workId

allWorkspaces :: [MyWorkspace]
allWorkspaces = enumFromTo minBound maxBound

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show allWorkspaces

-- FIXME: https://www.reddit.com/r/xmonad/comments/3vkrc3/does_this_layout_exist_if_not_can_anyone_suggest/

-- | My window layouts
myLayout = avoidStruts $ modifyL unmodified
  where
    unmodified = tiled ||| Mirror tiled ||| Full
    -- default tiling algorithm partitions the screen into two panes
    tiled   = ResizableTall nmaster delta ratio []
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100
    -- Functions to run on the layout
    modifyL = smartBorders . avoidStruts

-- | My event logging hook
myLogHook :: X ()
myLogHook = return ()

-- | My event handling hook
myHandleEventHook :: Event -> X All
myHandleEventHook = F.fullscreenEventHook

-- | My startup hook
myStartupHook :: X ()
myStartupHook = do
  setWMName "LG3D"
  setDefaultCursor xC_left_ptr
  return ()

--------------------------------------------------------------------------------
-- * XMobar

myXmobar :: LayoutClass l Window => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
myXmobar = statusBar "xmobar" myPP toggleStrutsKey
  where
    toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b)

myPP = xmobarPP
  { ppCurrent       = xmobarColor "#cc342b" ""
  , ppHidden        = xmobarColor "#373b41" ""
  , ppHiddenNoWindows   = xmobarColor "#c5c8c6" ""
  , ppUrgent        = xmobarColor "#198844" ""
  , ppLayout        = xmobarColor "#c5c8c6" ""
  , ppTitle     = xmobarColor "#373b41" "" . shorten 80
  , ppSep       = xmobarColor "#c5c8c6" "" "  "
  }


--------------------------------------------------------------------------------
-- * Manage Hooks

-- | The 'ManageHook' for my XMonad configuration
myManageHook :: ManageHook
myManageHook = composeAll
  [ isDialog --> doCenterFloat   -- Float dialog boxes
  , manageDocks                  -- Avoid struts (e.g.: a panel)
  , isFullscreen --> doFullFloat -- Fixes fullscreen windows
  , buildHooks [myApplicationGroups, specialWindows]
  ]

buildHooks :: [M.Map X11Query ManageHook] -> ManageHook
buildHooks = M.foldMapWithKey applyProp . mconcat

-- | This is a list of programs where XMonad's default behavior is not ideal.
specialWindows :: M.Map X11Query ManageHook
specialWindows = M.fromList
  [ (QClassName "7zFM",                                  doCenterFloat)
  , (QClassName "Arandr",                                doCenterFloat)
  , (QClassName "Avahi-discover",                        doCenterFloat)
  , (QClassName "bssh",                                  doCenterFloat)
  , (QClassName "bvnc",                                  doCenterFloat)
  , (QClassName "File-roller",                           doCenterFloat)
  , (QClassName "Gigolo",                                doCenterFloat)
  , (QClassName "Ghb",                                   doCenterFloat)
  , (QClassName "melt",                                  doCenterFloat)
  , (QClassName ".nm-connection-editor-wrapped",         doCenterFloat)
  , (QClassName "net-sf-openrocket-startup-Startup",     doCenterFloat)
  , (QClassName "net-technicpack-launcher-LauncherMain", doCenterFloat)
  , (QClassName "Ristretto",                             doCenterFloat)
  , (QClassName "Unetbootin",                            doCenterFloat)
  , (QClassName "Xfce4-about",                           doCenterFloat)
  , (QClassName "Xfce4-appfinder",                       doCenterFloat)
  , (QClassName "Xfce4-accessibility-settings",          doCenterFloat)
  , (QClassName "Xfce4-appearance-settings",             doCenterFloat)
  , (QClassName "Xfce4-display-settings",                doCenterFloat)
  , (QClassName "Xfce4-keyboard-settings",               doCenterFloat)
  , (QClassName "Xfce4-mime-settings",                   doCenterFloat)
  , (QClassName "Xfce4-mouse-settings",                  doCenterFloat)
  , (QClassName "Xfce4-notifyd-config",                  doCenterFloat)
  , (QClassName "Xfce4-session-settings",                doCenterFloat)
  , (QClassName "Xfce4-taskmanager",                     doCenterFloat)
  , (QClassName "Xfce4-settings-manager",                doCenterFloat)
  , (QClassName "Zenity",                                doCenterFloat)
  , (QClassName "Wrapper-1.0",                           doFloat)
  , (QClassName "MPlayer",                               doFloat)
  , (QClassName "Gimp",                                  doFloat)
  , (QAppName   "IcedTea-Web Control Panel",             doFloat)
  , (QAppName   "Java Control Panel",                    doFloat)
  , (QAppName   "Policy Tool",                           doFloat)
  ]

myApplicationGroups :: M.Map X11Query ManageHook
myApplicationGroups =
  foldMap (uncurry (foldMap . flip M.singleton . viewShift . show )) groups
  where
    viewShift = doF . liftM2 (.) W.greedyView W.shift

    groups =
      [ (Messages, QAppName <$>
          [ "Pidgin", "Messenger", "Telegram", "Telegram Desktop"])
      , (Web1, QClassName <$>
         [ "google-chrome", "firefox" ])
      , (Code, QClassName <$>
         [ "emacs", "konsole", "xterm", "gnome-terminal" ])
      , (Pdf, QClassName <$>
         ["evince"])
      ]

--------------------------------------------------------------------------------
----------------------------------- Utility ------------------------------------
--------------------------------------------------------------------------------

-- | An X11 query.
--   Find the data for creating an 'X11Query' with the @xprop@ command.
--   For example:
--      @WM_CLASS(STRING) = "Chromium"@ in your @xprop@ output
--   gives you
--      @FWClassName "Chromium"@ as an 'X11Query'
--
data X11Query = QTitle     String        -- ^ The X11 window title
              | QAppName   String        -- ^ The X11 application name
              | QClassName String        -- ^ The X11 class name
              | QArbitrary String String -- ^ An arbitrary X property
              deriving (Eq, Show, Read, Ord)

-- | Translate an 'X11Query' to the corresponding 'Query Bool'.
queryX11 :: X11Query -> Query Bool
queryX11 (QTitle       s) = title            =? s
queryX11 (QAppName     s) = appName          =? s
queryX11 (QClassName   s) = className        =? s
queryX11 (QArbitrary p s) = stringProperty p =? s

-- | Declare that windows that match a given 'X11Query'
--   should execute the given 'ManageHook'.
applyProp :: X11Query -> ManageHook -> Query (Endo WindowSet)
applyProp q mh = queryX11 q --> mh


-- https://www.nepherte.be/blog/step-by-step-configuration-of-xmonad.html
-- -- Define the workspace an application has to go to
-- myManageHook = composeAll . concat $
--     [
--           -- Applications that go to web
--         [ className =? b --> viewShift "web"      | b <- myClassWebShifts  ]
--          -- Applications that go to chat
--       , [ appName  =? c --> doF (W.shift "chat") | c <- myClassChatShifts ]
--     ]
--     where
--         viewShift = doF . liftM2 (.) W.greedyView W.shift
--         myClassWebShifts  = ["Firefox"]
--         myClassChatShifts = ["Pidgin" ]
