-- My XMonad configuration

import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.WorkspaceCompare (getSortByXineramaRule)
import XMonad.Util.Hacks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.DynamicIcons
import XMonad.Layout.Spacing
import XMonad.Layout.CenterMainFluid
import XMonad.Layout.Renamed
import XMonad.Layout.AvoidFloats
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowGo
import XMonad.Actions.CopyWindow
import XMonad.Actions.WorkspaceNames
import XMonad.Actions.Submap
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Window

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified Data.List as L
import Data.Monoid (All (..))
import Data.Maybe (isNothing)
import Control.Monad (liftM2, forM_, when)
import System.Exit (exitSuccess)

-- Settings
myModMask :: KeyMask
myModMask = mod4Mask
myTerm :: String
myTerm = "alacritty"
myBorderWidth :: Dimension
myBorderWidth = 4
myFocusColor :: String
myFocusColor = colors !! 1
myNormalColor :: String
myNormalColor = colors !! 0
myFont :: String
myFont = "xft:RobotoMono Nerd Font:size:12:antialias=true:hinting=true"

-- Configuration
myConfig = def
  { modMask = myModMask
  , terminal = myTerm
  , focusedBorderColor = myFocusColor
  , normalBorderColor = myNormalColor
  , borderWidth = myBorderWidth
  , layoutHook = myLayouts
  , startupHook = myStartupHook
  , manageHook = myManageHook
  , handleEventHook = myHandleEventHook
  }
  `additionalKeysP` myKeys

-- | Copy the focused window to all visible workspaces.
copyToVisible :: (Eq s, Eq i, Eq a) => W.StackSet i l a s sd -> W.StackSet i l a s sd
copyToVisible s = foldr (copy . W.tag) s visibleWS
  where
    visibleWS = W.workspace (W.current s) : filter (\w -> numWindowsInWorkspace w > 0) (W.hidden s)
    numWindowsInWorkspace :: W.Workspace i l a -> Int
    numWindowsInWorkspace ws = length $ W.integrate' (W.stack ws)

-- Keybindings
myKeys :: [(String, X ())]
myKeys =
  [ ("M-p", runOrRaisePrompt =<< withHistMatch promptConfig <$> initMatches) -- prompt for running program
  , ("M-'", windowPrompt promptConfig Goto allWindows) -- prompt for selecting window
  , ("M-S-'", windowPrompt promptConfig Bring allWindows) -- prompt for bringing window
  , ("M-C-'", windowPrompt promptConfig Goto wsWindows) -- prompt for workspace windows
  , ("M-S-e", runOrRaise "emacs" (className =? "Emacs")) -- raise Emacs
  , ("M-<Backspace>", toggleWS) -- select previous workspace
  , ("M-a", sendMessage AvoidFloatToggle) -- pseudo-tile floating windows
  , ("M-m", sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts) -- full screen
  , ("M-s", windows copyToVisible) -- pin to all visible workspaces
  , ("M-C-s", killAllOtherCopies) -- remove window from all but current
  , ("M-S-s", kill1) -- remove window from current, kill if only one
  , ("<XF86MonBrightnessUp>", spawn "brightnessctl s 1%+")
  , ("<XF86MonBrightnessDown>", spawn "brightnessctl s 1%-")
  , ("<XF86AudioMute>", spawn "pamixer -t")
  , ("<XF86AudioLowerVolume>", spawn "pamixer -d 1")
  , ("<XF86AudioRaiseVolume>", spawn "pamixer -i 1")
  , ("<XF86AudioMicMute>", spawn "pamixer --source 135 -t")
  , ("M-b", sendMessage ToggleStruts) -- toggle bar
  , ("M-C-q", io exitSuccess) -- exit XMonad
  , ("M-S-q", spawn lockCommand) -- lock screen
  -- workspaces submap
  , ("M-w", submap . M.fromList $
    [ ((0, xK_r), renameWorkspace promptConfig)
    , ((0, xK_d), setCurrentWorkspaceName "")
    , ((0, xK_Left), prevWS)
    , ((0, xK_Right), nextWS)
    , ((shiftMask, xK_Left), shiftToPrev)
    , ((shiftMask, xK_Right), shiftToNext)
    , ((0, xK_e), moveTo Next emptyWS)
    , ((0, xK_l), workspaceNamePrompt promptConfig (windows . W.greedyView))
    , ((0, xK_w), workspaceNamePrompt promptConfig (windows . W.shift))])
  -- layout submap
  , ("M-y", submap . M.fromList $
    [ ((0, xK_t), sendMessage $ JumpToLayout "Tall")
    , ((0, xK_m), sendMessage $ JumpToLayout "Mirror Tall")
    , ((0, xK_f), sendMessage $ JumpToLayout "Full")
    , ((0, xK_c), sendMessage $ JumpToLayout "Center")])
  ]
  where
    lockCommand = "betterlockscreen -l dimblur --off 300"
    withHistMatch xpc hm = xpc
                           { promptKeymap = M.union (M.fromList [ ((0, xK_Up),   historyUpMatching hm)
                                                                , ((0, xK_Down), historyDownMatching hm)
                                                                ]) (promptKeymap xpc)}

-- Start XMonad
main :: IO ()
main = xmonad
  . ewmhFullscreen
  . ewmh
  . withSB mySB
  . docks
  $ myConfig
  where
    mySB = statusBarProp "xmobar" (clickablePP =<< workspaceNamesPP myXmobarPP)

-- Bar information
myXmobarPP :: PP
myXmobarPP = def
 { ppSep             = magenta " • "
 , ppTitleSanitize   = xmobarStrip
 , ppTitle           = ppWindow
 , ppCurrent         = wrap "" "" . xmobarBorder "Top" (colors !! 6) 2
 , ppHidden          = white . wrap "" ""
 , ppUrgent          = red . wrap (yellow "!") (yellow "!")
 , ppOrder           = \[ws, l, win, wins] -> [ws, l, win]
 , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "" else w) . shorten 50
    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor (colors !! 5) ""
    blue     = xmobarColor (colors !! 4) ""
    white    = xmobarColor (colors !! 7) ""
    yellow   = xmobarColor (colors !! 11) ""
    red      = xmobarColor (colors !! 9) ""
    lowWhite = xmobarColor (colors !! 15) ""

-- Colors
colors :: [String]
colors =
  [ "#3b4252"
  , "#bf616a"
  , "#a3be8c"
  , "#ebcb8b"
  , "#81a1c1"
  , "#b48ead"
  , "#88c0d0"
  , "#e5e9f0"
  , "#4c566a"
  , "#bf616a"
  , "#a3be8c"
  , "#ebcb8b"
  , "#81a1c1"
  , "#b48ead"
  , "#8fbcbb"
  , "#eceff4"
  ]

-- Layouts
myLayouts = avoidFloats $
  avoidStruts $
  mkToggle (NBFULL ?? NOBORDERS ?? EOT) $
  renamed [ CutWordsLeft 1 ] $
  smartSpacingWithEdge gaps $
  tiled ||| Mirror tiled ||| Full ||| centered
  where
    tiled = Tall nmaster delta ratio
    centered = named "Center" $ CenterMainFluid nmaster delta centerFract
    nmaster = 1
    ratio = 1/2
    delta = 3/100
    gaps = 10
    centerFract = 70/100

-- Startup
myStartupHook :: X ()
myStartupHook = do
  spawnOnce "xsetroot -cursor_name left_ptr"
  spawnOnce "feh --bg-fill --no-fehbg ~/.wallpapers/kace.jpg"
  spawnOnce "trayer --edge top --align right --widthtype request --padding 2 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint 0x1a1b26 --height 40"
  setWMName "LG3D"

-- Functions to be hooked into as window rules
-- Rename workspace for certain windows
-- Move certain applications to next empty workspace
doRenameWS :: String -> ManageHook
doRenameWS newName = do
  liftX $ do
    mname <- getCurrentWorkspaceName
    if isNothing mname
      then setCurrentWorkspaceName newName
      else return ()
  return mempty

doMoveToNextEmptyWS :: ManageHook
doMoveToNextEmptyWS = do
  liftX $ do
    nextWS <- findWorkspace getSortByXineramaRule Next emptyWS 1
    windows . W.greedyView $ nextWS
  return mempty

-- Window rules
myManageHook :: ManageHook
myManageHook = composeAll
  [ className =? "zoom" --> doFloat
  , title =? "Zoom Meeting" --> doFloat
  , title =? "Picture-in-Picture" --> doFloat
  , title ~? "Figure" --> doFloat
  , title =? "bluetooth" --> doRectFloat (W.RationalRect 0.05 0.10 0.40 0.55)
  , title =? "Ediff" --> doFloat
  , className =? "gksqt" --> doFloat
  , isDialog --> doCenterFloat
  , className =? "firefox" <||> className =? "librewolf" --> doRenameWS "web"
  , className =? "Emacs" --> doRenameWS "emacs"
  , className =? "Slack" --> doRenameWS "slack"
  , checkDock --> doLower
  ]

-- Hook to handle events
-- Swallow parent window for some applications (including zoom and programs launched from a terminal)
-- Clear workspace name when window is killed and there are no windows left
myHandleEventHook :: Event -> X All
myHandleEventHook = composeAll
  [ swallowEventHook (className =? "kitty" <||> title ~? "Zoom Workplace") (return True)
  , clearWorkspaceNameHook
  , trayerPaddingXmobarEventHook
  ]

getNumberOfWindowsInWorkpace :: X Int
getNumberOfWindowsInWorkpace = withWindowSet (pure . length . W.index)

clearWorkspaceNameHook :: Event -> X All
clearWorkspaceNameHook (DestroyWindowEvent {ev_window = w}) = do
  n <- getNumberOfWindowsInWorkpace
  when (n < 1) $ setCurrentWorkspaceName ""
  return $ All True
clearWorkspaceNameHook _ = return $ All True

-- Prompt configuration
promptConfig :: XPConfig
promptConfig = def
  { position = CenteredAt 0.2 0.5
  , font = myFont
  , bgColor = colors !! 0
  , fgColor = colors !! 12
  , fgHLight = colors !! 15
  , bgHLight = colors !! 8
  , promptBorderWidth = 0
  , height = 32
  , maxComplRows = Just 12
  , maxComplColumns = Just 1
  , alwaysHighlight = True
  , historySize = 0
  , autoComplete = Nothing
  , promptKeymap = emacsLikeXPKeymap
  , searchPredicate = fuzzyMatch
  , sorter = fuzzySort
  }
