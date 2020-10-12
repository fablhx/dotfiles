{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction #-}

import XMonad

import XMonad.Actions.CycleWS
import XMonad.Actions.SwapWorkspaces

import XMonad.Config.Gnome

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Layout.IndependentScreens
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat

import XMonad.Util.WorkspaceCompare

import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- Default config
myBaseConfig = gnomeConfig

-- Mod key. Mod4 is the Super / Windows key
winMask = mod4Mask
altMask = mod1Mask

-- Default terminal
myTerminal = "terminator"

-- Browser to launch
browserCmd :: String
browserCmd = "google-chrome"
browserCmdIncognito :: String
browserCmdIncognito = "google-chrome --incognito"

-- Editor to launch
editorCmd :: String
editorCmd = "emacs"

-- File explorer to launch
fileExplorerCmd :: String
fileExplorerCmd = "nautilus --new-window"

-- Screenshot
screenshotCmd :: String
screenshotCmd = "gnome-screenshot -i"

-- Display
myBorderWidth = 2
myNormalBorderColor = "#202030"
myFocusedBorderColor = "#A0A0D0"

-- Workspaces
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7"]

-- Hooks
myManageHook =
  (composeAll . concat $
    [ [ manageHook myBaseConfig ]
    , [ className =? c --> doShift "7" | c  <- ["Slack"] ]
    ])

-- Layouts
basicLayout = ResizableTall nmaster delta ratio [] where
    nmaster = 1
    delta   = 3/100
    ratio   = 1/2
tallLayout = named "tall" $ reflectHoriz $ avoidStruts $ basicLayout
wideLayout = named "wide" $ avoidStruts $ Mirror basicLayout
singleLayout = named "single" $ avoidStruts $ noBorders Full

myLayoutHook = normal where
    normal = tallLayout ||| wideLayout ||| singleLayout ||| simplestFloat ||| Full

-- Key bindings
myKeys conf = M.fromList $
    -- Navigation
    [ ((m, xK_Left ), c)
        | (c, m) <- [ (prevWS               , controlMask .|. altMask)
                    , (shiftToPrev >> prevWS, winMask)
                    , (shiftToPrev          , winMask .|. shiftMask)
                    , (swapTo Prev          , altMask .|. winMask)
                    , (swapTo Prev >> nextWS, altMask .|. winMask .|. shiftMask)
                    ]
    ]
    ++
    [ ((m, xK_Right), c)
        | (c, m) <- [ (nextWS               , controlMask .|. altMask)
                    , (shiftToNext >> nextWS, winMask)
                    , (shiftToNext          , winMask .|. shiftMask)
                    , (swapTo Next          , altMask .|. winMask)
                    , (swapTo Next >> prevWS, altMask .|. winMask .|. shiftMask)
                    ]
    ]
    ++
    [ ((m, xK_Up   ), c)
        | (c, m) <- [ (prevScreen                   , controlMask .|. altMask)
                    , (shiftPrevScreen >> prevScreen, winMask)
                    , (shiftPrevScreen              , winMask .|. shiftMask)
                    , (swapPrevScreen >> nextScreen , altMask .|. winMask)
                    , (swapPrevScreen               , altMask .|. winMask .|. shiftMask)
                    ]
    ]
    ++
    [ ((m, xK_Down ), c)
        | (c, m) <- [ (nextScreen                   , controlMask .|. altMask)
                    , (shiftNextScreen >> nextScreen, winMask)
                    , (shiftNextScreen              , winMask .|. shiftMask)
                    , (swapNextScreen >> prevScreen , altMask .|. winMask)
                    , (swapNextScreen               , altMask .|. winMask .|. shiftMask)
                    ]
    ]
    ++
    [ ((altMask, k), windows $ W.view i)
        | (i, k) <- zip myWorkspaces workspaceKeys
    ]
    ++
    [ ((winMask, k), (windows $ W.shift i) >> (windows $ W.view i))
        | (i, k) <- zip myWorkspaces workspaceKeys
    ]
    ++
    [ ((altMask              , xK_Tab), windows W.focusUp)
    , ((altMask .|. shiftMask, xK_Tab), windows W.focusDown)
    ]
    ++
    [ ((altMask .|. shiftMask, xK_Down   ), windows W.swapDown)
    , ((altMask .|. shiftMask, xK_Up     ), windows W.swapUp)
    ]
    ++
    -- Layout management
    [ ((controlMask .|. winMask .|. altMask, xK_Left ), sendMessage Expand)
    , ((controlMask .|. winMask .|. altMask, xK_Right), sendMessage Shrink)
    , ((controlMask .|. winMask .|. altMask, xK_Up   ), sendMessage MirrorExpand)
    , ((controlMask .|. winMask .|. altMask, xK_Down ), sendMessage MirrorShrink)
    , ((controlMask .|. winMask .|. altMask, xK_space), sendMessage NextLayout)
    , ((winMask                            , xK_comma ), sendMessage (IncMasterN 1))
    , ((winMask                            , xK_period), sendMessage (IncMasterN (-1)))
    ]
    ++
    -- Other
    [ ((winMask              , xK_e     ), spawn editorCmd)
    , ((winMask              , xK_Return), spawn $ XMonad.terminal conf)
    , ((winMask              , xK_f     ), spawn fileExplorerCmd)
    , ((winMask              , xK_i     ), spawn browserCmd)
    , ((0                    , xK_Print ), spawn screenshotCmd)
    , ((winMask              , xK_h     ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    , ((winMask              , xK_m     ), windows W.swapMaster)
    , ((winMask              , xK_q     ), kill)
    , ((winMask              , xK_r     ), gnomeRun)
    , ((winMask .|. shiftMask, xK_i     ), spawn browserCmdIncognito)
    , ((winMask .|. shiftMask, xK_q     ), spawn "gnome-session-quit --power-off")
    , ((winMask              , xK_x     ), broadcastMessage ReleaseResources >> restart "xmonad" True)
    ]
    where workspaceKeys = [xK_F1 .. xK_F10]

-- Mouse bindings that mimic Gnome's
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((altMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((altMask, button2), (\w -> focus w >> mouseResizeWindow w))
    , ((altMask, button3), (\w -> focus w >> (withFocused $ windows . W.sink)))
    , ((altMask, button4), (const $ windows W.swapUp))
    , ((altMask, button5), (const $ windows W.swapDown))
    ]

-- Command to launch the bar.
myBar = "xmobar"

-- Custom stdin pretty-printer for xmobar.
myPP = xmobarPP
  { ppLayout = const ""
  , ppSort = getSortByIndex
  , ppTitle = const ""
  , ppTitleSanitize = const ""
  , ppVisible = wrap "(" ")"
  }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Main configuration
myConfig =
  myBaseConfig
    { modMask = winMask
    , terminal = myTerminal
    , workspaces = myWorkspaces
    , manageHook = myManageHook
    , layoutHook = myLayoutHook
    , borderWidth = myBorderWidth
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , keys = myKeys
    , mouseBindings = myMouseBindings
    }

-- main
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- help
help :: String
help = unlines
  [ "-- Navigation:"
  , ""
  , "-- Ctrl+Alt+Left/Right         Switch to workspace to the left/right"
  , "-- Ctrl+Alt+Up/Down            Switch to next/previous screen"
  , ""
  , "-- Win+Left/Right              Move window to left/right and follow"
  , "-- Win+Up/Down                 Move window to next/previous screen and follow"
  , ""
  , "-- Win+Shift+Left/Right        Move window to left/right"
  , "-- Win+Shift+Up/Down           Move window to next/previous screen"
  , ""
  , "-- Alt+Win+Left/Right          Swap with workspace to left/right and follow"
  , "-- Alt+Win+Up/Down             Swap with next/previous screen and follow"
  , ""
  , "-- Alt+Win+Shift+Left/Right    Swap with workspace to left/right"
  , "-- Alt+Win+Shift+Up/Down       Swap with next/previous screen"
  , ""
  , "-- Alt+F1..F10                 Switch to workspace N"
  , "-- Win+F1..F10                 Move window to workspace N"
  , ""
  , "-- Alt+Tab                     Focus next window"
  , "-- Alt+Shift+Tab               Focus previous window"
  , "-- Alt+Shift+Up/Down           Move window up/down"
  , ""
  , "-- Layout management:"
  , ""
  , "-- Ctrl+Win+Alt+Left/Right     Shrink/expand master area"
  , "-- Ctrl+Win+Alt+Up/Down        Shrink/expand mirror-master area"
  , "-- Ctrl+Win+Alt+Space          Cycle layouts"
  , "-- Win+,                       Increase number windows master"
  , "-- Win+.                       Decrease number windows master"
  , ""
  , "-- Other:"
  , ""
  , "-- Win+E                       Start an editor"
  , "-- Win+Enter                   Start a terminal"
  , "-- Win+F                       Start a file explorer"
  , "-- Win+H                       Show this help"
  , "-- Win+I                       Start a browser"
  , "-- Win+M                       Move window to master area"
  , "-- Win+Q                       Close window"
  , "-- Win+R                       Open the Gnome run dialog"
  , "-- Win+Shift+I                 Start a browser incognito"
  , "-- Win+Shift+Q                 Display Gnome shutdown dialog"
  , "-- Win+X                       Restart XMonad"
  ]
