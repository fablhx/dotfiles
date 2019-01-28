{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction #-}

-- Navigation:
-- Alt+F1..F10          switch to workspace
-- Win+F1..F10          move window to workspace
-- Alt+Tab              focus next window
-- Alt+Shift+Tab        focus previous window
-- Win+Shift+Up/Down    move window up/down
-- Win+C                close window
-- Win+M                move window to master area
--
-- Navigation:
-- Ctrl+Alt+Left/Right         Switch to workspace to the left or right
-- Win+Left/Right              Move window to left or right and follow
-- Win+Shift+Left/Right        Move window to left or right
-- Alt+Win+Left/Right          Swap with workspace to left or right and follow
-- Alt+Win+Shift+Left/Right    Swap with workspace to left or right
-- Ctrl+Alt+Up/Down            Switch to next/previous screen
-- Win+Up/Down                 Move window to next/previous screen and follow
-- Win+Shift+Up/Down           Move window to next/previous screen
-- Alt+Win+Up/Down             Swap with next/previous screen and follow
-- Alt+Win+Shift+Up/Down       Swap with next/previous screen
--
-- Layout management:
-- Ctrl+Win+Alt+Left/Right    shrink/expand master area
-- Ctrl+Win+Alt+Up/Down       shrink/expand mirror-master area
-- Win+Space                  cycle layouts
--
-- Other:
-- Win+Enter      start a terminal
-- Win+I          start a browser
-- Win+E          start an editor
-- Win+F          start a file explorer
-- Win+R          open the Gnome run dialog
-- Win+Q          restart XMonad
-- Win+Shift+Q    display Gnome shutdown dialog

import XMonad
import qualified XMonad.StackSet as S
import XMonad.Actions.CycleWS
import XMonad.Actions.SwapWorkspaces
import XMonad.Config.Gnome
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Combo
import XMonad.Layout.Grid
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Util.WindowProperties
import XMonad.Util.EZConfig
import Control.Monad
import Data.Ratio
import qualified Data.Map as M

-- defaults on which we build
-- use e.g. defaultConfig or gnomeConfig
myBaseConfig = gnomeConfig

-- Mod4 is the Super / Windows key
winMask = mod4Mask
altMask = mod1Mask

-- default terminal
myTerminal = "terminator"

-- browser to launch
browserCmd :: String
browserCmd = "google-chrome"
browserCmdIncognito :: String
browserCmdIncognito = "google-chrome --incognito"

-- editor to launch
editorCmd :: String
editorCmd = "emacs"

-- file explorer to launch
fileExplorerCmd :: String
fileExplorerCmd = "nautilus --new-window"

-- display
-- replace the bright red border with a more stylish colour
myBorderWidth = 2
myNormalBorderColor = "#202030"
myFocusedBorderColor = "#A0A0D0"

-- workspaces
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]

-- layouts
basicLayout = ResizableTall nmaster delta ratio [] where
    nmaster = 1
    delta   = 3/100
    ratio   = 1/2
tallLayout = named "tall" $ reflectHoriz $ avoidStruts $ basicLayout
wideLayout = named "wide" $ avoidStruts $ Mirror basicLayout
singleLayout = named "single" $ avoidStruts $ noBorders Full

myLayoutHook = normal where
    normal = tallLayout ||| wideLayout ||| singleLayout ||| simplestFloat ||| Full

myKeys conf = M.fromList $
    [ ((m, xK_Left ), c)
        | (c, m) <- [ (prevWS               , controlMask .|. altMask)
                    , (shiftToPrev >> prevWS, winMask)
                    , (shiftToPrev          , winMask .|. shiftMask)
                    , (swapTo Prev          , altMask .|. winMask)
                    , (swapTo Prev >> nextWS, altMask .|. winMask .|. shiftMask)
                    ]
    ] ++
    [ ((m, xK_Right), c)
        | (c, m) <- [ (nextWS               , controlMask .|. altMask)
                    , (shiftToNext >> nextWS, winMask)
                    , (shiftToNext          , winMask .|. shiftMask)
                    , (swapTo Next          , altMask .|. winMask)
                    , (swapTo Next >> prevWS, altMask .|. winMask .|. shiftMask)
                    ]
    ] ++
    [ ((m, xK_Up   ), c)
        | (c, m) <- [ (prevScreen                   , controlMask .|. altMask)
                    , (shiftPrevScreen >> prevScreen, winMask)
                    , (shiftPrevScreen              , winMask .|. shiftMask)
                    , (swapPrevScreen >> nextScreen , altMask .|. winMask)
                    , (swapPrevScreen               , altMask .|. winMask .|. shiftMask)
                    ]
    ] ++
    [ ((m, xK_Down ), c)
        | (c, m) <- [ (nextScreen                   , controlMask .|. altMask)
                    , (shiftNextScreen >> nextScreen, winMask)
                    , (shiftNextScreen              , winMask .|. shiftMask)
                    , (swapNextScreen >> prevScreen , altMask .|. winMask)
                    , (swapNextScreen               , altMask .|. winMask .|. shiftMask)
                    ]
    ] ++
    -- Navigation
    [ ((altMask, k), windows $ S.view i)
        | (i, k) <- zip myWorkspaces workspaceKeys
    ] ++
    [ ((winMask, k), (windows $ S.shift i) >> (windows $ S.view i))
        | (i, k) <- zip myWorkspaces workspaceKeys
    ] ++
    [ ((altMask              , xK_Tab), windows S.focusDown)
    , ((altMask .|. shiftMask, xK_Tab), windows S.focusUp)
    ] ++
    [ ((winMask .|. shiftMask, xK_Down   ), windows S.swapDown)
    , ((winMask .|. shiftMask, xK_Up     ), windows S.swapUp)
    , ((winMask              , xK_c      ), kill)
    , ((winMask              , xK_m      ), windows S.swapMaster)
    ] ++
    -- Layout management
    [ ((controlMask .|. winMask .|. altMask, xK_Left ), sendMessage Expand)
    , ((controlMask .|. winMask .|. altMask, xK_Right), sendMessage Shrink)
    , ((controlMask .|. winMask .|. altMask, xK_Up   ), sendMessage MirrorExpand)
    , ((controlMask .|. winMask .|. altMask, xK_Down ), sendMessage MirrorShrink)
    , ((controlMask .|. winMask .|. altMask, xK_space), sendMessage NextLayout)
    ] ++
    -- Other
    [ ((winMask              , xK_Return), spawn $ XMonad.terminal conf)
    , ((winMask              , xK_i     ), spawn browserCmd)
    , ((winMask .|. shiftMask, xK_i     ), spawn browserCmdIncognito)
    , ((winMask              , xK_e     ), spawn editorCmd)
    , ((winMask              , xK_f     ), spawn fileExplorerCmd)
    , ((winMask              , xK_r     ), gnomeRun)
    , ((winMask .|. shiftMask, xK_q     ), spawn "gnome-session-quit --power-off")
    , ((winMask              , xK_q     ), broadcastMessage ReleaseResources >> restart "xmonad" True)
    ]
    where workspaceKeys = [xK_F1 .. xK_F10]

-- mouse bindings that mimic Gnome's
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((altMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((altMask, button2), (\w -> focus w >> mouseResizeWindow w))
    , ((altMask, button3), (\w -> focus w >> (withFocused $ windows . S.sink)))
    , ((altMask, button4), (const $ windows S.swapUp))
    , ((altMask, button5), (const $ windows S.swapDown))
    ]

-- put it all together
main = xmonad $ myBaseConfig
    { modMask = winMask
    , terminal = myTerminal
    , workspaces = myWorkspaces
    , layoutHook = myLayoutHook
    , borderWidth = myBorderWidth
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , keys = myKeys
    , mouseBindings = myMouseBindings
    }
