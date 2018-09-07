{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction #-}

-- Navigation:
-- Alt+F1..F10          switch to workspace
-- Ctrl+Alt+Left/Right  switch to previous/next workspace
-- Alt+Tab              focus next window
-- Alt+Shift+Tab        focus previous window
--
-- Window management:
-- Win+F1..F10          move window to workspace
-- Win+Up/Down          move window up/down
-- Win+C                close window
-- Alt+ScrollUp/Down    move focused window up/down
-- Win+M                move window to master area
-- Win+N                refresh the current window
-- Alt+LMB              move floating window
-- Alt+MMB              resize floating window
-- Alt+RMB              unfloat floating window
-- Win+T                unfloat floating window
--
-- Layout management:
-- Win+Left/Right       shrink/expand master area
-- Win+W/V              move more/less windows into master area
-- Win+Space            cycle layouts
--
-- Other:
-- Win+Enter            start a terminal
-- Win+I                start a browser
-- Win+E                start an editor
-- Win+R                open the Gnome run dialog
-- Win+Q                restart XMonad
-- Win+Shift+Q          display Gnome shutdown dialog

import XMonad
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as S
import XMonad.Actions.CycleWS
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
import Control.Monad
import Data.Ratio
import qualified Data.Map as M

-- defaults on which we build
-- use e.g. defaultConfig or gnomeConfig
myBaseConfig = gnomeConfig

-- Mod4 is the Super / Windows key
myModMask = mod4Mask
altMask = mod1Mask

-- default terminal
myTerminal = "terminator"

-- browser to launch
browserCmd :: String
browserCmd = "google-chrome"

-- editor to launch
editorCmd :: String
editorCmd = "emacs"

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

-- better keybindings for dvorak
myKeys conf = M.fromList $
    [ ((myModMask              , xK_Return), spawn $ XMonad.terminal conf)
    , ((myModMask              , xK_r     ), gnomeRun)
    , ((myModMask              , xK_c     ), kill)
    , ((myModMask              , xK_space ), sendMessage NextLayout)
    , ((myModMask              , xK_n     ), refresh)
    , ((myModMask              , xK_m     ), windows S.swapMaster)
    , ((altMask                , xK_Tab   ), windows S.focusDown)
    , ((altMask .|. shiftMask  , xK_Tab   ), windows S.focusUp)
    , ((myModMask .|. shiftMask, xK_Down  ), windows S.swapDown)
    , ((myModMask .|. shiftMask, xK_Up    ), windows S.swapUp)
    , ((myModMask              , xK_Left  ), sendMessage Expand)
    , ((myModMask              , xK_Right ), sendMessage Shrink)
    , ((myModMask              , xK_Up    ), sendMessage MirrorExpand)
    , ((myModMask              , xK_Down  ), sendMessage MirrorShrink)
    , ((myModMask              , xK_t     ), withFocused $ windows . S.sink)
    , ((myModMask              , xK_w     ), sendMessage (IncMasterN 1))
    , ((myModMask              , xK_v     ), sendMessage (IncMasterN (-1)))
    , ((myModMask              , xK_q     ), broadcastMessage ReleaseResources >> restart "xmonad" True)
    , ((myModMask              , xK_i), spawn browserCmd)
    , ((myModMask              , xK_e), spawn editorCmd)
    , ((altMask .|. controlMask, xK_Left  ), prevWS)
    , ((altMask .|. controlMask, xK_Right ), nextWS)
    , ((myModMask .|. shiftMask, xK_q     ), spawn "gnome-session-quit --power-off")
    ] ++
    -- Alt+F1..F10 switches to workspace
    -- (Alt is in a nicer location for the thumb than the Windows key,
    -- and 1..9 keys are already in use by Firefox, irssi, ...)
    [ ((altMask, k), windows $ S.greedyView i)
        | (i, k) <- zip myWorkspaces workspaceKeys
    ] ++
    -- mod+F1..F10 moves window to workspace and switches to that workspace
    [ ((myModMask, k), (windows $ S.shift i) >> (windows $ S.greedyView i))
        | (i, k) <- zip myWorkspaces workspaceKeys
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
    { modMask = myModMask
    , terminal = myTerminal
    , workspaces = myWorkspaces
    , layoutHook = myLayoutHook
    , borderWidth = myBorderWidth
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , keys = myKeys
    , mouseBindings = myMouseBindings
    }
