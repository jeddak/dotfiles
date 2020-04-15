import XMonad

import XMonad.Actions.CycleWS (nextWS, prevWS, toggleWS)
import XMonad.Actions.WindowGo (raiseMaybe)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, xmobarPP, ppOutput, ppCurrent, ppVisible, ppHiddenNoWindows, ppUrgent, ppTitle, ppLayout, xmobarColor, wrap, shorten)
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks)
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook (withUrgencyHook, NoUrgencyHook(..))
import XMonad.Layout.Accordion
import XMonad.Layout.Circle
import XMonad.Layout.Dwindle
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.GridVariants
import XMonad.Layout.IM (withIM, Property(..))
import XMonad.Layout.Named (named)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.Renamed as R
import XMonad.Layout.Roledex
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed (tabbedBottom, defaultTheme, activeColor, inactiveColor, urgentColor, activeTextColor, inactiveTextColor, urgentTextColor, activeBorderColor, inactiveBorderColor, urgentBorderColor, shrinkText)
import XMonad.Layout.ThreeColumns (ThreeCol(..))
import XMonad.Layout.ToggleLayouts (toggleLayouts, ToggleLayout(..))
import XMonad.Layout.ZoomRow
import XMonad.StackSet (sink)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run (spawnPipe, safeSpawn, runInTerm)
import qualified XMonad.StackSet as W   -- manageHook rules
import qualified XMonad.Util.CustomKeys as C


import Data.Char (toUpper)
import Data.Map (union, fromList)
import System.IO (hPutStrLn)


-- also requires dmenu (suckless-tools) to be installed
-- for mod-p to display a launch prompt


myTheme = defaultTheme
  { activeColor         = "#1a1a1a"
  , inactiveColor       = "#000000"
  , urgentColor         = "#1a1a1a"
  , activeTextColor     = "#00ffff"
  , inactiveTextColor   = "#ffbe33"
  , urgentTextColor     = "#ff00ff"
  , activeBorderColor   = "#000000"
  , inactiveBorderColor = "#1a1a1a"
  , urgentBorderColor   = "#000000"
  }

-- apps that we'd prefer to not be tiled
myManageHook = composeAll
    [
      className =? "Gimp"      --> doFloat,
      -- className =? "keepass2"  --> doFloat,
      className =? "Vncviewer" --> doFloat,
      className =? "x11vnc"    --> doFloat,
      className =? "vinagre"    --> doFloat
    ]

myStartupHook = do
  spawn ("$HOME/.xmonad/scripts/autostart.sh")



myLayoutHook =
  avoidStruts $ smartBorders $ toggleLayouts tabbedLayout
  (tiled ||| Mirror tiled ||| threeCol ||| tabbedLayout ||| gimpLayout)    -- fold ||| spiral (6/7) 
  -- try this sometime:
  -- spiral (toRational (2 / (1 + sqrt 5::Double))) 
  where
    tiled    = Tall nmaster delta ratio
    threeCol = ThreeColMid nmaster delta ratio
    nmaster  = 1
    delta    = 3/100
    ratio    = 1/2
    fold = renamed [R.Replace "Fold"] $ Accordion

    gimpLayout = named "Gimp" (
        withIM (0.15) (Role "gimp-toolbox") $
        reflectHoriz $
        withIM (0.2) (Role "gimp-dock") Full
      )

    tabbedLayout = named "Tabbed" (tabbedBottom shrinkText myTheme)



main :: IO ()
main = do
    xmproc <- spawnPipe "/home/jdonald/.local/bin/xmobar /home/jdonald/.xmobarrc"

{-
https://www.reddit.com/r/xmonad/comments/9vg646/xmobar_with_multiple_monitors/

JMR03
3 points ·
1 year ago
· edited 1 year ago

For multiple monitors I use XMonad.Hooks.DynamicBars. The way this package handles multiple monitors is to spawn a separate bar for each window with something like

spawnPipe $ "xmobar -x " ++ show sid

for each screen id (0,1..).The -x tells xmobar which screen to display the bar on.


-}


    
    xmonad $ defaultConfig
        {
          borderWidth = 1,
          normalBorderColor = "#333745", 
          -- focusedBorderColor = "#ff33cc",   --pink
          focusedBorderColor = "#00ff00", -- lime
          -- focusedBorderColor = "#ff8000", -- orange
          -- focusedBorderColor = ""#66ccff", -- blue
          -- focusedBorderColor = "#cc00ff", -- violet
          terminal= "urxvt",

          manageHook = manageDocks <+> myManageHook  <+> manageHook defaultConfig,

          -- manageHook = manageDocks <+> manageHook defaultConfig,
          startupHook = myStartupHook,
          layoutHook = myLayoutHook, --avoidStruts $ layoutHook defaultConfig,
          logHook            = dynamicLogWithPP xmobarPP
                               -- use hPutStrLn xmproc to transmit data via a pipe to xmobar
                               { ppOutput = hPutStrLn xmproc
                               , ppCurrent = xmobarColor "#859900" "" . wrap "[" "]"       --was: yellow
                               , ppHiddenNoWindows = xmobarColor "blue" ""             -- was: blue
                               , ppVisible = xmobarColor "#2aa198" "" . wrap "(" ")"
                               , ppUrgent  = xmobarColor "red" "yellow"
                               -- put the first 60 characters of the window title in the title area in xmobar
                               , ppTitle   = xmobarColor "#859900" "" . shorten 60     --const ""         --was: green
                               , ppLayout  = xmobarColor "#2aa198" "" --const ""
                               },
          -- Rebind Mod to the Windows key so as not to fight with Emacs
          -- You can find the names for keys in the haskell-X11 source package   http://hackage.haskell.org/package/X11-1.9.1/src/
          -- in the files Graphics/X11/Types.hsc and Graphics.X11.ExtraTypes.hsc
          modMask = mod4Mask
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock"),           -- mod-Shift-z runs locking screensaver
        -- ctrl-PrintScreen takes a screenshot using scrot
        -- The 'sleep' before running the 'scrot -s' command is
        -- to leave time for keys to be released before scrot -s tries
        -- to grab the keyboard.
        --  ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s"),
         ((controlMask, xK_Print), spawn "xfce4-screenshooter"),                          -- take screenshots with PrtSc key
        --  The first part of the (0, xK_Print) tuple states
        -- what modifier keys (ctrl, alt, etc.) have to be held down
        -- for a pattern to match.
        -- For the PrintScreen key, we don't need anything to be held down,
        -- and the zero indicates that.
        -- ((0, xK_Print), spawn "scrot"
          ((0, xK_Print), spawn "xfce4-screenshooter"),

          -- unfortunately, these keys are not recognized by Haskell X11 bindings:
--          ((0 , XF86XK_AudioLowerVolume), spawn "amixer set Master on && amixer set Headphone on && amixer set Master 1-"),  -- audio controls
--          ((0 , XF86XK_AudioRaiseVolume), spawn "amixer set Master on && amixer set Headphone on && amixer set Master 1+"),
--          ((0 , XF86XK_AudioMute), spawn "amixer set Master toggle && amixer set Headphone toggle")


          -- launch Mixer with ctrl-F1
          ((controlMask, xK_F1), spawn "gnome-alsamixer")
          
        ]




