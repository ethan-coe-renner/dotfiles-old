import XMonad

import qualified XMonad.StackSet as W

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Ungrab

import XMonad.Layout.Grid
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts

import XMonad.Actions.Navigation2D

import System.Exit

main :: IO ()
main =
  xmonad .
  withNavigation2DConfig def {defaultTiledNavigation = sideNavigation} .
  ewmhFullscreen .
  ewmh .
  withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey $
  myConfig

myConfig =
  def
    { modMask = mod4Mask -- Rebind Mod to the Super key
    , layoutHook = myLayout -- Use custom layouts
    , manageHook = myManageHook -- Match on certain windows
    } `additionalKeysP`
  [ ("M-S-c", io (exitWith ExitSuccess))
  , ("M-c", spawn "xmonad --recompile && xmonad --restart")
  -- program launchers --
  , ("<XF86Favorites>", spawn "bemenu-run")
  , ("M-g", spawn "qutebrowser")
  , ("M-<Return>", spawn "alacritty -e sh -c 'tmux attach || tmux'")
  , ("M-l", spawn "slock")
  , ("M-x", spawn "emacsclient -c")
    -- volume --
  , ("<XF86AudioMute>", spawn "amixer -q set Master toggle")
  , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 5%-")
  , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 5%+")
  -- brightness --
  , ("<XF86MonBrightnessUp>", spawn "light -A 10")
  , ("<XF86MonBrightnessDown>", spawn "light -U 10")
  -- window management --
  , ("M-q", kill)
  , ("M-f", sendMessage ToggleLayout)
  , ("M-w", windows W.focusDown)
  , ("M-S-w", windows W.swapDown)
  , ("M-<U>", windowGo U False)
  , ("M-<D>", windowGo D False)
  , ("M-<R>", windowGo R False)
  , ("M-<L>", windowGo L False)
  , ("M-S-<U>", windowSwap U False)
  , ("M-S-<D>", windowSwap D False)
  , ("M-S-<R>", windowSwap R False)
  , ("M-S-<L>", windowSwap L False)
  --- can also add navigation of screens here
  ]

-- XMonad Session Management --
myManageHook :: ManageHook
myManageHook =
  composeAll [className =? "Gimp" --> doFloat, isDialog --> doFloat]

myLayout = toggleLayouts Full (spacing 15 $ tiled ||| threeCol)
  where
    threeCol = ThreeColMid nmaster delta ratio
    tiled = Tall nmaster delta ratio
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 1 / 2 -- Default proportion of screen occupied by master pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = magenta " • "
    , ppTitleSanitize = xmobarStrip
    , ppCurrent = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent = red . wrap (yellow "!") (yellow "!")
    , ppOrder = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused = wrap (white "[") (white "]") . green . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow
  -- | Windows should have *some* title, which should not not exceed a
  -- sane length.
    ppWindow :: String -> String
    ppWindow =
      xmobarRaw .
      (\w ->
         if null w
           then "untitled"
           else w) .
      shorten 30
    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta = xmobarColor "#ff79c6" ""
    purple = xmobarColor "#bd93f9" ""
    white = xmobarColor "#f8f8f2" ""
    green = xmobarColor "#50fa7b" ""
    yellow = xmobarColor "#f1fa8c" ""
    red = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#e0e0e0" ""
    blue = xmobarColor "#6272a4" ""
