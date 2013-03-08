import XMonad
import XMonad.Layout
import XMonad.Layout.Spiral
import XMonad.Layout.Accordion
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

myModMask = mod4Mask
myBorderWidth = 1
myNormalBorderColor = "black" -- "#000000"
myFocusedBorderColor = "red" -- "#ff0000"
myManageHook = manageHook defaultConfig <+> manageDocks
myLayouts = avoidStruts (tall ||| Mirror tall ||| spiral (6/7))
                  where tall = Tall 1 (3/100) (1/2)
myTerminal = "urxvt"
myStartupHook = spawn "xmobar"

main = do
    xmonad $ withUrgencyHook NoUrgencyHook
           $ defaultConfig
           { borderWidth        = myBorderWidth
           , focusedBorderColor = myFocusedBorderColor
           , layoutHook         = myLayouts
           , manageHook         = myManageHook
           , modMask            = myModMask
           , normalBorderColor  = myNormalBorderColor
           , startupHook        = myStartupHook
           , terminal           = myTerminal }
