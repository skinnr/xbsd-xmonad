import XMonad
import qualified XMonad.StackSet as W

import XMonad.Util.Themes --allow decoration themes where applicable
import XMonad.Util.Dmenu

import XMonad.Actions.FloatKeys -- add ability to move and resize floating windows
import XMonad.Actions.FloatSnap -- add ability to snap floating windows to others
import XMonad.Actions.Warp -- allow for mouse warping
import XMonad.Actions.CycleWS -- cycle through workspaces

import XMonad.Layout.WindowNavigation --add window navigation using arrow keys
import XMonad.Layout.NoBorders
import XMonad.Layout.Decoration --add window decorations where applicable
import XMonad.Layout.Spacing --add a configurable amount of space around borders 
import XMonad.Layout.Grid --add a grid layout
import XMonad.Layout.Tabbed --add a tabbed layout
import XMonad.Layout.ResizableTile --add a tiled layout that allows changing w/h
import XMonad.Layout.ShowWName -- show the workspace name

import XMonad.Layout.MosaicAlt
import qualified Data.Map as M

import Data.Monoid
import System.IO
import System.Exit

myTerminal      = "urxvtc"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth   = 1

myModMask       = mod4Mask

myWorkspaces    = ["0001","0010","0011","0100","0101","0110","0111","1000","1001"]
   
myNormalBorderColor  = "#262626"
myFocusedBorderColor = "#d0d0d0"

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm,                 xK_space ), spawn myTerminal)

    -- launch dmenu_run
    , ((modm,                 xK_d     ), spawn "xdmenu")

    -- launch gmrun
    , ((modm,		      xK_f     ), spawn "zshrun")

    -- banish pointer
    , ((modm,                 xK_p     ), warpToScreen 0 1.0 1.0)

    -- close focused WindowSet
    , ((modm,                 xK_x     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,                 xK_Return ), sendMessage NextLayout)

    -- Move focus to the next Window
    , ((modm,                 xK_Tab   ), windows W.focusDown)

    -- Mosaic Alt resize commands 
    , ((modm,         	      xK_equal       ), withFocused (sendMessage . expandWindowAlt))
    , ((modm,         	      xK_minus       ), withFocused (sendMessage . shrinkWindowAlt)) 
    , ((modm,		      xK_bracketleft ), withFocused (sendMessage . tallWindowAlt))
    , ((modm,         	      xK_bracketright), withFocused (sendMessage . wideWindowAlt))
    , ((modm,		      xK_period      ), sendMessage resetAlt)
    
    -- Push window back into tiling
    , ((modm,                 xK_t           ), withFocused $ windows . W.sink)
  
    -- Navigate, Swap and Resize windows using the arrow keys
    , ((modm,                 xK_l           ), sendMessage $ Go R)
    , ((modm,                 xK_h           ), sendMessage $ Go L)
    , ((modm,                 xK_k           ), sendMessage $ Go U)
    , ((modm,                 xK_j           ), sendMessage $ Go D)
    , ((modm .|. shiftMask,   xK_l           ), sendMessage $ Swap R)
    , ((modm .|. shiftMask,   xK_h           ), sendMessage $ Swap L)
    , ((modm .|. shiftMask,   xK_k           ), sendMessage $ Swap U)
    , ((modm .|. shiftMask,   xK_j           ), sendMessage $ Swap D)

    -- Move and resize floating windows
    , ((modm .|. controlMask, xK_h           ), withFocused $ snapMove L Nothing)  
    , ((modm .|. controlMask, xK_l           ), withFocused $ snapMove R Nothing)  
    , ((modm .|. controlMask, xK_k           ), withFocused $ snapMove U Nothing)  
    , ((modm .|. controlMask, xK_j           ), withFocused $ snapMove D Nothing)  
    , ((modm .|. controlMask, xK_minus       ), withFocused (keysResizeWindow (-10,-10) (1/3,1/3)))
    , ((modm .|. controlMask, xK_equal       ), withFocused (keysResizeWindow (10,10) (1/3,1/3))) 
    , ((modm .|. controlMask, xK_period      ), withFocused (keysMoveWindowTo (960, 540) (1/3,1/3))) 
    
    -- Restart xmonad
    , ((modm,                 xK_q           ), spawn "xmonad --recompile; xmonad --restart")

    -- Cycle Workspaces
    , ((modm,                 xK_semicolon   ),  nextWS)
    , ((modm,                 xK_g           ),  prevWS)
    , ((modm .|. shiftMask,   xK_semicolon   ),  shiftToNext)
    , ((modm .|. shiftMask,   xK_g           ),  shiftToPrev)
    ]
    ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

myLayoutHook = windowNavigation (MosaicAlt M.empty ||| resizabletiled ||| tabbedlayout)  
   where
     resizabletiled = ResizableTall nmaster delta ratio []
     tabbedlayout   = tabbed shrinkText myTabConfig
     nmaster        = 1
     ratio          = 1/2
     delta          = 3/100

myTabConfig = defaultTheme { fontName		 = "-xos4-terminus-*-*-normal--12-120-*-72-c-60-*-*"  
			   , activeTextColor 	 = "#d0d0d0"
			   , inactiveTextColor	 = "#5f5f5f"
			   , activeBorderColor   = "#afd700"
			   , inactiveBorderColor = "#5f5f5f"
			   , activeColor         = "#1c1c1c"
			   , inactiveColor       = "#1c1c1c"
			   , decoHeight	         = 16
}

mySWNConfig = defaultSWNConfig { swn_font    = "-xos4-terminus-*-*-normal--12-120-*-72-c-60-*-*"
                               , swn_bgcolor = "#afd700"
                               , swn_color   = "#005f00"
                               , swn_fade    = 1
}

-- > xprop | grep WM_CLASS
myManageHook = composeAll
	[ className =? "MPlayer"        --> doFloat
	, className =? "Gimp"           --> doFloat
        , title     =? "zshrun"         --> doFloat
	, resource  =? "desktop_window" --> doIgnore
	, resource  =? "kdesktop"       --> doIgnore ]
     
myStartupHook = do
        warpToScreen 0 1.0 1.0 

main = do
        xmonad $ defaultConfig {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        keys               = myKeys,
        layoutHook         = spacing 1 $ smartBorders $ showWName' mySWNConfig myLayoutHook, 
        manageHook         = myManageHook, 
        startupHook        = myStartupHook
} 
