-- previous setup
--
-- main = do
--     xmproc <- spawnPipe "/usr/local/bin/xmobar ~/.xmobarrc"
--     xmonad $ defaultConfig 
-- 	{ manageHook = manageDocks <+> manageHook defaultConfig
-- 	, layoutHook = avoidStruts  $  layoutHook defaultConfig
-- 	, logHook = dynamicLogWithPP xmobarPP
-- 			{ ppOutput = hPutStrLn xmproc
-- 			, ppTitle = xmobarColor "#66aa11" "" . shorten 50
-- 			, ppCurrent = xmobarColor "#960050" ""
-- 			} 
-- 	, modMask = mod4Mask	-- Rebind Mod to the Windows key
-- 	, borderWidth		= 1
-- 	, terminal 		= "urxvt"
-- 	, normalBorderColor	= "#5f5f5f"
-- 	, focusedBorderColor	= "#d0d0d0"
-- 	} `additionalKeys`
-- 	[ ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
-- 	, ((0, xK_Print), spawn "scrot")
-- 	]
	    
--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
------------------------------------------------------------------------
-- imports
import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run 
import XMonad.Util.Themes --allow decoration themes where applicable

import XMonad.ManageHook
import XMonad.Util.NamedScratchpad

import XMonad.Actions.FloatKeys -- add ability to move and resize floating windows
import XMonad.Actions.FloatSnap -- add ability to snap floating windows to others
import XMonad.Actions.Warp -- allow for mouse warping
 
import XMonad.Layout.WindowNavigation --add window navigation using arrow keys
import XMonad.Layout.NoBorders
import XMonad.Layout.Decoration --add window decorations where applicable
import XMonad.Layout.Spacing --add a configurable amount of space around borders 
import XMonad.Layout.Grid --add a grid layout
import XMonad.Layout.Tabbed --add a tabbed layout
import XMonad.Layout.ResizableTile --add a tiled layout that allows changing w/h
import XMonad.Actions.CycleWS -- cycle through workspaces

import XMonad.Layout.MosaicAlt
import qualified Data.Map as M

import Data.Monoid
import System.IO
import System.Exit

import qualified XMonad.StackSet as S
import qualified XMonad.StackSet as W
------------------------------------------------------------------------
-- Basic settings
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--

myTerminal      = "urxvt"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window borders in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which denotes not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The default number of Workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- OF THIs list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["0001","0010","0011","0100","0101","0110","0111","1000","1001"]
   
-- Border colors focusedbordercolor unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#262626"
myFocusedBorderColor = "#d0d0d0"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm,                 xK_space ), safeSpawnProg myTerminal)

    -- launch dmenu_run
    , ((modm,                 xK_d     ), spawn "dmenu_run -nb '#303030' -sb '#afd700' -nf '#9e9e9e' -sf '#005f00' -fn 'Terminess Powerline-8'")

    -- launch gmrun
    , ((modm,		      xK_f     ), safeSpawnProg "gmrun")

    -- banish pointer
    , ((modm,                 xK_p     ), warpToScreen 0 1.0 1.0)

    -- close focused WindowSet
    , ((modm,                 xK_x      ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,                 xK_Return ), sendMessage NextLayout)

    -- unused key bindings ('Tall' layout)
    --  Reset the layouts on the current workspace to default
    --  , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows those the correct size
    -- , ((modm,               xK_n     ), refresh)

    -- Move focus to the nextWS window
    -- , ((modm,               xK_k     ), windows W.focusDown)
    
    -- Move focus to the previous window
    -- , ((modm,               xK_j     ), windows W.focusUp)

    -- Swap the focused Window and the master window
    -- , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Swap the focused Window with the next window
    -- , ((modm .|. shiftMask, xK_j     ), windows W.swapDown)

    -- Swap the focused Window with the previous window
    -- , ((modm .|. shiftMask, xK_k     ), windows W.swapUp)

    -- Shrink the master area
    -- , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    -- , ((modm,               xK_equal     ), sendMessage Expand)

     -- Increment the number of Windows in the master area
    -- , ((modm .|. shiftMask,     xK_comma       ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    -- , ((modm .|. shiftMask,     xK_period      ), sendMessage (IncMasterN (-1)))


    -- Move focus to the next Window
    , ((modm,                 xK_Tab   ), windows W.focusDown)

    -- Mosaic Alt resize commands 
    , ((modm,         	      xK_equal       ), withFocused (sendMessage . expandWindowAlt))
    , ((modm,         	      xK_minus       ), withFocused (sendMessage . shrinkWindowAlt)) 
    , ((modm,		      xK_bracketleft ), withFocused (sendMessage . tallWindowAlt))
    , ((modm,         	      xK_bracketright), withFocused (sendMessage . wideWindowAlt))
    , ((modm,		      xK_period      ), sendMessage resetAlt)
    
    -- Push window back into tiling
    , ((modm,                 xK_t        ), withFocused $ windows . W.sink)
  
    -- Navigate, Swap and Resize windows using the arrow keys
    , ((modm,                 xK_l ), sendMessage $ Go R)
    , ((modm,                 xK_h ), sendMessage $ Go L)
    , ((modm,                 xK_k ), sendMessage $ Go U)
    , ((modm,                 xK_j ), sendMessage $ Go D)
    , ((modm .|. shiftMask,   xK_l ), sendMessage $ Swap R)
    , ((modm .|. shiftMask,   xK_h ), sendMessage $ Swap L)
    , ((modm .|. shiftMask,   xK_k ), sendMessage $ Swap U)
    , ((modm .|. shiftMask,   xK_j ), sendMessage $ Swap D)
    --, ((modm .|. controlMask, xK_l), sendMessage   Expand)  
    --, ((modm .|. controlMask, xK_h ), sendMessage   Shrink)
    --, ((modm .|. controlMask, xK_k   ), sendMessage   MirrorExpand)
    --, ((modm .|. controlMask, xK_j ), sendMessage   MirrorShrink

    -- Floating Scratchpads
    , ((modm,                 xK_Menu  ), namedScratchpadAction scratchpads "rexima") 
    , ((modm .|. shiftMask,   xK_Menu  ), namedScratchpadAction scratchpads "mocp") 

    -- Move and resize floating windows
    , ((modm .|. controlMask, xK_h     ), withFocused $ snapMove L Nothing)  
    , ((modm .|. controlMask, xK_l     ), withFocused $ snapMove R Nothing)  
    , ((modm .|. controlMask, xK_k     ), withFocused $ snapMove U Nothing)  
    , ((modm .|. controlMask, xK_j     ), withFocused $ snapMove D Nothing)  
    , ((modm .|. controlMask, xK_minus ), withFocused (keysResizeWindow (-10,-10) (1/3,1/3)))
    , ((modm .|. controlMask, xK_equal ), withFocused (keysResizeWindow (10,10) (1/3,1/3))) 
    , ((modm .|. controlMask, xK_period),withFocused (keysMoveWindowTo (960, 540) (1/3,1/3))) 
    
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    --, ((modm .|. shiftMask, xK_q     ), io (exitWith exitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- Cycle Workspaces
    , ((modm,               xK_semicolon ),  nextWS)
    , ((modm,               xK_g         ),  prevWS)
    , ((modm .|. shiftMask, xK_semicolon ),  shiftToNext)
    , ((modm .|. shiftMask, xK_g         ),  shiftToPrev)


    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    --, ((modMask .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = windowNavigation (MosaicAlt M.empty ||| resizabletiled ||| tabbedlayout)  
   where

     -- resizable tiling algorithm
     resizabletiled = ResizableTall nmaster delta ratio [] 
 
     -- tabbed view
     tabbedlayout   = tabbed shrinkText myTabConfig 

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Tabbed Layout decoration configuration

myTabConfig = defaultTheme { fontName		 = "xft:terminus:size=8"
			   , activeTextColor 	 = "#d0d0d0"
			   , inactiveTextColor	 = "#5f5f5f"
			   , activeBorderColor   = "#afd700"
			   , inactiveBorderColor = "#5f5f5f"
			   , activeColor         = "#1c1c1c"
			   , inactiveColor       = "#1c1c1c"
			   , decoHeight	         = 16
}
------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook :: ManageHook
myManageHook = 
	manageDocks <+>
	manageWindows <+>
	manageScratchPad

manageWindows :: ManageHook
manageWindows = composeAll 
	[ className =? "MPlayer"        --> doFloat
	, className =? "Gimp"           --> doFloat ] <+> manageScratchPad
	--, resource  =? "desktop_window" --> doIgnore
	--, resource  =? "kdesktop"       --> doIgnore ] <+> manageScratchPad
     
manageScratchPad :: ManageHook
manageScratchPad = namedScratchpadManageHook scratchpads

scratchpads :: NamedScratchpads
scratchpads = [ NS "rexima" "urxvt -e rexima" (title =? "rexima") (customFloating $ S.RationalRect 0.623 0.777 0.375 0.2)
  	      , NS "mocp" "urxvt -e mocp" (title =? "mocp") (customFloating $ S.RationalRect 0.498 0.627 0.5 0.35)]
  where 
    role = stringProperty "WM_WINDOW_ROLE"

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
-- Dzen + Conky
-- myDzenStatus = "dzen2 -x '0' -y '1060' -w '960' -ta 'l'" ++ myDzenStyle
-- myDzenConky  = "conky -c ~/.conkyrc | dzen2 -x '960' -w '1920' -ta 'c'" ++ myDzenStyle
-- myDzenStyle  = " -h '20' -fg '#777777' -bg '#222222' -fn '-*-terminus-medium-r-*-*-14-*-*-*-*-*-*-*' "
 
-- myDzenPP h  = defaultPP 
--     { ppCurrent = dzenColor "#3399ff" "" . wrap " " " "
--     , ppHidden  = dzenColor "#dddddd" "" . wrap " " " "
--     , ppHiddenNoWindows = dzenColor "#777777" "" . wrap " " " "
--     , ppUrgent  = dzenColor "#ff0000" "" . wrap " " " "
--     , ppSep     = "     "
--     , ppLayout  = dzenColor "#aaaaaa" "" 
--     , ppTitle   = dzenColor "#ffffff" "" . wrap " " " " 
--     , ppOutput  = hPutStrLn h 
--     }

myXmobarPP h = defaultPP
 	{ ppOutput  = hPutStrLn h
 	, ppTitle   = xmobarColor "#afd700" "" . shorten 50
 	, ppCurrent = xmobarColor "#ff005f" ""
        } 

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()
 
------------------------------------------------------------------------
-- Run xmonad configuration
--
main = do
-- when using Dzen & Conky...   
--      xmproc <- spawnPipe myDzenStatus
--	conky  <- spawnPipe myDzenConky
-- when using xmobar...
        xmproc <- spawnPipe "/usr/local/bin/xmobar ~/.xmonad/xmobarrc.hs"
        xmonad $ defaultConfig {
       -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = spacing 1 $ smartBorders $ avoidStruts $ myLayout, 
        manageHook         = myManageHook <+> manageHook defaultConfig,
        handleEventHook    = myEventHook,
        logHook            = dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ myXmobarPP xmproc, 
        startupHook        = myStartupHook
} 

------------------------------------------------------------------------
-- Old defaults:
-- defaults = defaultConfig {
      -- simple stuff
--        terminal           = myTerminal,
--        focusFollowsMouse  = myFocusFollowsMouse,
--        clickJustFocuses   = myClickJustFocuses,
--        borderWidth        = myBorderWidth,
--        modMask            = myModMask,
--        workspaces         = myWorkspaces,
--        normalBorderColor  = myNormalBorderColor,
--        focusedBorderColor = myFocusedBorderColor,
--
      -- key bindings
--        keys               = myKeys,
--        mouseBindings      = myMouseBindings,

      -- hooks, layouts
--        layoutHook         = avoidStruts $ myLayout,
--        manageHook         = manageDocks <+> myManageHook <+> manageDocks,
--        handleEventHook    = myEventHook,
--        logHook            = myLogHook,
--        startupHook        = myStartupHook
--    }
