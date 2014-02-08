--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
 
import XMonad
import System.Exit

import System.IO.UTF8 (hPutStrLn)

import XMonad.Actions.Submap
import XMonad.Layout.Tabbed
import XMonad.Layout.Circle
import XMonad.Layout.ThreeColumns
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Prompt
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Prompt.Input
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.DynamicLog (PP(..), defaultPP, dynamicLogWithPP,
                                dzenColor, pad, shorten)
import XMonad.Layout.IndependentScreens (withScreens)
import XMonad.Layout.Spiral
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Scratchpad

import Graphics.X11.Xlib
import Graphics.X11.Xinerama
-- import Graphics.X11.Xlib.Extras
-- import Graphics.X11.Xlib.Event

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Data.List
import Data.Function

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "urxvtc"

myFont          = "-*-proggyopti-medium-r-normal-*-11-*-96-96-c-70-iso8859-1"

myColorBG       = "#040810"
myColorRed      = "#b691a0"
myColorBrown    = "#504e2a"
myColorWhite    = "#d0d6dd"
myColorCyan     = "#357aac"
 
-- Width of the window border in pixels.
--
myBorderWidth   = 1
 
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask
 
-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = withScreens 2 ["term", "ed", "www"] ++ map show [5..10] ++ ["music"]
 
-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = myColorBG
myFocusedBorderColor = myColorCyan

myXPConfig = defaultXPConfig

-- sendKeyPress :: KeyMask -> KeySym -> X ()
-- sendKeyPress = userCode $ withDisplay sendKeyPress'
-- 
-- sendKeyPress' :: Display -> KeyMask -> KeySym -> X ()
-- sendKeyPress' dpy mask key = do
--   root <- asks theRoot
--   time <- currentTime
--   evt  <-


renameWS :: String -> X ()
renameWS newTag = windows $ \s -> let old = W.tag $ W.workspace $ W.current s
                                  in W.renameTag old newTag s


------------------------------------------------------------------------------
-- Status bar
--

myXmonadBarL = concat
    [ "dzen2 -x '0' -y '0' -h '16' -w '680' -ta 'l'"
    , " -fg '", myColorWhite
    , "' -bg '", myColorBG
    , "' -fn '", myFont
    , "'"
    ]
myXmonadBarR = concat
    [ "conky -c /home/b/.xmonad/conky_dzen | "
    , "dzen2 -x '680' -y '0' -w '1000' -h '16' -ta 'r' "
    , "-bg '", myColorBG
    , "' -fg '", myColorWhite
    , "' -fn '", myFont
    , "'"
    ]


myStartupHook =
    spawnOnce "xsetroot -cursor_name left_ptr &" >>
    spawnOnce "export LANG=en_US.UTF-8" >>
    spawnOnce "urxvtd -q -o -f" >>
    spawnOnce "xrdb -load $HOME/.Xresources" >>
    spawnOnce "sh $HOME/.fehbg &" >>
    spawnOnce "unclutter &" >>
    spawnOnce "compton -c -b -e 0.8 -t -8 -l -9 -r 6 -o 0.7 -m 1.0 &"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- launch a terminal
    [ ((modMask .|. shiftMask, xK_Return)       , spawn $ XMonad.terminal conf)
    , ((modMask,               xK_c)            , spawn $ XMonad.terminal conf)

    , ((modMask .|. shiftMask, xK_r)            , spawn "pkill dzen && xmonad --recompile; xmonad --restart")

 
    --, ((modMask,               xK_p     )       , spawn "mpc toggle")
    , ((modMask,               xK_bracketright) , spawn "mpc volume +5")
    , ((modMask,               xK_bracketleft)  , spawn "mpc volume -5")
    , ((modMask .|. shiftMask, xK_l)            , spawn "xscreensaver-command -lock")

    , ((modMask              , xK_x)            , spawn "xmodmap ~/.Xmodmap")

    --, ((modMask .|. controlMask, xK_p), spawn "sleep 0.2; scrot -s")
    , ((modMask, xK_p), spawn "scrot")
 
    -- close focused window 
    , ((modMask .|. shiftMask, xK_c     ), kill)
 
     -- Rotate through the available layout algorithms
    , ((modMask,               xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
 
    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modMask,               xK_n     ), refresh)
 
    -- Move focus to the next window
    , ((modMask,               xK_Tab   ), windows W.focusDown)
 
    -- Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown)
 
    -- Move focus to the previous window
    , ((modMask,               xK_k     ), windows W.focusUp  )
 
    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.focusMaster  )
 
    -- Swap the focused window and the master window
    , ((modMask,               xK_Return), windows W.swapMaster)
 
    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
 
    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )
 
    -- Shrink the master area
    , ((modMask,               xK_h     ), sendMessage Shrink)
 
    -- Expand the master area
    , ((modMask,               xK_l     ), sendMessage Expand)
 
    -- Push window back into tiling
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
 
    -- Increment the number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))
 
    -- toggle the status bar gap
    , ((modMask              , xK_b     ), sendMessage ToggleStruts)
 
    , ((modMask              , xK_g     ),
       workspacePrompt  myXPConfig (windows . W.greedyView))
    , ((modMask .|. shiftMask, xK_g     ),
       workspacePrompt  myXPConfig (windows . W.shift))

    -- C-t submap
    , ((controlMask, xK_t)       , submap . M.fromList $
       [ ((controlMask,  xK_t)      ,   toggleWS) 
       , ((0,            xK_Tab)    ,   windows W.focusDown) -- @@ Move focus to the next window
       , ((shiftMask,    xK_Tab)    ,   windows W.focusUp  ) -- @@ Move focus to the previous window
       , ((0,            xK_c)      ,   spawn $ XMonad.terminal conf)
       , ((0,            xK_k)      ,   kill)
       , ((0,            xK_Return) ,   windows W.swapMaster)
--       , ((shiftMask,    xK_1)      ,   spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
       , ((shiftMask,    xK_1)      ,   scratchpadSpawnActionTerminal "gterm")
       , ((0,            xK_s)      ,   sshPrompt myXPConfig)

       , ((shiftMask,    xK_s)      ,   spawn "/usr/bin/tracker-search-tool")
       , ((0,            xK_x)      ,   spawn "/home/nelhage/bin/rp-hm-complete.sh")

       , ((0,            xK_g)      ,   workspacePrompt  myXPConfig (windows . W.greedyView))
       , ((shiftMask,    xK_g)      ,   workspacePrompt  myXPConfig (windows . W.shift))
       , ((0,            xK_n)      ,   inputPrompt myXPConfig "Workspace name" ?+ addWorkspace)
       , ((shiftMask,    xK_k)      ,   removeWorkspace)

       , ((0,            xK_r)      ,   renameWorkspace myXPConfig)
       , ((0,            xK_q)      ,   viewScreen 0)
       , ((0,            xK_w)      ,   viewScreen 1)
       ] ++
       [((0, k), windows $ W.greedyView i)
        | (i, k) <- zip (take 10 (XMonad.workspaces conf)) [xK_1 ..]
       ])
    ]
    ++
 
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
 
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((modMask .|. mask, key), f sc)
        | (key, sc) <- zip [xK_q, xK_w, xK_e] [0..]
        , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
 
    -- mod-button3, Raise the window to the top of the stack
    , ((modMask, button3), (\w -> focus w >> windows W.swapMaster))
 
    -- mod-button2, Set the window to floating mode and resize by dragging
    , ((modMask, button2), (\w -> focus w >> mouseResizeWindow w))
 
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
-- myLayout = avoidStruts $
--            tiled
--            ||| Mirror tiled
--            ||| Full
--            ||| tabbed shrinkText defaultTheme
--            ||| threeCol
-- --           ||| spiral (4/3)
--   where
--      -- default tiling algorithm partitions the screen into two panes
--      tiled   = Tall nmaster delta ratio
-- 
--      threeCol = ThreeCol nmaster delta ratio
--  
--      -- The default number of windows in the master pane
--      nmaster = 1
--  
--      -- Default proportion of screen occupied by master pane
--      ratio   = 1/2
--  
--      -- Percent of screen to increment by when resizing panes
--      delta   = 2/100

myLayout = avoidStruts (layoutHook defaultConfig)
 
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

-- myManageHook = composeAll
--     [ className =? "MPlayer"        --> doFloat
--     , className =? "Gimp"           --> doFloat
-- --    , className =? "display"        --> doFloat
--     , className =? "Wpa_gui"        --> doFloat
--     , resource  =? "desktop_window" --> doIgnore
--     , resource  =? "kdesktop"       --> doIgnore
--     , title     =? "zsh-onetime"    --> doFloat
--     , manageDocks
--     , scratchpadManageHook (W.RationalRect 0.125 0.25 0.75 0.5)
--     ]

myManageHook = manageDocks <+> manageHook defaultConfig
 
 
------------------------------------------------------------------------
-- Status bars and logging
 
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- myLogHook = fadeInactiveLogHook 0xA0000000

myLogHook h = dynamicLogWithPP (defaultPP
    { ppOutput              = hPutStrLn h
    , ppCurrent             = dzenColor myColorRed myColorBG . pad
    , ppHidden              = dzenColor myColorBrown myColorBG . no_scratch_pad
    , ppHiddenNoWindows     = dzenColor myColorBG myColorBG . no_scratch_pad
    -- ppSep                 = dzenColor myColorRed myColorBG "╻"
    -- , ppWsSep               = dzenColor myColorRed myColorBG "╷"
    , ppSep                 = dzenColor myColorRed myColorBG "|"
    , ppWsSep               = dzenColor myColorRed myColorBG ""
    , ppTitle               = dzenColor myColorBrown myColorBG . shorten 50
    , ppOrder               = \(ws:l:t:_) -> [ws, l, t]
    , ppLayout              = dzenColor myColorRed myColorBG . id
    })
  where
    no_scratch_pad ws = if ws == "NSP" then "" else pad ws
 
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
 
-- Run xmonad with the settings you specify. No need to modify this.
--
main = spawnPipe myXmonadBarR >>
       spawnPipe myXmonadBarL >>=
       xmonad . defaults
 
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will 
-- use the defaults defined in xmonad/XMonad/Config.hs
-- 
-- No need to modify this.
--
defaults dzenLeftBar = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
 
      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
 
      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        logHook            = myLogHook dzenLeftBar >>
                             updatePointer (Relative 0.5 0.5),
        startupHook        = myStartupHook
    }
