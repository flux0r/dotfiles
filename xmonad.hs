-- vim: fdm=marker sw=4 sts=4 ts=4 et ai

{-# LANGUAGE ForeignFunctionInterface #-}

------------------------------------------------------------------------
-- ~/.xmonad/xmonad.hs
-- validate syntax: $ xmonad --recompile
------------------------------------------------------------------------

-- Based on And1's xmonad.hs
--

import Control.Exception (catch)

import XMonad hiding (Tall)
import XMonad.Actions.CycleWS
import XMonad.Actions.NoBorders
import XMonad.Actions.SpawnOn
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.HintedTile
import XMonad.Layout.ResizableTile
--import XMonad.Layout.LayoutHints
import XMonad.Layout.ThreeColumns
import XMonad.Layout.PerWorkspace
import XMonad.Layout.WindowNavigation
import XMonad.Layout.NoBorders
import XMonad.ManageHook
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Run
import XMonad.Hooks.SetWMName
--import Text.Regex.Posix
import System.Exit
import System.IO
import System.IO.Error (IOError)
import System.Directory
import System.Environment

import System.Posix.IO
import System.Posix.Process
import System.Posix.Types

import Graphics.X11
import Graphics.X11.Xinerama

import qualified Data.Map as M
import qualified System.IO.UTF8
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.StackSet as W
--import XMonad.Hooks.ICCCMFocus -- use in next xmonad-contrib to fix focus bugs

-- Begin getHostName
import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Error
import Foreign.Marshal.Array

foreign import ccall unsafe "gethostname" gethostname :: CString -> CSize -> IO CInt

getHostName :: IO HostName
getHostName = allocaArray0 size $ \cstr -> do
        throwErrnoIfMinus1_ "getHostName" $ gethostname cstr (fromIntegral size)
        peekCString cstr
    where size = 256

type HostName = String
-- End getHostName


-- Settings {{{
-- Definitions {{{
myNormalBGColor :: String
myNormalFGColor :: String
myFocusedBGColor :: String
myFocusedFGColor :: String
myNormalBorderColor :: String
myFocusedBorderColor :: String
myUrgentFGColor :: String
myUrgentBGColor :: String
mySeparatorColor :: String

myFont :: String
myTerminal :: String

myNormalBGColor = "#2e3436"
myFocusedBGColor = "#414141"
myNormalFGColor = "#babdb6"
myFocusedFGColor = "#73d216"
myNormalBorderColor = myNormalBGColor
myFocusedBorderColor = "#2277aa"
myUrgentFGColor = "#f57900"
myUrgentBGColor = myNormalBGColor
mySeparatorColor = "#555555"
myFont = "-misc-fixed"
myTerminal = "urxvt"

main :: IO ()
main = do
    home <- catch (getEnv "HOME") (const $ return [] :: IOError -> IO [a])

    d <- catch (getEnv "DISPLAY") (const $ return [] :: IOError -> IO [a])
    dpy <- openDisplay d
    let scr = defaultScreenOfDisplay dpy
    let myWidth = read (show (widthOfScreen scr)) :: Int
    let myHeight = read (show (heightOfScreen scr)) :: Int

    let myStatusBar = "dzen2 -x '0' -y '0' -h '16' -w " ++ show (myWidth - 680) ++ " -ta 'l' -bg '" ++ myNormalBGColor ++ "' -fg '" ++ myNormalFGColor ++ "' -fn 'fixed' -e 'onstart=lower'"
    let myTopBar = "conky -c ~/.conkytoprc | dzen2 -x " ++ show (myWidth - 680) ++ " -y '0' -h '16' -w '680' -ta 'r' -bg '" ++ myNormalBGColor  ++ "' -fg '" ++ myNormalFGColor ++ "' -fn 'fixed' -e 'onstart=lower'"
    let myBottomBar = "conky -c ~/.conkybottomrc | dzen2 -x '0' -y " ++ show (myHeight - 16) ++ " -h '16' -w " ++ show (myWidth-128) ++ " -ta 'l' -bg '" ++ myNormalBGColor ++ "' -fg '" ++ myNormalFGColor ++ "' -fn '" ++ myFont ++ "' -e 'onstart=lower'"
    din  <- spawnPipe myStatusBar
    din2 <- spawnPipe myTopBar
    din3 <- spawnPipe myBottomBar

    spawn ("stalonetray -bg '" ++ myNormalBGColor ++ "'") -- tray
    spawn ("xsetroot -solid '" ++ myNormalBGColor ++ "'") -- set background color
--     spawn "numlockx on" -- activate numlock
    spawn "xsetroot -cursor_name left_ptr" --set mouse cursor
--     spawn "nitrogen --restore" -- set background

    hostname <- getHostName
    file <- doesFileExist "/tmp/xmonad_restart"
    if (file)
      then removeFile "/tmp/xmonad_restart"
      else startup home hostname

    xmonad $ myUrgencyHook myHeight myWidth $ ewmh defaultConfig
       { normalBorderColor = myNormalBorderColor
       , focusedBorderColor = myFocusedBorderColor
       , terminal = myTerminal
       , layoutHook = windowNavigation myLayout
       , manageHook = manageSpawn <+> myManageHook <+> manageDocks
       , handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook
       , workspaces = ["1:term", "2:www", "3:mail", "4:im"] ++ map show [5..18 ::Int]
--        , modMask = mod1Mask
--        , keys = myKeys
--        , mouseBindings = myMouseBindings
       , borderWidth = 1
       , logHook = do
--            --takeTopFocus -- use in next xmonad-contrib to fix focus bugs (http://code.google.com/p/xmonad/issues/detail?id=177)
             dynamicLogWithPP $ myLogHook din home
       , focusFollowsMouse = True
--        , startupHook = setWMName "LG3D" -- fix java bug with non reparenting wm, because sun/oracle jdk uses a hardcoded list of wms and xmonad is not one of them. (this fixes cisco asdm).
       }

myUrgencyHook height width = withUrgencyHook dzenUrgencyHook
    { args = ["-x", "0", "-y", show (height - 16), "-h", "16", "-w"
    , show (width), "-ta", "r", "-expand", "l", "-bg", myNormalBGColor
    , "-fg", "#0077cc", "-fn", myFont] }


startup :: String -> HostName -> IO ()
startup home "Kheldar" = do
    startup home "default"

    spawn "wicd-gtk"
    spawn (home ++ "/.dropbox-dist/dropbox")
    spawn "gtk-redshift -l 65.8:22"

startup home "Belgarion" = do
    startup home "default"

    spawn "pidgin"
    spawn "thunderbird"
    spawn (home ++ "/bin/start_gnome-screensaver")
    spawn "gnome-screensaver-command --lock"
    spawn "xcompmgr"
    spawn "gtk-redshift -l 65.8:22 -t 6500:3700"

startup home "taurus" = do
    startup home "default"

    spawn "xset m 0 150" -- mouse acceleration

startup home hostname = do -- default
    spawn "xset -b b off" -- disable bell
    spawn "xrdb -load $HOME/.Xresources"

restart_xmonad :: X ()
restart_xmonad = do
    spawn "killall conky dzen2 stalonetray"
    catchIO(writeFile "/tmp/xmonad_restart" "true")
    restart "xmonad" True

restart_dzen :: IO ()
restart_dzen = do
    spawn "killall conky"

    d <- catch (getEnv "DISPLAY") (const $ return [] :: IOError -> IO [a])
    dpy <- openDisplay d
    let scr = defaultScreenOfDisplay dpy
    let myWidth = read (show (widthOfScreen scr)) :: Int
    let myHeight = read (show (heightOfScreen scr)) :: Int

    let myTopBar = "conky -c ~/.conkytoprc | dzen2 -x " ++ show (myWidth - 680) ++ " -y '0' -h '16' -w '680' -ta 'r' -bg '" ++ myNormalBGColor  ++ "' -fg '" ++ myNormalFGColor ++ "' -fn 'fixed' -e 'onstart=lower'"
    let myBottomBar = "conky -c ~/.conkybottomrc | dzen2 -x '0' -y " ++ show (myHeight - 16) ++ " -h '16' -w " ++ show (myWidth-128) ++ " -ta 'l' -bg '" ++ myNormalBGColor ++ "' -fg '" ++ myNormalFGColor ++ "' -fn '" ++ myFont ++ "' -e 'onstart=lower'"
    spawn myTopBar
    spawn myBottomBar

-- Layout options:
myLayout = avoidStruts $ smartBorders $
    onWorkspace "1:term" (hintedTile Wide ||| noBorders Full) $
    onWorkspaces ["2:www","3:mail"] (noBorders Full) $
    onWorkspace "4:im" (noBorders $ HintedTile nmaster delta (4/5) TopLeft Tall) $
    (hintedTile Tall ||| hintedTile Wide ||| (noBorders Full) ||| (ThreeCol 1 (3/100) (1/2)) ||| ResizableTall 1 (3/100) (1/2) [])
    where
    hintedTile = HintedTile nmaster delta ratio TopLeft
    nmaster = 1
    ratio = toRational (2/(1+sqrt(5)::Double))
    delta = 3/100

-- XPConfig options:
myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
    { font = "fixed"
    , bgColor = myNormalBGColor
    , fgColor = myNormalFGColor
    , fgHLight = myFocusedFGColor
    , bgHLight = myFocusedBGColor
    , borderColor = myNormalBorderColor
    , promptBorderWidth = 1
    , position = Bottom
    , height = 16
    , historySize = 100
    }

-- Key bindings:
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask .|. shiftMask,   xK_Return), spawn $ XMonad.terminal conf)
    , ((mod4Mask,                xK_w     ), spawn "nitrogen --sort=alpha ~/Wallpapers")
    , ((mod4Mask,                xK_p     ), shellPromptHere myXPConfig)
    , ((mod4Mask,                xK_l     ), spawn "gnome-screensaver-command --lock")
    , ((modMask,                 xK_Print ), spawn "scrot desk_%Y-%m-%d-%H%M%S_$wx$h.png -d 1 -e 'mv $f ~/screenshots/' ") -- take a screenshot
    , ((modMask .|. controlMask, xK_x     ), kill) -- close focused window
    , ((modMask .|. shiftMask,   xK_b     ), withFocused toggleBorder)
    , ((modMask .|. shiftMask,   xK_s     ), sendMessage ToggleStruts)
    , ((modMask,                 xK_space ), sendMessage NextLayout) -- rotate through the available layout algorithms
    , ((modMask .|. shiftMask,   xK_f     ), setLayout $ XMonad.layoutHook conf) -- reset the layouts on the current workspace to default
    , ((modMask,                 xK_n     ), refresh) -- resize viewed windows to the correct size
    , ((modMask,                 xK_Tab   ), windows W.focusDown) -- move focus to the next window
    , ((modMask .|. shiftMask,   xK_Tab   ), windows W.focusUp) -- move focus to the next window
    , ((modMask,                 xK_Return), windows W.focusMaster) -- move focus to the master window
    , ((modMask,                 xK_m     ), windows W.swapMaster) -- swap the focused window and the master window
    , ((modMask .|. shiftMask,   xK_j     ), sendMessage MirrorShrink) -- shrink the master area
    , ((modMask .|. shiftMask,   xK_k     ), sendMessage MirrorExpand) -- expand the master area
    , ((modMask .|. shiftMask,   xK_h     ), sendMessage Shrink) -- shrink the master area
    , ((modMask .|. shiftMask,   xK_l     ), sendMessage Expand) -- expand the master area
	, ((modMask,                 xK_Right ), sendMessage $ Go R)
    , ((modMask,                 xK_Left  ), sendMessage $ Go L)
    , ((modMask,                 xK_Up    ), sendMessage $ Go U)
    , ((modMask,                 xK_Down  ), sendMessage $ Go D)
    , ((modMask .|. controlMask, xK_Right ), sendMessage $ Swap R)
    , ((modMask .|. controlMask, xK_Left  ), sendMessage $ Swap L)
    , ((modMask .|. controlMask, xK_Up    ), sendMessage $ Swap U)
    , ((modMask .|. controlMask, xK_Down  ), sendMessage $ Swap D)
    , ((modMask .|. controlMask, xK_d     ), withFocused $ windows . W.sink) -- push window back into tiling
    , ((modMask .|. controlMask .|. shiftMask, xK_Left ), sendMessage (IncMasterN 1)) -- increment the number of windows in the master area
    , ((modMask .|. controlMask .|. shiftMask, xK_Right), sendMessage (IncMasterN (-1))) -- decrement the number of windows in the master area
    , ((mod4Mask,                 xK_g     ), goToSelected defaultGSConfig)

    -- multimedia keys
    , ((0, 0x1008ff11), spawn "amixer -q set Master 2%-")    -- XF86AudioLowerVolume
    , ((0, 0x1008ff13), spawn "amixer -q set Master 2%+")    -- XF86AudioRaiseVolume
    , ((0, 0x1008ff12), spawn "amixer -q set Master toggle") -- XF86AudioMute
    , ((0, 0x1008ff17), spawn "mpc2 next")                   -- XF86AudioNext
    , ((0, 0x1008ff16), spawn "mpc2 prev")                   -- XF86AudioPrev
    , ((0, 0x1008ff14), spawn "mpc2 toggle")                 -- XF86AudioPlay

    , ((modMask .|. shiftMask .|. controlMask, xK_q), io (exitWith ExitSuccess)) -- quit xmonad
    , ((modMask .|. shiftMask .|. controlMask, xK_r), restart_xmonad) -- restart xmonad
    , ((modMask .|. controlMask, xK_r), catchIO restart_dzen) -- restart dzen
    --, ((modMask .|. controlMask, xK_r), spawn "killall conky dzen2 stalonetray" >> restart "xmonad" True) -- restart xmonad
    ]
    ++
    [ ((m .|. modMask, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_F1 .. xK_F9] -- mod-[F1..F9], Switch to workspace N
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)] -- mod-shift-[F1..F9], Move client to workspace N
    ]
    ++
    [ ((m .|. mod4Mask, k), windows $ f i)
    | (i, k) <- zip (drop 9 $ XMonad.workspaces conf) [xK_F1 .. xK_F9] -- mod4-[F1..F9], Switch to workspace N
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)] -- mod4-shift-[F1..F9], Move client to workspace N
    ]
    ++
    [ ((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_aring, xK_adiaeresis, xK_odiaeresis] [0..] -- win-{q,w,e}, Switch to physical/Xinerama screens 1, 2, or 3
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)] -- win-shift-{q,w,e}, Move client to screen 1, 2, or 3
    ]

-- Mouse bindings:
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w)) -- Set the window to floating mode and move by dragging
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster)) -- Raise the window to the top of the stack
    , ((modMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w)) -- Set the window to floating mode and resize by dragging
    , ((modMask, button4), (\_ -> prevWS)) -- Switch to previous workspace
    , ((modMask, button5), (\_ -> nextWS)) -- Switch to next workspace
    , ((modMask .|. shiftMask, button4), (\w -> focus w >> spawn "transset-df -p 0.1 --inc")) -- increase opacity
    , ((modMask .|. shiftMask, button5), (\w -> focus w >> spawn "transset-df -p 0.1 --dec")) -- decrease opacity
    ]

-- Window rules:
myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [ [className =? c --> doFloat | c <- myFloats]
    , [title =? t --> doFloat | t <- myOtherFloats]
    , [resource =? r --> doIgnore | r <- myIgnores]
    , [className =? "Thunderbird" --> doF (W.shift "3:mail")]
    , [className =? "Pidgin" --> doF (W.shift "4:im")]
    , [className =? "Eclipse" --> doF (W.shift "10")]
    , [isFullscreen --> doFullFloat]
    ]
    where
    myFloats = ["ekiga", "Gimp", "gimp", "MPlayer", "Nitrogen", "Transmission-gtk", "Xmessage", "xmms", "Steam"]
    myOtherFloats = ["Downloads", "Iceweasel Preferences", "Save As...", "Compose: (no subject)", "Icedove Preferences", "Tag and File Name scan", "Preferences...", "Confirm...", "gmpc - Configuration", "gmpc - song info", "Save Playlist", "GQview Preferences", "Inkscape Preferences (Shift+Ctrl+P)", "Select file to open", "Select file to save to", "Warning", "Closing Project - K3b", "Open Files - K3b", "Options - K3b", "Close Nicotine-Plus?", "Nicotine Settings", "OpenOffice.org 2.0", "Open", "Options - OpenOffice.org - User Data", "File Properties", "Preference", "Plugins:", "Preferences", "Firefox - Återställ föregående session", "Firefox-inställningar", "StepMania - pop * candy -", "Custom Smiley Manager", "Insticksmoduler", "Systemlogg", "Volbar", "Minecraft Launcher", "Minecraft"]
    myIgnores = ["stalonetray", "trayer"]

-- dynamicLog pretty printer for dzen:
myLogHook h home = defaultPP
    { ppCurrent = wrap ("^bg(#444444)^p(2)^fg(#00aaff)^i(" ++ home ++ "/.dzen/plus.xbm)^fg(#ffffff)") "^p(2)^fg()^bg()" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId
    , ppVisible = wrap "^bg(#444444)^fg(#aaaaaa)^p(2)" "^p(2)^fg()^bg()"
    , ppHidden = wrap ("^fg(#ffffff)^p(2)^i(" ++ home ++ "/.dzen/plus.xbm)^fg()") "^p(2)^fg()" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId
    , ppHiddenNoWindows = id . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId
    , ppSep = " ^fg(" ++ mySeparatorColor ++ ")::^fg() "
    , ppWsSep = " "
    , ppLayout = dzenColor "#ffffff" "" .
        (\x -> case x of
        "Hinted Tall" -> "^i(" ++ home ++ "/.dzen/tall.xbm)"
        "Hinted Wide" -> "^i(" ++ home ++ "/.dzen/mtall.xbm)"
        "Hinted Full" -> "^i(" ++ home ++ "/.dzen/full.xbm)"
        "Tall" -> "^i(" ++ home ++ "/.dzen/tall.xbm)"
        "Wide" -> "^i(" ++ home ++ "/.dzen/mtall.xbm)"
        "Full" -> "^i(" ++ home ++ "/.dzen/full.xbm)"
        "ThreeCol" -> "^i(" ++ home ++ "/.dzen/threecol.xbm)"
        _ -> ""
        )
    , ppTitle = dzenColor "#ffffff" "" . wrap "< " " >"
    , ppOutput = System.IO.UTF8.hPutStrLn h
    }


