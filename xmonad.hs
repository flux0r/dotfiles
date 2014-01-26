import Control.Exception (SomeException, catch)
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt (CInt), CSize (CSize))
import Foreign.Marshal.Array (allocaArray0)
import Graphics.X11 (Screen, defaultScreenOfDisplay, heightOfScreen,
                     openDisplay, widthOfScreen)
import System.Directory (doesFileExist, removeFile)
import System.Environment (getEnv)
import System.IO.UTF8 (hPutStrLn)
import XMonad ((<+>), xmonad)
import XMonad.Actions.SpawnOn (manageSpawn)
import XMonad.Config (defaultConfig)
import XMonad.Core (XConfig (..), borderWidth, catchIO, handleEventHook,
                    spawn)
import XMonad.Hooks.DynamicLog (PP (..), defaultPP, dzenColor,
                                dynamicLogWithPP, wrap)
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks)
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import XMonad.Hooks.UrgencyHook (DzenUrgencyHook (args), dzenUrgencyHook,
                                 withUrgencyHook)
import XMonad.Layout ((|||), Full (Full))
import XMonad.Layout.HintedTile (Alignment (TopLeft), HintedTile (HintedTile),
                                 Orientation (Tall, Wide))
import XMonad.Layout.WindowNavigation (windowNavigation)
import XMonad.ManageHook ((-->), (=?), className, composeAll, doFloat,
                          doIgnore, resource, title)
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.ResizableTile (ResizableTall (ResizableTall))
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeCol))
import XMonad.Operations (restart)
import XMonad.Prompt (XPPosition (Bottom), XPConfig (..), defaultXPConfig)
import XMonad.Util.Run (spawnPipe)

foreign import ccall unsafe "gethostname" gethostname
    :: CString -> CSize -> IO CInt

get_host_name :: IO String
get_host_name = let sz = 256 in
    allocaArray0 sz (\x ->
    throwErrnoIfMinus1_ "get_host_name" (gethostname x (fromIntegral sz)) >>
    peekCString x)

my_term     = "urxvt"

my_norm_bg      = "#222526"
my_norm_fg      = "#c4f0ff"
my_focus_bg     = "#222526"
my_focus_fg     = "#c4f0ff"
my_norm_bord    = my_norm_bg
my_focus_bord   = my_norm_fg
my_urgent_bg    = my_norm_bg
my_urgent_fg    = "#61d8ff"
my_separator    = "#444444"

my_font         = "-misc-fixed"

main = do
    home <- catch (getEnv "HOME") handler
    d <- catch (getEnv "DISPLAY") handler
    dpy <- openDisplay d
    let scr = defaultScreenOfDisplay dpy
    let my_width = read (show (widthOfScreen scr)) :: Int
    let my_height = read (show (heightOfScreen scr)) :: Int
    let status_bar = "dzen2 -x '0' -y '0' -h '16' -w " ++
                     show (my_width - 680) ++ " -ta 'l' -bg '" ++
                     my_norm_bg ++ "' -fg '" ++ my_norm_fg ++
                     "' -fn 'fixed' -e 'onstart=lower'"
    let top_bar     = "conky -c ~/.conkytoprc | dzen2 -x " ++
                      show (my_width - 680) ++
                      " -y '0' -h '16' -w '680' -ta 'r' -bg '" ++
                      my_norm_bg ++
                      " -fg '" ++
                      my_norm_fg ++
                      "' -fn 'fixed' -e 'onstart=lower'"
    let bottom_bar = "conky -c ~/.conkybottomrc | dzen2 -x '0' -y " ++
                     show (my_height - 16) ++
                     " -h '16' -w " ++
                     show (my_width - 128) ++
                     " -ta 'l' -bg '" ++
                     my_norm_bg ++
                     "' -fg '" ++
                     my_norm_fg ++
                     "' -fn '" ++
                     my_font ++
                     "' -e 'onstart=lower'"
    din_status  <- spawnPipe status_bar
    din_top     <- spawnPipe top_bar
    din_bottom  <- spawnPipe bottom_bar
    spawn ("stalonetray -bg '" ++ my_norm_bg ++ "'")
    spawn ("xsetroot -solid '" ++ my_norm_bg ++ "'")
    spawn "xsetroot -cursor_name left_ptr"
    host <- get_host_name
    exists <- doesFileExist "/tmp/xmonad_restart"
    if exists
        then removeFile "/tmp/xmonad_restart"
        else startup home host
    xmonad (my_urgency_hook my_height my_width (ewmh defaultConfig
        { normalBorderColor     = my_norm_bord
        , focusedBorderColor    = my_focus_bord
        , terminal              = my_term
        , layoutHook            = windowNavigation my_layout
        , manageHook            = manageSpawn <+> my_manage_hook <+>
                                  manageDocks
        , handleEventHook       = handleEventHook defaultConfig <+>
                                  fullscreenEventHook
        , workspaces            = ["1:term", "2:www", "3:mail", "4:comm"] ++
                                  map show [5..18 :: Int]
        , borderWidth           = 1
        , logHook               = dynamicLogWithPP
                                    (my_log_hook din_status home)
        , focusFollowsMouse     = True
        }))
  where
    handler :: SomeException -> IO [b]
    handler = (const . return) []


my_urgency_hook h w = withUrgencyHook dzenUrgencyHook
    { args = [ "-x", "0", "-y", show (h - 16), "-h", "16", "-w" , show w
             , "-ta", "r", "-expand", "l", "-bg", my_norm_bg , "-fg"
             , my_norm_fg
             ]
    }

startup home _ = spawn "xset -b b off" >> spawn "xrdb -merge ~/.Xresources"

restart_xmonad = spawn "killall conky dzen2 stalonetray" >>
    catchIO (writeFile "/tmp/xmonad_restart" "true") >>
    restart "xmonad" True

my_width, my_height :: Screen -> Int
my_width = read . show . widthOfScreen 
my_height = read . show . heightOfScreen

restart_dzen :: IO ()
restart_dzen = spawn "killall conky" >>
    catch (getEnv "DISPLAY") handler >>=
    openDisplay >>= (\d ->
    let scr = defaultScreenOfDisplay d in
        spawn (top_bar scr) >>
        spawn (bottom_bar scr))
  where
    handler :: SomeException -> IO [b]
    handler         = (const . return) []
    top_x s         = show (my_width s - 680)
    bottom_y s      = show (my_height s - 16)
    bottom_w s      = show (my_width s - 128)
    top_bar s       = "conky -c ~/.conkytoprc | dzen2 -x " ++
                      top_x s ++
                      " -y '0' -h '16' -w '680' -ta 'r' -bg '" ++
                      my_norm_bg ++
                      " -fg '" ++
                      my_norm_fg ++
                      "' -fn 'fixed' -e 'onstart=lower'"
    bottom_bar s   = "conky -c ~/.conkybottomrc | dzen2 -x '0' -y " ++
                     bottom_y s ++
                     " -h '16' -w " ++
                     bottom_w s ++
                     " -ta 'l' -bg '" ++
                     my_norm_bg ++
                     "' -fg '" ++
                     my_norm_fg ++
                     "' -fn '" ++
                     my_font ++
                     "' -e 'onstart=lower'"

my_layout = avoidStruts (smartBorders
    (onWorkspace "1:term" (hinted_tile Wide ||| noBorders Full)
    (onWorkspaces ["2:www", "3:mail"] (noBorders Full)
    (onWorkspace "4:comm" (noBorders (HintedTile
                                       nmaster delta (4/5) TopLeft Tall))
    (hinted_tile Tall ||| hinted_tile Wide ||| (noBorders Full) |||
        (ThreeCol 1 (3/100) (1/2)) ||| ResizableTall 1 (3/100) (1/2) [])))))
  where
    hinted_tile     = HintedTile nmaster delta ratio TopLeft
    nmaster         = 1
    ratio           = toRational (2/(1 + sqrt(5) :: Double))
    delta           = 3/100

my_xp_conf = defaultXPConfig
    { font              = "fixed"
    , bgColor           = my_norm_bg
    , fgColor           = my_norm_fg
    , fgHLight          = my_focus_fg
    , bgHLight          = my_focus_bg
    , borderColor       = my_norm_bord
    , promptBorderWidth = 1
    , position          = Bottom
    , height            = 16
    , historySize       = 100
    }

my_manage_hook = (composeAll . concat)
    [ [className =? c --> doFloat | c <- my_floats]
    , [title =? t --> doFloat | t <- my_other_floats]
    , [resource =? r --> doIgnore | r <- my_ignores]
    , [isFullscreen --> doFullFloat]
    ]
  where
    my_floats       = ["Gimp", "gimp", "Xmessage"]
    my_other_floats = ["Downloads", "Save As...", "Confirm...",
                       "Preferences...",
                       "Inkscape Preferences (Shift+Ctrl+P)",
                       "Select file to save to", "Warning", "Open",
                       "File Properties", "Preferences"]
    my_ignores      = ["stalonetray", "trayer"]

my_log_hook h home = defaultPP
    { ppCurrent         = wrap ("^bg(" ++ my_norm_bg ++ ")^p(2)^fg(" ++
                                my_norm_fg ++ ")^i(" ++ home ++
                                "/.dzen/plus.xbm)^fg(" ++ my_norm_fg ++ ")")
                               "^p(2)^fg()^bg()"
                               . ws_id
    , ppVisible         = wrap ("^bg(" ++ my_norm_bg ++ ")^fg(" ++
                                my_norm_fg ++ ")^p(2)")
                               "^p(2)^fg()^bg()"
    , ppHidden          = wrap ("^fg(" ++ my_norm_fg ++ ")^p(2)^i(" ++
                                home ++ "/.dzen/plus.xbm)^fg()")
                               "^p(2)^fg()"
                               . ws_id
    , ppHiddenNoWindows = id . ws_id
    , ppSep             = " ^fg(" ++ my_separator ++ ")::^fg() "
    , ppWsSep           = " "
    , ppLayout          = dzenColor my_norm_fg "" . lay
    , ppTitle           = dzenColor my_norm_fg "" . wrap "< " " >"
    , ppOutput          = hPutStrLn h
    }
  where
    ws_id x = if elem ':' x then drop 2 x else x
    lay x 
        | x == "Hinted Tall"    = lay_str "tall"
        | x == "Hinted Wide"    = lay_str "mtall"
        | x == "Hinted Full"    = lay_str "full"
        | x == "Tall"           = lay_str "tall"
        | x == "Wide"           = lay_str "mtall"
        | x == "Full"           = lay_str "full"
        | x == "ThreeCol"       = lay_str "threecol"
        | otherwise             = ""
    lay_prefix  = "^i(" ++ home ++ "/.dzen/"
    lay_suffix  = ".xbm)"
    lay_str x   = lay_prefix ++ x ++ lay_suffix 
