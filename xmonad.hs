import Control.Exception (SomeException, catch)
import Graphics.X11 (Screen, defaultScreenOfDisplay, heightOfScreen,
                     openDisplay, widthOfScreen)
import System.Environment (getEnv)
import XMonad.Core (catchIO, spawn)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Hooks.UrgencyHook (DzenUrgencyHook (args), dzenUrgencyHook,
                                 withUrgencyHook)
import XMonad.Layout ((|||), Full (Full))
import XMonad.Layout.HintedTile (Alignment (TopLeft), HintedTile (HintedTile),
                                 Orientation (Tall, Wide))
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.ResizableTile (ResizableTall (ResizableTall))
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeCol))
import XMonad.Operations (restart)

my_term     = "urxvt"

my_norm_bg      = "#222526"
my_norm_fg      = "#c4f0ff"
my_focus_bg     = "#222526"
my_focus_fg     = "#c4f0ff"
my_norm_bord    = my_norm_bg
my_focus_bord   = my_norm_fg
my_urgent_bg    = my_norm_bg
my_urgent_fg    = "#ff6633"
my_separator    = "#444444"

my_font         = "-misc-fixed"

my_urgency_hook h w = withUrgencyHook dzenUrgencyHook
    { args = [ "-x", "0", "-y", show (h - 16), "-h", "16", "-w" , show w
             , "-ta", "r", "-expand", "l", "-bg", my_norm_bg , "-fg"
             , my_norm_fg
             ]
    }

startup home = spawn "xset -b b off" >> spawn "xrdb -merge ~/.Xresources"

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
