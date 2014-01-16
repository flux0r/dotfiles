#!/bin/sh
# xrandr -s 1280x800
xrdb -load $HOME/.Xresources
xmonad >$HOME/.xmonad/log.txt 2>&1
# xrandr -s 1280x800
# xsetroot -cursor_name left_ptr
# xset b off
# xset s off
# xset -dpms
# xsetbg -onroot -fork $HOME/wp/space-wallpaper.0.jpg
# xcompmgr -CcfF -I-0.15 -O-.03 -D4 -t-5 -l-5 -r4
# urxvt &
# urxvt &
# urxvt &
