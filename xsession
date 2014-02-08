xrandr --output VBOX0 --mode 1920x1080 --right-of VBOX1
xrandr --output VBOX1 --mode 1280x800 --left-of VBOX0
exec /usr/local/bin/xmonad
