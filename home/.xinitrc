#!/bin/bash

# set the background
feh --bg-scale ~/pic/ch.jpg
# set the cursor
xsetroot -cursor_name left_ptr
# start xmonad
xscreensaver -no-splash &
exec xmonad
