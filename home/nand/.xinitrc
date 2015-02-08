#!/bin/sh

# Fix keyboard repeat rate
xset r rate 300 40

# Set pointer speed
xset m 1 0
#xinput --set-ptr-feedback 9 0 120 100

# Try to disable screensavers / monitor blanking
setterm -blank 0 -powersave off -powerdown 0
xset s off
xset +fp /usr/share/fonts/terminus/

# Load the correct modes
xrandr --output DFP1 --mode 4096x2160 --primary --output DFP7 --mode 1920x1200 --rotate left --pos 4096x240

# Load the correct keyboard map
xmodmap ~/.Xmodmap

# Load the correct color management profile
argyll-dispwin -I -d1 ~/icc/main.icc &
argyll-dispwin -I -d2 ~/icc/alt.icc &

# Fix the fan speed
aticonfig --pplib-cmd "set fanspeed 0 18"

# Automatically update the mail cache on new emails
#while inotifywait -e CREATE ./new/; do notmuch new & done &
mailsync

# Attempt to start the autoplay script
# autoplay info >/dev/null &

# Daemonize urxvt
urxvtd -q -o -f

# Hide mouse cursor when not in use
unclutter -noevents -idle 2 -jitter 1 -root >/dev/null 2>&1 &
swarp 0 0 &

# Start gpg-agent
eval $(gpg-agent --daemon)

# Start the window manager
exec xmonad
