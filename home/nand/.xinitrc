#!/bin/sh

# Fix keyboard repeat rate
xset r rate 200 50

# Set pointer speed
xset m 1 0
#xinput --set-ptr-feedback 9 0 120 100

# Try to disable screensavers / monitor blanking
setterm -blank 0 -powersave off -powerdown 0
xset s off
xset +fp /usr/share/fonts/terminus/

# Load the correct resolution
xrandr --output DP-0 --mode 4096x2160 --primary --output DVI-I-1 --mode 1920x1200 --rotate left --pos 4096x120

# Load the correct keyboard map
xmodmap ~/.Xmodmap

# Load the correct color management profile
argyll-dispwin -I -d2 ~/icc/main.icc &
argyll-dispwin -I -d1 ~/icc/alt.icc &

# Fix the fan speed
#aticonfig --pplib-cmd "set fanspeed 0 18"

# Fix the GPU speed
nvidia-settings -a '[gpu:0]/GPUGraphicsClockOffset[3]=100'
nvidia-settings -a '[gpu:0]/GPUPowerMizerMode=1'

# Automatically update the mail cache on new emails
#while inotifywait -e CREATE ./new/; do notmuch new & done &
#mailsync

# Attempt to start the autoplay script
# autoplay info >/dev/null &

# Daemonize urxvt
export RXVT_SOCKET="$HOME/.urxvt/urxvtd$DISPLAY"
urxvtd -q -o -f

# Hide mouse cursor when not in use
unclutter -noevents -idle 2 -jitter 1 -root &>/dev/null &
swarp 0 0 &

# Start polling for mail
mswatch -c ~/.mswatch-nanovps &>/dev/null &
mswatch -c ~/.mswatch-uulm    &>/dev/null &

# Start gpg-agent
eval $(gpg-agent --daemon)

# Start the window manager
exec xmonad
