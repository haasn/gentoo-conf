#!/bin/sh

# Fix keyboard repeat rate
xset r rate 300 40

# Set pointer speed
xset m 0 0
xinput set-prop 'Kingsis Peripherals ZOWIE Gaming mouse' 'libinput Accel Profile Enabled' 0, 1

# Start up evrouter
sudo evrouter /dev/input/by-id/usb-Kingsis_Peripherals_ZOWIE_Gaming_mouse-event-mouse -c ~/.evrouterrc &>/dev/null

# Try to disable screensavers / monitor blanking
setterm -blank 0 -powersave off -powerdown 0
xset s off
xset +fp /usr/share/fonts/terminus/

# Load the correct resolution
#xrandr --output DP-0 --mode 4096x2160 --primary --output HDMI-0 --off
xrandr --dpi 150
xrandr --output DisplayPort-0 --set TearFree on

# Load the correct keyboard map
xmodmap ~/.Xmodmap

# Load the correct color management profile
argyll-dispwin -I -d1 ~/icc/main.icc &
#argyll-dispwin -I -d1 ~/icc/alt.icc &

# Fix the fan speed
#aticonfig --pplib-cmd "set fanspeed 0 18"

# Fix the GPU speed
#nvidia-settings -a '[gpu:0]/GPUGraphicsClockOffset[3]=100'
#nvidia-settings -a '[gpu:0]/GPUPowerMizerMode=1'

# Daemonize udiskie
#udiskie -N &>/dev/null &

# Daemonize urxvt
export RXVT_SOCKET="$HOME/.urxvt/urxvtd$DISPLAY"
urxvtd -q -o -f

# Hide mouse cursor when not in use
unclutter -noevents -idle 2 -jitter 1 -root &>/dev/null &
#easystroke &

# Start MPD and autoplay
systemctl --user start mpd
autoplay info &

# Start gpg-agent
eval $(gpg-agent --daemon)

# Start SCIM
export XMODIFIERS=@im=SCIM
export GTK_IM_MODULE="xim"
export QT_IM_MODULE="scim"
scim -d

# Start the window manager
exec xmonad
