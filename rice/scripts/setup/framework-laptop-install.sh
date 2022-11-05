#!/bin/bash

### This script is for Framework laptop specifically.
## Collocted from https://wiki.archlinux.org/title/Framework_Laptop

echo_green() {
    echo -e "\e[34;1m$@\e[0m"
}

sudo pacman -Syy

### Ambient light sensor
echo_green "[ Ambient light sensor ]"
sudo pacman -S iio-sensor-proxy

### Finger print
echo_green "[ Finger print ]
To enroll finger print, run 'fprintd-enroll'
"
sudo pacman -S fprintd

### Bluetooth
echo_green "[ Bluetooth ]"
sudo pacman -S bluez bluez-utils
sudo systemctl enable --now bluetooth

### Touchpad two/three finger clicks
echo_green "[ Touchpad two/three finger clicks ]
1. Get touchpad device id
  $ xinput
2. Add to .xinitrc
  $ xinput set-prop <device> 'libinput Click Method Enabled' 0 1
"

### Brightness buttons
echo_green "[ Fix brightness buttons ]
Add to kernel parameters:
  module_blacklist=hid_sensor_hub
"

### Suspend
echo_green "[ Better power saving on suspend ]
Add to kernel parameters:
  mem_sleep_default=deep
  nvme.noacpi=1
"
