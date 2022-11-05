#!/bin/bash

### Post-installation for laptops.
## Run this script after booting into installed system.

laptop="$1" && shift

### Helper functions
echo_green() {
    echo -e "\e[34;1m$@\e[0m"
}

## Framework laptop fixs
## Collocted from https://wiki.archlinux.org/title/Framework_Laptop
fix_laptop_framework() {
    echo_green "[ Framework laptop fixs ]"
    ## Ambient light sensor
    echo_green "[ Ambient light sensor ]"
    sudo pacman -S iio-sensor-proxy

    ## Finger print
    echo_green "[ Finger print ]
To enroll finger print, run 'fprintd-enroll'
"
    sudo pacman -S fprintd

    ## Bluetooth
    echo_green "[ Bluetooth ]"
    sudo pacman -S bluez bluez-utils
    sudo systemctl enable --now bluetooth

    ## Touchpad two/three finger clicks
    echo_green "[ Touchpad two/three finger clicks ]
1. Get touchpad device id
  $ xinput
2. Add to .xinitrc
  $ xinput set-prop <device> 'libinput Click Method Enabled' 0 1
"

    ## Brightness buttons
    echo_green "[ Fix brightness buttons ]
Add to kernel parameters:
  module_blacklist=hid_sensor_hub
"

    ## Suspend
    echo_green "[ Better power saving on suspend ]
Add to kernel parameters:
  mem_sleep_default=deep
  nvme.noacpi=1
"
}

### Start script
sudo pacman -Syy

### TLP
cat <<EOF > /etc/tlp.d/my-power-plan.conf
TLP_DEFAULT_MODE=AC
TLP_PERSISTENT_DEFAULT=0

CPU_SCALING_GOVERNOR_ON_AC=performance
CPU_SCALING_GOVERNOR_ON_BAT=powersave

CPU_ENERGY_PERF_POLICY_ON_AC=balance_performance
CPU_ENERGY_PERF_POLICY_ON_BAT=balance_power

CPU_MIN_PERF_ON_AC=0
CPU_MAX_PERF_ON_AC=100
CPU_MIN_PERF_ON_BAT=0
CPU_MAX_PERF_ON_BAT=50

CPU_BOOST_ON_AC=1
CPU_BOOST_ON_BAT=0

CPU_HWP_DYN_BOOST_ON_AC=1
CPU_HWP_DYN_BOOST_ON_BAT=0

SCHED_POWERSAVE_ON_AC=0
SCHED_POWERSAVE_ON_BAT=1
EOF

### Fix specific laptop
case "$laptop" in
    framework)
        fix_laptop_framework
        ;;
    *)
        :;
        ;;
esac
