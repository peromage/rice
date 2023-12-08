#!/usr/bin/env bash

## Set Linux system time to local time instead of UTC. Useful when dual-booting
## with Windows.
sudo timedatectl set-local-rtc true
sudo hwclock --systohc
timedatectl show
