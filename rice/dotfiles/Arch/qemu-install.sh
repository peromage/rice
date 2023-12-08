#!/usr/bin/env bash

## QEMU + KVM

PACKAGES="
qemu
libvirt
virt-manager
dnsmasq
dmidecode
ovmf
"

sudo pacman -Sy
sudo pacman -S $PACKAGES
sudo usermod -aG libvirt $(id -un)
sudo systemctl enable libvirtd

echo "## In order to take the installation effective, you have to completely log out or restart the system!"
