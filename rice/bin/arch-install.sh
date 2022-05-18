#!/bin/sh

## This script must be executed in a archiso environment
## Notice: Beforing running this script, disks should be partitioned and mounted
## under /mnt (default)

## Some config variables
MY_ARCH_ROOT=/mnt
MY_NAME=fang
MY_UID=1234
MY_HOSTNAME=ARCHMAGE
## From timedatectl list-timezones
MY_TIMEZONE=America/Detroit
MY_SWAPFILE_SIZE_MB=8192

## Package lists

BASE_PACKAGES="
base
linux
linux-firmware
intel-ucode
"

SYS_PACKAGES="
sudo
iw
iwd
man
grub
efibootmgr
pulseaudio
pulseaudio-alsa
pavucontrol
ntfs-3g
"

DE_PACKAGES="
xorg
xfce4
xfce4-goodies
"

DEV_PACKAGES="
cmake
base-devel
"

FONT_PACKAGES="
adobe-source-han-sans-cn-fonts
adobe-source-han-serif-cn-fonts
noto-fonts
noto-fonts-cjk
noto-fonts-emoji
noto-fonts-extra
otf-cascadia-code
ttc-iosevka
"

MY_PACKAGES="
git
vim
emacs
tmux
openssh
bash
bash-completion
firefox
fcitx-im
fcitx-configtool
fcitx-cloudpinyin
fcitx-sunpinyin
rsync
pass
xclip
"

chdo() {
    arch-chroot $MY_ARCH_ROOT $@
}

echo "## Installing Arch Linux base system..."
pacstrap $MY_ARCH_ROOT $BASE_PACKAGES

## Begin arch-chroot
echo "## Now entering arch-chroot..."

echo "## Syncing package..."
chdo pacman -Syy
chdo pacman -S $SYS_PACKAGES $DE_PACKAGES $DEV_PACKAGES $FONT_PACKAGES $MY_PACKAGES

echo "## Configuring date and time..."
chdo hwclock --systohc
chdo timedatectl set-timezone $MY_TIMEZONE
chdo timedatectl set-ntp true

echo "## Configuring locale..."
echo en_US.UTF-8 UTF-8 >> $MY_ARCH_ROOT/etc/locale.gen
echo zh_CN.UTF-8 UTF-8 >> $MY_ARCH_ROOT/etc/locale.gen
chdo locale-gen

echo "## Configuring hostname..."
echo $MY_HOSTNAME > $MY_ARCH_ROOT/etc/hostname

echo "## Configuring logins..."
echo "## Updating password for root..."
chdo passwd root
echo "## Adding user $MY_NAME..."
chdo useradd -m -s /bin/bash -u $MY_UID $MY_NAME
chdo usermod -aG wheel $MY_NAME
chdo passwd $MY_NAME
echo "$MY_NAME ALL=(ALL:ALL) ALL" > $MY_ARCH_ROOT/etc/sudoers.d/$MY_NAME

echo "## Configuring system services..."
chdo systemctl enable iwd
chdo systemctl enable systemd-resolved

echo "## Configuring bootloader..."
chdo mkinitcpio -P
chdo grub-install --efi-directory=/boot --boot-directory=/boot --target=x86_64-efi --bootloader-id=$MY_HOSTNAME
chdo grub-mkconfig -o /boot/grub/grub.cfg

echo "## arch-chroot done!"
## End arch-chroot

echo "Generating swapfile..."
dd if=/dev/zero of=$MY_ARCH_ROOT/swapfile bs=1M count=$MY_SWAPFILE_SIZE_MB status=progress
chmod 600 $MY_ARCH_ROOT/swapfile
mkswap $MY_ARCH_ROOT/swapfile
swapon $MY_ARCH_ROOT/swapfile

echo "Generating fstab..."
genfstab -U $MY_ARCH_ROOT >> $MY_ARCH_ROOT/etc/fstab
