#!/bin/bash
##
## This script must be executed in an archiso environment
##
## Notice: Beforing running this script, disks should be partitioned and mounted
## under /mnt (default)
##

## Some config variables #######################################################
## Root
MY_ARCH_ROOT=/mnt

## User information
MY_NAME=fang
MY_UID=1234
MY_HOSTNAME=saffyyre

## From /usr/share/zoneinfo
MY_TIMEZONE=America/Detroit

## Swap file. Set a value that is greater than 0 to enable.
MY_SWAPFILE_SIZE_MB=0

## Arch core packages
PACKAGE_BASE=(
    base
    linux
    linux-firmware
    intel-ucode
)

## System utility packages
PACKAGE_SYSTEM=(
    ## Boot loader
    grub
    grub-btrfs

    ## File system
    btrfs-progs
    ntfs-3g

    ## Shell
    bash
    bash-completion

    ## System tools
    sudo
    man
    man-db
    man-pages
    rsync

    ## System services
    iw
    iwd
    efibootmgr
    pulseaudio
    pulseaudio-alsa
    pavucontrol
    openssh
)

## Desktop environment packages
PACKAGE_DESKTOP=(
    xorg
    xfce4
    xfce4-goodies
)

## Development packages
PACKAGE_DEVEL=(
    base-devel
    git
    make
    cmake
    vim
    emacs
    tmux
)

## Font packages
PACKAGE_FONT=(
    adobe-source-han-sans-cn-fonts
    adobe-source-han-serif-cn-fonts
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    noto-fonts-extra
    otf-cascadia-code
    ttc-iosevka
)

## Some random packages
PACKAGE_CUSTOM=(
    firefox
    fcitx-im
    fcitx-configtool
    fcitx-cloudpinyin
    fcitx-sunpinyin
    pass
    xclip
)

## Helper functions ############################################################
chrootdo() {
    arch-chroot $MY_ARCH_ROOT $@
}

stdout_color_reset() {
    echo -ne '\e[0m'
}

stdout_color_bright_white() {
    echo -ne "\e[37;1m"
}

stdout_color_bright_red() {
    echo -ne '\e[31;1m'
}

logi() {
    stdout_color_bright_white
    echo "[ INFO ]" "$@"
    stdout_color_reset
}

loge() {
    stdout_color_bright_red
    echo "[ ERROR ]" "$@"
    stdout_color_reset
}

##
## Steps below follow Arch wiki: https://wiki.archlinux.org/title/installation_guide
##

## Install packages ############################################################
logi "Installing Arch Linux base system..."
pacstrap -KG $MY_ARCH_ROOT ${PACKAGE_BASE[@]} ${PACKAGE_SYSTEM[@]} ${PACKAGE_DESKTOP[@]} ${PACKAGE_DEVEL[@]} ${PACKAGE_FONT[@]} ${PACKAGE_CUSTOM[@]}

## Configure the system ########################################################
## Time zone ###################################################################
logi "Configuring time zone..."
chrootdo ln -sf /usr/share/zoneinfo/$MY_TIMEZONE /etc/localtime
chrootdo hwclock --systohc
#chrootdo timedatectl set-ntp true

## Localization ################################################################
logi "Configuring locale..."
echo en_US.UTF-8 UTF-8 >> $MY_ARCH_ROOT/etc/locale.gen
echo zh_CN.UTF-8 UTF-8 >> $MY_ARCH_ROOT/etc/locale.gen
chrootdo locale-gen
echo LANG=en_US.UTF-8 >> $MY_ARCH_ROOT/etc/locale.conf

## Network configuration #######################################################
logi "Configuring hostname..."
echo $MY_HOSTNAME > $MY_ARCH_ROOT/etc/hostname

## Login #######################################################################
logi "Configuring logins..."
logi "Setting root password..."
chrootdo passwd root
logi "Adding user $MY_NAME..."
chrootdo useradd -m -s /bin/bash -u $MY_UID $MY_NAME
chrootdo usermod -aG wheel $MY_NAME
chrootdo passwd $MY_NAME
echo "$MY_NAME ALL=(ALL:ALL) ALL" > $MY_ARCH_ROOT/etc/sudoers.d/$MY_NAME

## Services ####################################################################
logi "Configuring system services..."
chrootdo systemctl enable iwd
chrootdo systemctl enable systemd-resolved

## System Boot #################################################################
## Fstab #######################################################################
logi "Generating fstab..."
genfstab -U $MY_ARCH_ROOT >> $MY_ARCH_ROOT/etc/fstab

## Bootloader ##################################################################
logi "Configuring bootloader..."
chrootdo mkinitcpio -P
chrootdo grub-install --efi-directory=/boot --boot-directory=/boot --target=x86_64-efi --bootloader-id=$MY_HOSTNAME
chrootdo grub-mkconfig -o /boot/grub/grub.cfg

## Swap file ###################################################################
if [[ $MY_SWAPFILE_SIZE_MB -gt 0 ]]; then
    logi "Generating swapfile..."
    dd if=/dev/zero of=$MY_ARCH_ROOT/swapfile bs=1M count=$MY_SWAPFILE_SIZE_MB status=progress
    chmod 600 $MY_ARCH_ROOT/swapfile
    mkswap $MY_ARCH_ROOT/swapfile
    swapon $MY_ARCH_ROOT/swapfile
else
    logi "Swap file disabled"
fi

logi "Arch installation completed!"
