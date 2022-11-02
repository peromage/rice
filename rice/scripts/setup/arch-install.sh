#!/bin/bash
##
## This script must be executed in an archiso environment
##
## Notice: Beforing running this script, disks should be partitioned and mounted
## under /mnt (default)
##
## Any customization should go to the end of this file.
##
source ${BASH_SOURCE%/*}/../source/logger.sh

### Some config variables ######################################################
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

## System packages
## Declaring a package with "PACKAGE=1" to enable post-installation configuration.
## If that is the case, a function named "configure_PACKAGE" must be defined.
## The configuration function will be called after all the packages in this list
## are installed. Calling order dependends on the package declaration order.
## If the value is missing or anything other than 1 to install the package only.
SYSTEM_PACKAGE_CONFIGURE_PREFIX="configure_"
SYSTEM_PACKAGES=(
    ## Arch base packages
    base=1
    linux=1
    linux-firmware
    intel-ucode

    ## Pacman
    reflector

    ## Boot loader
    efibootmgr
    grub=1
    grub-btrfs

    ## File system
    btrfs-progs
    ntfs-3g

    ## Power
    cpupower=1
    tlp=1
    htop
    s-tui

    ## Audio
    pipewire
    pipewire-alsa
    pipewire-pulse

    ## Network
    networkmanager=1
    nm-connection-editor
    network-manager-applet
    openssh=1

    ## Tools
    sudo=1
    man
    man-db
    man-pages
    rsync
    xclip
    zip
    unzip
    pass
    ispell

    ## Terminal and shell
    bash
    bash-completion
    alacritty

    ## Development
    base-devel
    git
    make
    cmake
    vim
    emacs
    tmux

    ## Fonts
    adobe-source-han-sans-cn-fonts
    adobe-source-han-serif-cn-fonts
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    noto-fonts-extra
    otf-cascadia-code
    ttc-iosevka

    ## Desktop
    xorg
    xfce4
    xfce4-goodies

    ## GUI applications
    firefox

    ## Input methods
    fcitx-im
    fcitx-configtool
    fcitx-cloudpinyin
    fcitx-sunpinyin
)

### Package configurations #####################################################
configure_base() {
    logi "Configuring Arch base system"
    ## Configure the system
    ## Steps below follow Arch wiki: https://wiki.archlinux.org/title/installation_guide

    ## Time zone
    logi "Time zone"
    chrootdo ln -sf /usr/share/zoneinfo/$MY_TIMEZONE /etc/localtime
    chrootdo hwclock --systohc
    #chrootdo timedatectl set-ntp true

    ## Localization
    logi "Locale"
    chrootdo "echo en_US.UTF-8 UTF-8 >> /etc/locale.gen"
    chrootdo "echo zh_CN.UTF-8 UTF-8 >> /etc/locale.gen"
    chrootdo locale-gen
    chrootdo "echo LANG=en_US.UTF-8 >> /etc/locale.conf"

    ## Network configuration
    logi "Hostname: $MY_HOSTNAME"
    chrootdo "echo $MY_HOSTNAME > /etc/hostname"

    ## Login
    logi "Logins"
    logi "Setting root password"
    chrootdo passwd root
    logi "Adding user: $MY_NAME"
    chrootdo useradd -m -s /bin/bash -u $MY_UID $MY_NAME
    chrootdo usermod -aG wheel $MY_NAME
    chrootdo passwd $MY_NAME
}

configure_linux() {
    logi "Generating ramdisk"
    chrootdo mkinitcpio -P
}

configure_grub() {
    logi "Configuring GRUB"
    chrootdo grub-install --efi-directory=/boot --boot-directory=/boot --target=x86_64-efi --bootloader-id=$MY_HOSTNAME
    chrootdo grub-mkconfig -o /boot/grub/grub.cfg
}

configure_cpupower() {
    logi "Enabling service: cpupower"
    chrootdo systemctl enable cpupower.service
}

configure_tlp() {
    logi "Enabling service: tlp"
    chrootdo systemctl enable tlp.service
}

configure_networkmanager() {
    logi "Enabling service: NetworkManager"
    chrootdo systemctl enable NetworkManager.service

    logi "Enabling service: systemd-resolved"
    chrootdo systemctl enable systemd-resolved.service
}

configure_openssh() {
    logi "Enabling service: sshd"
    chrootdo systemctl enable sshd.service
}

configure_sudo() {
    logi "Making $MY_NAME as a sudoer"
    chrootdo "echo '$MY_NAME ALL=(ALL:ALL) ALL' > /etc/sudoers.d/$MY_NAME"
}

### Helper functions ###########################################################
chrootdo() {
    arch-chroot $MY_ARCH_ROOT "$@"
}

## Get package names without trailing "=*"
system_package_normalized_list() {
    for package in "${SYSTEM_PACKAGES[@]}"; do
        echo $package | perl -pe 's/(.*)(=.*)/$1/g'
    done
}

## Get configuration functions of packages that have trailing "=1"
system_package_configure_list() {
    for package in "${SYSTEM_PACKAGES[@]}"; do
        echo $package | perl -ne "print \"${SYSTEM_PACKAGE_CONFIGURE_PREFIX}\$1\\n\" if /(.*)=.*/"
    done
}

## Check if functions exit and return non-zero if any one of them not found
validate_functions() {
    local bad=0
    for func in $@; do
        if ! declare -F | grep -Pq "(?<=declare -f )$func\$"; then
            loge "Missing $func"
            bad=1
        fi
    done
    return $bad
}

### Sanity checks ##############################################################
if ! validate_functions $(system_package_configure_list); then
    loge "One or more configuration functions missing. Aborting..."
    exit
fi

### Install packages ###########################################################
logi "Installing Arch Linux base system"
pacstrap -KG $MY_ARCH_ROOT $(system_package_normalized_list)

### Configure packages #########################################################
for func in $(system_package_configure_list); do
    eval $func
done

### Mount ######################################################################
logi "Mouting partitions"
## Swap file
if [[ $MY_SWAPFILE_SIZE_MB -gt 0 ]]; then
    logi "Generating swapfile"
    dd if=/dev/zero of=$MY_ARCH_ROOT/swapfile bs=1M count=$MY_SWAPFILE_SIZE_MB status=progress
    chmod 600 $MY_ARCH_ROOT/swapfile
    mkswap $MY_ARCH_ROOT/swapfile
    swapon $MY_ARCH_ROOT/swapfile
else
    logi "Swap file disabled"
fi
## Fstab
logi "Generating fstab"
genfstab -U $MY_ARCH_ROOT >> $MY_ARCH_ROOT/etc/fstab

### Some nice settings #########################################################
cat <<EOF >> $MY_ARCH_ROOT/etc/pacman.conf
[options]
ILoveCandy
Color
EOF

### Custom installation process ################################################
logi "Starting custom installation"


logi "Arch installation completed!"
