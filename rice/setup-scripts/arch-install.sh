#!/bin/bash

##
## This script must be executed in an archiso environment
##
## Notice: Beforing running this script, disks should be partitioned and mounted
## under /mnt (default)
##
## Any customization should go to the end of this file.
##

################################################################################
### Installation Customization
################################################################################
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
MY_PACKAGE_CONFIGURE_FUNCTION_PREFIX="configure_"
MY_PACKAGE_REGEX="([^=]+)=(\d+)"
MY_PACKAGES=(
    ## Arch base packages
    base=1
    linux=1
    linux-firmware
    intel-ucode

    ## Pacman
    reflector

    ## Boot loader
    efibootmgr
    #grub=1
    #grub-btrfs
    tpm2-tss

    ## File system
    btrfs-progs
    ntfs-3g
    exfatprogs

    ## Auto-mounting
    gvfs
    gvfs-mtp
    gvfs-nfs
    gvfs-smb
    udisks2

    ## Power
    cpupower=1
    thermald=1
    ## TLP and power-profiles-daemon conflicts with each other.
    ## PPD is easier for daily use without involving too much tweak and it's
    ## supported by KDE and GNOME out of the box.
    #tlp=1
    power-profiles-daemon
    htop
    s-tui
    powertop

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
    aspell
    aspell-en

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

    ## SDK
    python
    python-gobject
    dotnet-sdk

    ## Fonts
    adobe-source-han-sans-cn-fonts
    adobe-source-han-serif-cn-fonts
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    noto-fonts-extra
    otf-cascadia-code
    ttc-iosevka

    ## Flatpak
    flatpak

    ## Desktop
    xorg
    xorg-xinit
    plasma-meta
    kde-system-meta
    kde-utilities-meta

    ## GUI applications
    firefox

    ## Input methods
    fcitx-im
    fcitx-configtool
    fcitx-cloudpinyin
    fcitx-sunpinyin

    ## QEMU + KVM
    qemu-full
    libvirt=1
    virt-manager
    edk2-ovmf
    dnsmasq
    iptables-nft
    swtpm
)

## Package configurations
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

    ## Some nice settings for pacman
    cat <<EOF >> $MY_ARCH_ROOT/etc/pacman.conf
[options]
ILoveCandy
Color
EOF
}

configure_linux() {
    ## RAM disk
    logi "Generating ramdisk"
    chrootdo mkinitcpio -P

    ## Boot manager
    logi "Installing boot manager"
    chrootdo bootctl install
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

configure_thermald() {
    logi "Enabling service: thermald"
    chrootdo systemctl enable thermald.service
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

configure_libvirt() {
    logi "Configuring libvirt for $MY_NAME"

    local libvirt_group="libvirt"
    local libvirtd_conf="/etc/libvirt/libvirtd.conf"
    logi "Enabling authentication for group $libvirt_group"
    chrootdo perl -i -pe "print \"\$1 = ${libvirt_group}\n\" if /^#(unix_sock_group) *=/" $libvirtd_conf

    local qemu_conf="/etc/libvirt/qemu.conf"
    logi "Adding user and group to $qemu_conf"
    chrootdo perl -i -pe "print \"\$1 = ${MY_NAME}\n\" if /^#(user) *=/" $qemu_conf
    chrootdo perl -i -pe "print \"\$1 = ${MY_NAME}\n\" if /^#(group) *=/" $qemu_conf

    logi "Adding user $MY_NAME to group ${libvirt_group}"
    chrootdo usermod -aG $libvirt_group $MY_NAME

    logi "Enabling service: libvirtd"
    chrootdo systemctl enable libvirtd.service
}

################################################################################
### Helper functions
################################################################################
logi() {
    echo -e "\e[34;1m[ INFO ] $@\e[0m"
}

loge() {
    echo -e "\e[31;1m[ ERROR ] $@\e[0m"
}

chrootdo() {
    arch-chroot $MY_ARCH_ROOT "$@"
}

## Get package names without trailing "=1"
generate_package_list() {
    for package in "${MY_PACKAGES[@]}"; do
        echo $package | perl -pe "s/${MY_PACKAGE_REGEX}/\$1/g"
    done
}

## Get configuration functions of packages that have trailing "=1"
generate_package_configure_function_list() {
    for package in "${MY_PACKAGES[@]}"; do
        echo $package | perl -ne "print \"${MY_PACKAGE_CONFIGURE_FUNCTION_PREFIX}\$1\\n\" if /${MY_PACKAGE_REGEX}/"
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

################################################################################
### Script starts here
################################################################################

## Configure functions must be presented
if ! validate_functions $(generate_package_configure_function_list); then
    loge "One or more configuration functions missing. Aborting..."
    exit
fi

## Install packages
logi "Installing Arch Linux base system"
pacstrap -KG $MY_ARCH_ROOT $(generate_package_list)

## Configure packages
for func in $(generate_package_configure_function_list); do
    eval $func
done

## Mount
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

logi "Arch installation completed!"
