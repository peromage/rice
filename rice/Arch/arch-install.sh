#!/usr/bin/env bash

##
## This script must be executed in an archiso environment
##
## Notice: Beforing running this script, disks should be partitioned and mounted
## under /mnt/arch (default)
##
## Any customization should go to the end of this file.
##

################################################################################
### Installation Customization
################################################################################
## Root
MY_ARCH_ROOT=/mnt/arch

## User information
MY_NAME=fang
MY_UID=1234
MY_HOSTNAME=saffyyre

## From /usr/share/zoneinfo
MY_TIMEZONE=America/Detroit

## Swap file. Set the file path and a value that is greater than 0 to enable.
MY_SWAPFILE_PATH=
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
    dkms

    ## Pacman
    reflector

    ## Boot loader
    efibootmgr
    #grub=1
    #grub-btrfs
    tpm2-tss
    sbctl

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

    ## Devices
    cups
    cups-pdf

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
    neofetch
    usbutils

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
    ttf-liberation

    ## Flatpak
    flatpak

    ## Desktop Environment
    xorg
    xorg-xinit
    wayland
    wayland-utils
    wl-clipboard
    xorg-xwayland
    ## KDE
    plasma-meta
    kde-system-meta
    kde-utilities-meta
    kde-graphics-meta

    ## GUI applications
    firefox

    ## Input methods
    fcitx5-im
    fcitx5-rime
    rime-double-pinyin
    rime-emoji

    ## QEMU + KVM
    qemu-full
    libvirt=1
    virt-manager
    edk2-ovmf
    dnsmasq
    iptables-nft
    swtpm

    ## Laptop related
    iio-sensor-proxy
    fprintd
    bluez-utils=1
)

## Package configurations
configure_base() {
    logi "Configuring Arch base system"
    ## Configure the system
    ## Steps below follow Arch wiki: https://wiki.archlinux.org/title/installation_guide

    ## Time zone
    logi "Time zone"
    chrootdo <<EOF
ln -sf /usr/share/zoneinfo/$MY_TIMEZONE /etc/localtime
hwclock --systohc
#timedatectl set-ntp true
EOF

    ## Localization
    logi "Locale"
    chrootdo <<EOF
echo en_US.UTF-8 UTF-8 >> /etc/locale.gen
echo zh_CN.UTF-8 UTF-8 >> /etc/locale.gen
locale-gen
echo LANG=en_US.UTF-8 >> /etc/locale.conf
EOF

    ## Network configuration
    logi "Hostname: $MY_HOSTNAME"
    chrootdo <<EOF
echo $MY_HOSTNAME > /etc/hostname
EOF

    ## Login
    logi "Logins"
    logi "Updating root password"
    logw "Using default password \"root\".  Consider updating it after installation. "
    chrootdo <<EOF
echo "root:root" | chpasswd
EOF
    logi "Adding user $MY_NAME"
    logw "Using default password \"${MY_NAME}\".  Consider updating it after installation. "
    chrootdo <<EOF
useradd -m -s /bin/bash -u $MY_UID $MY_NAME
usermod -aG wheel $MY_NAME
echo "$MY_NAME:$MY_NAME" | chpasswd
EOF

    ## Some nice settings for pacman
    chrootdo <<EOF
cat <<INNER_EOF >> /etc/pacman.conf
[options]
ILoveCandy
Color
INNER_EOF
EOF
}

configure_linux() {
    ## RAM disk
    logi "Generating ramdisk"
    chrootdo <<EOF
mkinitcpio -P
EOF

    ## Boot manager
    logi "Installing boot manager"
    chrootdo <<EOF
bootctl install
EOF
}

configure_grub() {
    logi "Configuring GRUB"
    chrootdo <<EOF
grub-install --efi-directory=/boot --boot-directory=/boot --target=x86_64-efi --bootloader-id=$MY_HOSTNAME
grub-mkconfig -o /boot/grub/grub.cfg
EOF
}

configure_cpupower() {
    logi "Enabling service: cpupower"
    chrootdo <<EOF
systemctl enable cpupower.service
EOF
}

configure_tlp() {
    logi "Enabling service: tlp"
    chrootdo <<EOF
systemctl enable tlp.service
EOF
}

configure_thermald() {
    logi "Enabling service: thermald"
    chrootdo <<EOF
systemctl enable thermald.service
EOF
}

configure_networkmanager() {
    logi "Enabling service: NetworkManager"
    chrootdo <<EOF
systemctl enable NetworkManager.service
EOF

    logi "Enabling service: systemd-resolved"
    chrootdo <<EOF
systemctl enable systemd-resolved.service
EOF
}

configure_openssh() {
    logi "Enabling service: sshd"
    chrootdo <<EOF
systemctl enable sshd.service
EOF
}

configure_sudo() {
    logi "Making $MY_NAME as a sudoer"
    chrootdo <<EOF
echo "$MY_NAME ALL=(ALL:ALL) ALL" > /etc/sudoers.d/$MY_NAME
EOF
}

configure_libvirt() {
    logi "Configuring libvirt for $MY_NAME"

    local libvirtd_conf="/etc/libvirt/libvirtd.conf"
    logi "Configuring $libvirtd_conf"
    chrootdo <<EOF
echo "unix_sock_group = \"libvirt\"" >> $libvirtd_conf
EOF

    local qemu_conf="/etc/libvirt/qemu.conf"
    logi "Configuring $qemu_conf"
    chrootdo <<EOF
echo "user = \"$MY_NAME\"" >> $qemu_conf
echo "group = \"$MY_NAME\"" >> $qemu_conf
EOF

    logi "Adding user $MY_NAME to group ${libvirt_group}"
    chrootdo <<EOF
usermod -aG $libvirt_group $MY_NAME
EOF

    logi "Enabling service: libvirtd"
    chrootdo <<EOF
systemctl enable libvirtd.service
EOF
}

configure_bluez-utils() {
    chrootdo <<EOF
systemctl enable bluetooth.service
EOF
}

################################################################################
### Helper functions
################################################################################
logi() {
    echo -e "\e[34;1m[ INFO ] $@\e[0m"
}

logw() {
    echo -e "\e[33;1m[ WARNING ] $@\e[0m"
}

loge() {
    echo -e "\e[31;1m[ ERROR ] $@\e[0m"
}

chrootdo() {
    cat | arch-chroot $MY_ARCH_ROOT
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
### Subcommands
################################################################################
validate() {
    ## Configure functions must be presented
    if ! validate_functions $(generate_package_configure_function_list); then
        loge "One or more configuration functions missing.  Aborting..."
        return 1
    fi
}

packages() {
    ## Install packages
    logi "Installing Arch Linux base system"
    pacstrap -KG $MY_ARCH_ROOT $(generate_package_list)
}

configure() {
    ## Configure packages
    for func in $(generate_package_configure_function_list); do
        eval $func
    done
}

gen-swap() {
    ## Swap file
    if [[ -n $MY_SWAPFILE_PATH ]] &&
           [[ ! -f $MY_SWAPFILE_PATH ]] &&
           [[ $MY_SWAPFILE_SIZE_MB -gt 0 ]]; then
        logi "Generating swapfile ($MY_SWAPFILE_SIZE_MB MB): $MY_SWAPFILE_PATH"
        dd if=/dev/zero of=$MY_SWAPFILE_PATH bs=1M count=$MY_SWAPFILE_SIZE_MB status=progress
        sync
        chmod 600 $MY_SWAPFILE_PATH
        mkswap $MY_SWAPFILE_PATH
        swapon $MY_SWAPFILE_PATH
    else
        logi "No swap file configured"
    fi
}

gen-fstab() {
    ## Fstab
    logi "Generating fstab"
    logw "genfstab may not recognize <pass> field for btrfs subvolumes.  Double check it before reboot."
    genfstab -U $MY_ARCH_ROOT >> $MY_ARCH_ROOT/etc/fstab
}

all() {
    logi "Starting full Arch installation..."
    validate &&
    packages &&
    configure &&
    gen-swap &&
    gen-fstab
    logi "Arch installation completed!"
}

################################################################################
### Script starts here
################################################################################

if [[ $# -eq 0 ]]; then
    all
    exit $?
fi

eval "$@"
