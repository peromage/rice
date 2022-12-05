#!/bin/sh

## KDE
launch_kde() {
    export DESKTOP_SESSION=plasma
    exec startplasma-x11
}

launch_kde
