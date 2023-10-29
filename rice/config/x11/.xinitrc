#!/usr/bin/env bash

## KDE
launch_kde() {
    export DESKTOP_SESSION=plasma
    exec startplasma-x11
}

launch_kde
