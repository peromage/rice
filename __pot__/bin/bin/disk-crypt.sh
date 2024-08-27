#!/usr/bin/env bash

## Easy manipulations for disk encryption options

set -e

function help {
    cat <<EOF
Usage:
  $0 <command> [arguments]

  dump        DEVICE
  test        DEVICE
  kill-slot   DEVICE SLOT-ID
  kill-token  DEVICE TOKEN-ID
  add-pin     DEVICE
EOF
    exit 1
}

if [[ $# -eq 0 ]]; then
    help
    exit 1
fi

CMD="${1:?Sub command is required}" && shift

case "$CMD" in
    (dump)
        { device="$1" && shift; } || help
        sudo cryptsetup luksDump "$device"
        ;;
    (test)
        { device="$1" && shift; } || help
        sudo cryptsetup open "$device" --test-passphrase -v
        ;;
    (kill-slot)
        { device="$1" && shift && slot_id="$1" && shift; } || help
        sudo cryptsetup luksKillSlot "$device" "$slot_id"
        ;;
    (kill-token)
        { device="$1" && shift && token_id="$1" && shift; } || help
        sudo cryptsetup token remove "$device" --token-id "$token_id"
        ;;
    (add-pin)
        { device="$1" && shift; } || help
        sudo systemd-cryptenroll --tpm2-device=auto --tpm2-pcrs=0+7 --tpm2-with-pin=true "$device"
        ;;
    (help|--help)
        help
        ;;
esac
