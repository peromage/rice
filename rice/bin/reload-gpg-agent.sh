#!/usr/bin/env bash

CLI_FLAG="USE_TTY"

case "$1" in
    -h|--help)
        exec cat <<EOF
Only one option is expected at a time.

If no option is specified, this script reloads gpg-agent without setting
environment variables.

  -h | --help           Print this help message
  -k | --kill           Terminate gpg-agent
  --sh                  Print commands that set environment variables for Bourne
                        shells:
                          eval \$($0 --sh)
  --fish                Print commands that set environemnt variables for Fish
                        shell:
                          eval ($0 --fish)
  --pwsh                Print commands that set environemnt variables for
                        PowerShell:
                          iex "\$($0 --pwsh)"
EOF
        ;;

    -k|--kill)
        echo "Killing gpg-agent"
        exec gpgconf --kill gpg-agent
        ;;

    --sh)
        exec cat <<EOF
GPG_TTY=\$(tty) && export GPG_TTY;
export PINENTRY_USER_DATA=${CLI_FLAG};
gpg-connect-agent updatestartuptty /bye >/dev/null;
EOF
        ;;

    --fish)
        exec cat <<EOF
set -gx GPG_TTY (tty);
set -gx PINENTRY_USER_DATA ${CLI_FLAG};
gpg-connect-agent updatestartuptty /bye >/dev/null;
EOF
        ;;

    --pwsh)
        exec cat <<EOF
\$env:GPG_TTY = (tty);
\$env:PINENTRY_USER_DATA = "${CLI_FLAG}";
gpg-connect-agent updatestartuptty /bye >/dev/null;
EOF
        ;;
    *)
        exec gpg-connect-agent updatestartuptty /bye >/dev/null
        ;;
esac
