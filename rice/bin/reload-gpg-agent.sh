#!/bin/sh

CLI_FLAG="USE_TTY"

case "$1" in
    -h|--help)
        exec cat <<EOF
Help:
  $0 -h|--help

Reload without setting environment variables:
  $0

Terminate gpg-agent:
  $0 -k|--kill

Use following commands to work in CLI environment.

Bourne Shells:
  eval \$($0 --sh)

Fish:
  eval ($0 --fish)

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
esac

exec gpg-connect-agent updatestartuptty /bye >/dev/null
