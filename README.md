# RICE - Rice Is a Configuration collEction

> Keep ricing

This is a collection of my daily tool configurations.

## Get Started

Simply use `install.sh` script to install supported configs.

```sh
# List supported configs
$ ./install.sh

# Install selected ones
$ ./install.sh bash pwsh git tmux
```

To add a new config installation, define a function in `install.sh` with name `NAME_conf`. Check the script for details.

## Why not use GNU Stow to manage dotfiles?

`Stow` is ideally good for a clean installation like Arch Linux where there are minimal files created in user's home directory. However, most of popular distros prefer to create a bunch of files by default, then `Stow` will have a hard time to link my files.

Unfortunately I have to work on those distros from time to time.

Also sometimes configs are modified for specific purposes but those changes are not intended to be tracked.

So `Stow` doesn't really work well for me and I prefer to refer my config in the local config file if it supports include-ish syntax.

## Complementary Tools

Quick jump

- [z.lua](https://github.com/skywind3000/z.lua)
