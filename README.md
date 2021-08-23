# PEW - Peromage's Emacs Workstation

This is my personal Emacs configuration together with some other common tools' settings as well.

This Emacs configuration is intended to be as minimal as possible.

## Get Started

Clone this repo and put it to your home directory with folder name `.emacs.d`.

```bash
$ git clone https://github.com/peromage/pew.git .emacs.d
```

Then Emacs will automatically installed everything for you.

## Make Local Changes

Temporary local changes will be put into `local.el` including settings by `customize`.

Also changes which don't need to be tracked by version control system can go into this file as well.

`local.el` will be applied on top of the default pew configuration.
