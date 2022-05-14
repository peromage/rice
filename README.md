# PEW - Peromage's Emacs Workbench

> A minimalist's Emacs configuration.

This is my personal Emacs configuration. Be aware that it might not fit your needs.

This configuration intends to be as minimal as possible meaning that it prefers to use Emacs built-in features and optimize them.

## Features

- Light and quick
- Terminal friendly
- Respect Emacs vanilla key bindings
- Evil mode as the main editing mode (not every buffer)
- Vertico and its complementary packages as the completion framework
- LSP support

## Get Started

Clone this repo and put it to your home directory with folder name `.emacs.d`.

```bash
$ git clone https://github.com/peromage/pew.git ~/.emacs.d
```

Then Emacs will automatically install everything for you.

## Local Files

Temporary local changes can go into `pew/local.el` including settings by `customize`.

`local.el` will be applied on top of the default pew configuration.

## List of files and directories

```
pew
├── lisp/                   # Configuration files
├── site-lisp/              # Addtional package files written by myself
├── yasnippets/             # Snippet files loaded by yasnippet
├── early-init.el           # Init file loaded before init.el
├── init.el                 # Main init file
├── .pew                    # Configuration root placeholder
├── .gitignore              # Git ignore-file
├── LICENSE                 # License file
└── README.md               # This file
```

## Acknowledgement

This configuration is inspired by

- [purcell/emacs.d](https://github.com/purcell/emacs.d)
- [Protesilaos Stavrou](https://protesilaos.com/emacs/dotemacs)
- [condy0919/.emacs.d](https://github.com/condy0919/.emacs.d)
