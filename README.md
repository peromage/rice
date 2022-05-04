# PEW - Peromage's Emacs Workbench

> A minimalist's Emacs configuration.

This is my personal Emacs configuration. Before using it you should be aware that it might be suitable for you.

This configuration intends to be as minimal as possible meaning it "just works" without installing too many packages.

I.E. if Emacs already has certain features I'll try to optimize them unless they are far from good-to-use.

## Get Started

Clone this repo and put it to your home directory with folder name `.emacs.d`.

```bash
$ git clone https://github.com/peromage/pew.git .emacs.d
```

Then Emacs will automatically install everything for you.

## Local Files

Temporary local changes can go into `pew/local.el` including settings by `customize`.

`local.el` will be applied on top of the default pew configuration.

## Directories

- *lisp*: Pew configurations.
- *site-lisp*: Third party packages.
- *snippets*: My snippets.

## Credit

This Configuration is inspired by [Steve Purell's emacs.d](https://github.com/purcell/emacs.d)
