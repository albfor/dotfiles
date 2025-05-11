# Dotfiles

This repository contains my personal dotfiles.

## Setup

Deploy symlinks by running:

```bash
./setup.sh
```

## Emacs

This setup assumes Emacs is running as a daemon with a custom  init-directory.
Make sure to launch with the following command:

``` bash
emacs --daemon=main --init-directory="</path/to/this/repo>/emacs"
```

And open frames/instances with:

```bash
emacsclient -c -n -s main
```

