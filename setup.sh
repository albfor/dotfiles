#!/bin/bash

DOTFILES_DIR="$HOME/projects/dotfiles"
CONFIG_DIR="$HOME/.config/"

declare -A SYMLINKS=(
    ["$DOTFILES_DIR/emacs"]="$CONFIG_DIR"
    ["$DOTFILES_DIR/sxhkd"]="$CONFIG_DIR"
    ["$DOTFILES_DIR/kitty"]="$CONFIG_DIR"
    ["$DOTFILES_DIR/bspwm"]="$CONFIG_DIR"
    ["$DOTFILES_DIR/polybar"]="$CONFIG_DIR"
)

for src in "${!SYMLINKS[@]}"; do
    dest="${SYMLINKS[$src]}"
    echo "Linking $src -> $dest"

    if [ -e  "$dest" ] && [ ! -L "$dest" ]; then
	mv "$dest" "$dest.backup.$(date +%s)"
	echo "Backup existing $dest"
    fi

    ln -sF "$src" "$dest"
done

echo "All symlinks created."
