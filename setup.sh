#!/bin/bash

DOTFILES_DIR="$HOME/projects/dotfiles"

declare -A SYMLINKS=(
    ["$DOTFILES_DIR/emacs"]="$HOME/.config/"
    ["$DOTFILES_DIR/sxhkd"]="$HOME/.config/"
    ["$DOTFILES_DIR/kitty"]="$HOME/.config/"
    ["$DOTFILES_DIR/bspwm"]="$HOME/.config/"
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
