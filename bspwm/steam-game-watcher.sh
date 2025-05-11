#!/bin/bash

while true; do
    for wid in $(bspc query -N -n .!hidden.window); do
	class=$(xprop -id "$wid" WM_CLASS 2>/dev/null | awk -F '"' '{print $4}')
	if  [[ "$class" =~ steam_app_ ]]; then
	    bspc node  "$wid" --to-desktop '^3'
	fi
    done
    sleep 1
done
