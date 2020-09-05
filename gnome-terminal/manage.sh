#!/bin/bash

TERM_PROFILE="/home/alex/dev/dotfiles/gnome-terminal/gnome-terminal-profiles.dconf"

if [ "$1" = "--import" ];
then
    dconf dump /org/gnome/terminal/legacy/profiles:/ > $TERM_PROFILE
    exit 0
fi

if [ "$1" = "--install" ];
then
    dconf load /org/gnome/terminal/legacy/profiles:/ < gnome-terminal-solarized-profile.dconf
    exit 0
fi

echo "Usage:"
echo "  manage.sh --import - Import gnome-termina profile"
echo "  manage.sh --install - Install gnome-terminal profile"

exit 1
