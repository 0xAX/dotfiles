#!/bin/bash

# Global environment
DISK="$HOME/disk"
DEV_DISK="$DISK/dev"
BASHRC="$HOME/.bashrc"
BASHRC_DIR="$HOME/.bash"
GNUPGHOME="$HOME/.gnupg"
PATCH_DIR="$HOME/patches"
OLD_HDD="$HOME/olddisk$HOME"
VIRT_DIR="$OLD_HDD/virt"

# Paths to dev env
DEV="$HOME/dev"
WORK="$HOME/work"
GIT_DEV="$DEV/git"
KERNEL_DEV="$DEV/linux"
DOTFILES="$DEV/dotfiles"

# some systemd related
SYSTEMD_SCRIPTS="/usr/lib/systemd/scripts"
SYSTEMD_SYSTEM="/usr/lib/systemd/system"
SYSTEMD_CONF="/etc/systemd/system.conf"
SYSTEMD_CONFIGS="/etc/systemd/system.conf.d"
SYSTEMD_USER_CONF="/etc/systemd/user.conf"

# some xdg
XDG_DESKTOP_DIR="$HOME/Desktop"
XDG_DOCUMENTS_DIR="$HOME/Documents"
XDG_DOWNLOAD_DIR="$HOME/Downloads"
XDG_MUSIC_DIR="$HOME/Music"
XDG_PICTURES_DIR="$HOME/Pictures"
XDG_PUBLICSHARE_DIR="$HOME/Public"
XDG_TEMPLATES_DIR="$HOME/.Templates"
XDG_VIDEOS_DIR="$HOME/Videos"

# OS dependend
OS=$(uname)
if [ "$OS" == "FreeBSD" ] || [ "$OS" == "DragonFly" ]; then
    export SHELL="/usr/local/bin/bash"
    export MAKE="/usr/local/bin/gmake"
    source /usr/local/share/bash-completion/bash_completion
else
    export SHELL="/bin/bash"
    export MAKE="/usr/bin/make"
    source /usr/share/bash-completion/bash_completion

    alias ls='ls --color'
    alias ll='ls -alF'
    alias la='ls -A'
    alias l='ls -CF'
    alias lsd="ls -lF"
fi

# global env
export BROWSER="firefox"

if [ -f "/home/alex/disk/dev/emacs/src/emacs" ];
then
    export EMACS="/home/alex/disk/dev/emacs/src/emacs"
else
    export EMACS="emacs"
fi
export EDITOR="$EMACS"

# global development env
export CC=gcc
export CXX=g++
export LD=ld
export TAR=tar

# Default wm
export WM="i3"

# check a given command in hash at first
shopt -s checkhash
# check LINES and COLUMNS after every command
shopt -s checkwinsize
# an argument to the cd builtin command that is not
# a directory is assumed to be the name of a variable
shopt -s cdable_vars
# command name that is the name of a directory is executed
# as if it were the argument
shopt -s autocd
# save multiline output to the history
shopt -s cmdhist
# spell check for directories
shopt -s dirspell
# enable pattern matching
shopt -s extglob
# autocomple hostname
shopt -s hostcomplete
# case-sensetive pattern matching
shopt -s nocaseglob
# the same for case [[ ]]
shopt -s nocasematch

# inhibit trancation of histor
export HISTFILESIZE=-1
export HISTSIZE=-1
export HISTTIMEFORMAT="[%F %T] "

# default locale
LANG="en_US.UTF-8"
LC_CTYPE="en_US.UTF-8"
LC_NUMERIC="en_US.UTF-8"
LC_TIME="en_US.UTF-8"
LC_COLLATE="en_US.UTF-8"
LC_MONETARY="en_US.UTF-8"
LC_MESSAGES="en_US.UTF-8"
LC_PAPER="en_US.UTF-8"
LC_NAME="en_US.UTF-8"
LC_ADDRESS="en_US.UTF-8"
LC_TELEPHONE="en_US.UTF-8"
LC_MEASUREMENT="en_US.UTF-8"
LC_IDENTIFICATION="en_US.UTF-8"
LC_ALL="en_US.UTF-8"

# include other sources
source $BASHRC_DIR/archives
source $BASHRC_DIR/completition
source $BASHRC_DIR/prompt
source $BASHRC_DIR/devel
source $BASHRC_DIR/system
source $BASHRC_DIR/standard
source $BASHRC_DIR/export
source $BASHRC_DIR/docker
source $BASHRC_DIR/wallpapers
source $BASHRC_DIR/terminal
source $BASHRC_DIR/vterm
source "$HOME/.cargo/env"

# turn off dpms
xset s off -dpms

# not start gvfsd fs
export GVFS_DISABLE_FUSE=1

# Disable Gnome accessibility bridge
NO_AT_BRIDGE=1

# Fix gpg without gdb and other Gnome things
export GPG_TTY=$(tty)

# Set keyboard layout
setxkbmap -model pc105 -layout pl,ru -option 'grp:lwin_toggle'
. "$HOME/.cargo/env"
