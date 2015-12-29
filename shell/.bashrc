#
# Global environment
#
HOME="/home/alex"
HOME_HDD="/home/alex/olddisk$HOME"
PATCHES="/home/alex/patches"
DEV="$HOME/dev"
WORK="$HOME/work"
GIT_DEV="$DEV/git"
KERNEL_DEV="$DEV/linux"
OS=`uname`

if [ "$OS" == "FreeBSD" ] || [ "$OS" == "DragonFly" ]; then
    export SHELL="/usr/local/bin/bash"
else
    export SHELL="/bin/bash"
fi
export EDITOR="emacs"
export BROWSER="firefox"
export CC=gcc
export AS=as
export AR=AR
export CXX=g++
export LD=ld

#
# bash
#
shopt -s nocaseglob;
shopt -s cdspell
shopt -s histappend
shopt -s checkwinsize
shopt -s autocd

export HISTFILESIZE=
export HISTSIZE=
export HISTTIMEFORMAT="[%F %T] "

#
# locale
#
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"

# pkg config
PKG_CONFIG_PATH=$PKG_CONFIG_PATH:~/usr/lib/pkgconfig
export PKG_CONFIG_PATH

#
# includes
#
source ~/.bash/export
source ~/.bash/devel
source ~/.bash/prompt

source ~/.bash/system
source ~/.bash/network
source ~/.bash/archives
source ~/.bash/standard
source ~/.bash/completition
