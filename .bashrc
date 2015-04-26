export SHELL=/bin/bash

#
# Global environment
#
MYHOME="/home/alex"
DEV="$MYHOME/dev"
WORK="$MYHOME/work"
GIT_DEV="$DEV/git"
KERNEL_DEV="$DEV/linux"

export EDITOR="emacs"
export BROWSER="firefox"
export CC=gcc
export AS=as
export AR=AR
export CXX=g++

#
# bash
#
set menu-complete-display-prefix=on

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

# Add custom compiled libraries to library search path.
LD_LIBRARY_PATH=$LD_LIBRARY_PATH:~/usr/lib
export LD_LIBRARY_PATH

# Add custom compiled libraries to library run path.
LD_RUN_PATH=$LD_RUN_PATH:~/usr/lib
export LD_RUN_PATH

#
# includes
#
source ~/.bash/export
source ~/.bash/devel
source ~/.bash/promt
source ~/.bash/system
source ~/.bash/network
source ~/.bash/archives
source ~/.bash/standard
source ~/.bash/completition
