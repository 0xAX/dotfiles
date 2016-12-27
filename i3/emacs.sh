#!/bin/bash

EMACS_RUNNED=$(ps ax | grep emacs | awk '{ print $5 }' | sed -n '1p')

if [ "$EMACS_RUNNED" != "emacs" ]
then
    emacs &
    sleep 1;
fi

i3-msg workspace number 2;
i3-msg mode passthrough;
