#!/usr/bin/perl

use strict;
use warnings;

my $emacs = `ps -C emacs -o pid=`;

if ($emacs eq "") {
    if (-x "/home/alex/disk/dev/emacs/src/emacs") {
        system("hyprctl dispatch workspace 2");
        system("/home/alex/disk/dev/emacs/src/emacs &");
    } else {
        system("hyprctl dispatch workspace 2");
        system("emacs &");
    }

    sleep 1;
}

system("hyprctl dispatch submap passthrough");

exit 0;
