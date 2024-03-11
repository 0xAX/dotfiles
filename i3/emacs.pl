#!/usr/bin/perl

use strict;
use warnings;

my $emacs = `ps -C emacs -o pid=`;

if ($emacs eq "") {

    if (-x "/home/alex/disk/dev/emacs/src/emacs") {
        system("/home/alex/disk/dev/emacs/src/emacs &");
    } else {
        system("emacs &");
    }

    sleep 1;
}

system("i3-msg", "mode", "passthrough");
system("i3-msg", "workspace", "number", "2");

exit 0;
