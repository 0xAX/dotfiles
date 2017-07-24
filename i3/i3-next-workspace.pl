#!/usr/bin/perl

use strict;
use warnings;

use JSON;

my $workspaces = `i3-msg -t get_workspaces`;
my $workspaces_json = decode_json($workspaces);

foreach my $workspace ( @{$workspaces_json} ) {
    my %workspace_hash = %{$workspace};
    my $focused = $workspace_hash{focused};
    my $workspace_num = $workspace_hash{num};

    my $emacs = `ps -C emacs -o pid=`;
        
    if ($focused eq "1") {
        if ($workspace_num == 2) {
            system("i3-msg", "mode", "default");
        } elsif ($workspace_num == 1 && $emacs ne "") {
            system("i3-msg", "mode", "passthrough");
        }
        system("i3-msg", "workspace next");
        last;
    }
}

exit 0;
