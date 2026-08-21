#!/usr/bin/perl
#
# Start emacs, or just switch to it when it already runs.
#
# Emacs lives alone on workspace 2, which sits on the big screen. The script
# starts emacs only if no emacs is running. Then it turns on passthrough mode,
# so keys go to emacs and not to sway, opens workspace 2, and gives the window
# the whole screen with no title bar.
#
# Bound to $mod+2 in ~/.config/sway/config.

use strict;
use warnings;

# $HOME, or the passwd entry for the current uid if it is somehow unset
my $home = $ENV{HOME} // (getpwuid($<))[7]
  or die "cannot determine the home directory\n";

# a self-built emacs is preferred over whatever is on PATH
my $self_built = "$home/disk/dev/emacs/src/emacs";

# check is emacs already running or not yet, and run it if not
my $emacs = `ps -C emacs -o pid=`;
if ($emacs eq "") {
    if (-x $self_built) {
        system("$self_built &");
    } else {
        system("emacs &");
    }
}

# Change sway mode to `passthrough` and switch to the second workspace
system("swaymsg", "mode", "passthrough");
system("swaymsg", "workspace", "number", "2");

# Wait for emacs up and running, and got to the proper workspace
for (1 .. 250) {
    last if `swaymsg -t get_tree` =~ /"class":\s*"[eE]macs"/;
    select(undef, undef, undef, 0.1);
}

# Switch to emacs window
system("swaymsg", '[class="^[eE]macs$"] focus');

# Drop window border for emacs
system("swaymsg", "border", "none");
system("swaymsg", "layout", "splith");

exit 0;
