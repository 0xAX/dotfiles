#!/usr/bin/env perl
#
# manage-dotfiles.pl            install/update my dotfiles.
#
# USAGE:
#
#       ./manage-dotfiles.pl -i - install dotfiles from this directory
#                                 to the local system.
#       ./manage-dotfiles.pl -u - update the repo from this directory.

use strict;

sub usage {
    print STDOUT <<USAGE_END;
USAGE:
  manage-dotfiles.pl -i - install dotfiles from this directory
                            to the local system.
  manage-dotfiles.pl -u - update the repo from this directory.
USAGE_END
    exit 0;
}
