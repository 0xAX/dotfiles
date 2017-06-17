#!/usr/bin/env perl
#
# manage-dotfiles.pl            install/update my dotfiles.
#
# USAGE:
#
#       manage-dotfiles.pl --install - install dotfiles from this directory
#                                      or directory pointed by the 
#                                      $DOTFILES_ROOT_DIR to the local system.
#       manage-dotfiles.pl --update  - put dotfiles from the system to the 
#                                      repo.
#
# Enviroment variables used by manage-dotfiles.pl:
#
#   * DOTFILES_ROOT_DIR - path to this repo.
#

use strict;

sub usage {
    print STDOUT <<USAGE_END;
USAGE:
  manage-dotfiles.pl --install - install dotfiles from this directory
                                 or directory pointed by the 
                                 \$DOTFILES_ROOT_DIR to the local system.
  manage-dotfiles.pl --update  - put dotfiles from the system to the 
                                 repo.

Enviroment variables used by manage-dotfiles.pl:

   * DOTFILES_ROOT_DIR - path to this repo.

USAGE_END
    exit 0;
}

usage;

exit 0;
