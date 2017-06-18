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
#   * DOTFILES_ROOT_DIR - path to this repo. If it is not set
#                         $PWD will be used.
#

use strict;
use Getopt::Long;
use warnings;
use Term::ANSIColor 2.00 qw(:pushpop);

#
# CLI flags
#
my $install_dotfiles = 0;
my $update_dotfiles = 0;

#
# Environment variables which are used here
#
my $HOME = $ENV{'HOME'};
my $DOTFILES_ROOT_DIR = $ENV{'DOTFILES_ROOT_DIR'} || $ENV{'PWD'};

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

GetOptions(
    "install" => \$install_dotfiles,
    "update"  => \$update_dotfiles
) or die("Error in command line arguments\n");

if ($install_dotfiles == 1 && $update_dotfiles == 1) {
    print PUSHCOLOR BOLD RED "Error:";
    print POPCOLOR;
    print " --install and --update can't be used together.\n";
    exit 1;
}

usage();

exit 0;
