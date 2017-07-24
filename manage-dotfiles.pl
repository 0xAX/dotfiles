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
use warnings;

use Getopt::Long;
use File::Copy qw(copy);
use File::Copy::Recursive qw(dircopy);
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

#
# Print help if something going wrong
#
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

my $argc = @ARGV;
if ($argc != 1) {
    usage();
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

#
# TODO
#
# * system
# * mail
# * and the same for install
#
if ($update_dotfiles == 1) {
    print PUSHCOLOR BOLD GREEN;
    print "Copy files: \n";
    print POPCOLOR;

    # i3
    print "Copying i3...\n";
    dircopy("$HOME/.i3", "i3");

    # bin
    print "Copying bin...\n";
    dircopy("$HOME/bin", "bin");

    # emacs
    print "Copying emacs...\n";
    dircopy("$HOME/.emacs.d", "emacs/.emacs.d");
    dircopy("$HOME/.emacscore", "emacs/.emacscore");
    copy("$HOME/.emacs", "emacs/.emacs");

    # mysql
    print "Copying mysql configuration...\n";
    copy("/etc/my.cnf", "mysql/");

    # shell
    print "Copying shell...\n";
    copy("$HOME/.bash_logout", "shell/");
    copy("$HOME/.bash_profile", "shell/");
    copy("$HOME/.bashrc", "shell/");
    dircopy("$HOME/.bash", "shell/.bash");

    # terminal configuration
    print "Copying terminator configuration...\n";
    copy("$HOME/.config/terminator/config", "terminator");

    # gdb configuration
    print "Copying GNU gdb configuration...\n";
    copy("$HOME/.gdbinit", ".");

    # git configuration
    print "Copying git configuration...\n";
    copy("$HOME/.gitconfig", ".");
    copy("$HOME/.gitignore_global", ".");

    # tmux configuration
    print "Copying tmux configuration...\n";
    copy("$HOME/.tmux.conf", ".");

    # intputrc configuration
    print "Copying .inputrc...\n";
    copy("$HOME/.inputrc", ".");
        
    print POPCOLOR;
    print "\n";

    #
    # clean everything unneded
    #
    print PUSHCOLOR BOLD YELLOW;
    print "Remove ignored stuff:\n";
    print POPCOLOR;
    system("git", "clean", "-x", "-f", "-d");

    exit(0);
}

if ($install_dotfiles == 1) {
    print PUSHCOLOR BOLD GREEN;

    # i3
    print "Copying i3...\n";
    dircopy("i3", "$HOME/.i3");

    # bin
    print "Copying bin...\n";
    dircopy("bin", "$HOME/bin");

    # emacs
    print "Copying emacs...\n";
    dircopy("emacs/.emacs.d", "$HOME/.emacs.d");
    dircopy("emacs/.emacscore", "$HOME/.emacscore");
    copy("emacs/.emacs", "$HOME/.emacs");

    # mysql
    print "Copying mysql configuration...\n";
    copy("mysql/my.cnf", "/etc/my.cnf");

    # shell
    print "Copying shell...\n";
    copy("shell/.bash_logout", "$HOME/");
    copy("shell/.bash_profile", "$HOME");
    copy("shell/.bashrc", "$HOME");
    dircopy("shell/.bash", "$HOME");

    # terminal configuration
    print "Copying terminator configuration...\n";
    copy("terminator/config", "$HOME/.config/terminator/");

    # gdb configuration
    print "Copying GNU gdb configuration...\n";
    copy(".gdbinit", "$HOME/");

    # git configuration
    print "Copying git configuration...\n";
    copy(".gitconfig", "$HOME/");
    copy(".gitignore_global", "$HOME/");

    # tmux configuration
    print "Copying tmux configuration...\n";
    copy(".tmux.conf", "$HOME/");

    # intputrc configuration
    print "Copying .inputrc...\n";
    copy(".inputrc", "$HOME/");
    
    print POPCOLOR;

    exit(0);
}

usage();

exit 0;
