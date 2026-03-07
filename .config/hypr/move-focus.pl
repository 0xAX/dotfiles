#!/usr/bin/perl

use strict;
use warnings;

my $direction = shift @ARGV // "";
my $group = `hyprctl activewindow | grep "grouped" | awk '{print \$2}'`;
my $lastWindowID = `hyprctl activeworkspace | grep "lastwindow:" | awk '{print \$2}' | sed 's/^0x//'`;
my $lastGroup = `echo "$group" | rev | cut -d"," -f 1 | rev | sed 's/^0x//'`;

chomp($group);
chomp($lastWindowID);
chomp($lastGroup);

if ($group eq "0") {
    if ($direction eq "l") {
        system("hyprctl dispatch movefocus l");
    }
    elsif ($direction eq "r") {
        system("hyprctl dispatch movefocus r");
    }
    elsif ($direction eq "u") {
        system("hyprctl dispatch movefocus u");
    }
    elsif ($direction eq "d") {
        system("hyprctl dispatch movefocus d");
    }
}
else {
    if ($direction eq "r") {
        if ($lastGroup eq $lastWindowID) {
            system("hyprctl dispatch movefocus r");
        }
        else {
            system("hyprctl dispatch changegroupactive f");
        }
    }
    elsif ($direction eq "l") {
        system("hyprctl dispatch changegroupactive b");
    }
}
