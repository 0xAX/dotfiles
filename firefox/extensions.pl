#!/usr/bin/env perl

# TODO: check how can we do it without wget
# TODO: add comments to the script

use strict;
use warnings FATAL => 'all';

my $firefox_dir = $ENV{"HOME"} . "/.mozilla/firefox/";
my %extensions = (
    "ublock-origin" => "https://addons.mozilla.org/en-US/firefox/addon/ublock-origin/",
    "zen-fox" => "https://addons.mozilla.org/en-US/firefox/addon/zen-fox/",
    "1password" => "https://addons.mozilla.org/en-US/firefox/addon/1password-x-password-manager",
    "awesome-screenshot" => "https://addons.mozilla.org/en-US/firefox/addon/screenshot-capture-annotate/"
);

sub get_firefox_default_profile_name {
    my ($in_section, $path, $is_default);
    my $profiles_ini = "$ENV{HOME}/.mozilla/firefox/profiles.ini";
    open my $fh, '<', $profiles_ini or die "Cannot open $profiles_ini: $!";

    while (my $line = <$fh>) {
        chomp $line;
        if ($line =~ /^\[Profile\d+\]/) {
            $in_section = 1;
            $path = undef;
            $is_default = 0;
        }
        elsif ($in_section && $line =~ /^Path=(.+)/) {
            $path = $1;
        }
        elsif ($in_section && $line =~ /^Default=1/) {
            return "$ENV{HOME}/.mozilla/firefox/" . $path  . "/" if defined $path;
        }
    }

    return undef;
}

sub ask_yes_no {
    my ($prompt, $default) = @_;
    my $hint = !defined $default ? "[y/n]"
        : $default ? "[Y/n]"
        : "[y/N]";

    while (1) {
        print "$prompt $hint ";
        chomp(my $ans = lc <STDIN> // "");

        return $default if $ans eq "" && defined $default;
        return 1 if $ans eq "y"  || $ans eq "yes";
        return 0 if $ans eq "n"  || $ans eq "no";

        print "Please answer yes or no.\n";
    }
}

my $firefox_profile_dir = get_firefox_default_profile_name();

foreach my $extension (keys %extensions) {
    my $extension_url = $extensions{$extension};
    my $download_ext_page_path = "/tmp/$extension";
    my $download_ext_path = "/tmp/$extension" . ".xpi";

    next unless ask_yes_no("Install $extension?", 0);

    unlink($download_ext_page_path);
    unlink($download_ext_path);

    print("Downloading $extension page\n");
    `wget --output-document=$download_ext_page_path $extension_url`;

    open my $fh, "<", $download_ext_page_path or die "Error: Can't open $download_ext_page_path file. Error: $!";
    my $data = do { local $/; <$fh> };
    my @xpi_urls = ($data =~ m{https?://[^\s"']+?\.xpi\b}g);
    if ((0 + @xpi_urls) == 0) {
        die "Error: Can not find download URL in the $download_ext_page_path."
    }

    print("Downloading $extension extension\n");
    my $download_ext_url = $xpi_urls[0];
    `wget --output-document=$download_ext_path $download_ext_url`;
    if (! -e $download_ext_path) {
        die "Error: Can not download extension - $download_ext_url\n";
    }

    `firefox --new-window $download_ext_path`;
}

exit(0);
