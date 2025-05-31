#!/usr/bin/env perl

# TODO: For this moment we use firefox manually to install extension.
#       Look for a way how to do it without clicking `Add`.
# TODO: check how can we do it without wget

use strict;
use warnings FATAL => 'all';

my %extensions = (
    "ublock-origin" => "https://addons.mozilla.org/en-US/firefox/addon/ublock-origin/",
    "zen-fox" => "https://addons.mozilla.org/en-US/firefox/addon/zen-fox/",
    "1password" => "https://addons.mozilla.org/en-US/firefox/addon/1password-x-password-manager",
    "awesome-screenshot" => "https://addons.mozilla.org/en-US/firefox/addon/screenshot-capture-annotate/"
);

my $firefox_dir = $ENV{"HOME"} . "/.mozilla/firefox/";
my $http = HTTP::Tiny->new();

opendir(my $DIR, $firefox_dir);
while (my $entry = readdir $DIR) {
    if ($entry =~ "default-release") {
        my $firefox_profile_dir = $firefox_dir . $entry . "/";

        foreach my $extension (keys %extensions) {
            my $extension_url = $extensions{$extension};
            my $download_ext_page_path = "/tmp/$extension";
            my $download_ext_path = "/tmp/$extension" . ".xpi";

            unlink($download_ext_page_path);
            unlink($$download_ext_path);

            print("Downloading $download_ext_page_path page\n");
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
            `firefox -new-window $download_ext_path`;
            unlink($download_ext_page_path);
        }

        # We have found default firefox profile. Use it.
        last;
    }
}

exit(0);
