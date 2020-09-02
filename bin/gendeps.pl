#!/usr/bin/env perl

use strict;
use File::Basename;

my @lib_dirs = ();
my $recipe = '';
my @inc_dirs = ();
my @input_files = ();
my $output_dir = '';
my $append = '';
my $default_dir = '';
my $obj = 0;

while(my $arg = shift @ARGV) {
    if($arg eq '-L') {
        my $d = shift(@ARGV);
        $d =~ s/([^\/])$/$1\//;
        push(@lib_dirs, $d);
    } elsif($arg eq '-I') {
        my $d = shift(@ARGV);
        $d =~ s/([^\/])$/$1\//;
        push(@inc_dirs, $d);
    } elsif($arg eq '-a') {
        $append .= shift(@ARGV);
    } elsif($arg eq '-r') {
        $recipe = shift(@ARGV);
    } elsif($arg eq '-o') {
        $output_dir = shift(@ARGV) . "/";
    } elsif($arg eq '-d') {
        $default_dir = shift(@ARGV);
    } elsif($arg eq '-c') {
        $obj = 1;
    } else {
        push(@input_files, $arg);
    }
}

sub find_file {
    my $name = shift;
    my $file_dir = shift;
    foreach(map { "${_}${name}" } @_) {
        return $_ if(-f $_);
    }
    if($default_dir) {
        return "${default_dir}/${name}";
    } else {
        return "${file_dir}/${name}";
    }
}

sub do_file {
    my $file_name = shift;
    my($name, $path, $suffix) = fileparse($file_name, qr/\.[^\.]*/);
    my @deps = ();
    my $mod_name = '';
    open(IN, "<$file_name");
    foreach(<IN>) {
        if($_ =~ m/\(module\s+(\w+)\)/) {
            $mod_name = $1;
        }
        while($_ =~ m/\(use\s+(\w+(\s+\w+)*)\)/g) {
            foreach(split(/\s+/, $1)) {
                push(@deps, find_file("${_}.a", $path, $path, @lib_dirs));
            }
        }
        while($_ =~ m/\(include "([^"]+)"\)/g) {
            push(@deps, find_file($1, $path, $path, @inc_dirs));
        }
    }
    if($mod_name) {
        print("${output_dir}${mod_name}.a:");
    } else {
        if($obj) {
            $name = "${name}.o";
        }
        print("${output_dir}${name}:");
    }
    print(" ${file_name}");
    foreach(@deps) {
        my $dep = $_;
        $dep =~ s/^\.\///;
        $dep =~ s/(\/\.\/)/\//g;
        print(" $dep");
    }
    print(" ${append}") if($append);
    print("\n");
    if($recipe) {
        print("\t${recipe}\n");
    }
    close(IN);
}

do_file($_) foreach(@input_files);

