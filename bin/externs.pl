#!/usr/bin/env perl

use strict;

sub match_func {
    my $s = shift;
    if($s =~ m/\(\(\:\:/) {
        if($s =~ m/"([^"]+)"/) {
            return $1;
        }
        return "*";
    }
    return 0;
}

sub match_name {
    my $s = shift;
    if($s =~ m/\(extern\s+([a-zA-Z0-9\-]+)/) {
        return $1;
    }
    return 0;
}

sub dofile {
    my $name = shift;
    open(FILE, $name);
    my $line = <FILE>;
    while(1) {
        if(my $int_name = match_name($line)) {
            my $done = 0;
            while(1) {
                if(match_name($line) and not $int_name) {
                    last;
                }
                if(my $e_name = match_func($line)) {
                    if($e_name eq "*") {
                        print("$int_name\n");
                        $int_name = '';
                    } else {
                        print("$e_name\n");
                    }
                    $line = <FILE>;
                } else {
                    if($done) {
                        last;
                    } else {
                        $line = <FILE>;
                    }
                }
                $done = 1;
            }
        } else {
            $line = <FILE>;
        }
        last if(not $line);
    }
    close(FILE);
}

sub doarg {
    my $arg = shift;
    if(-d $arg) {
        foreach my $name(glob("$arg/*")) {
            doarg($name);
        }
    } elsif(-f $arg) {
        dofile($arg);
    } else {
        die("Can't locate $arg\n");
    }
}

if(@ARGV == 0) {
    print<<EOF
Usage: $0 <file or dir> ...
EOF
    ;
    exit;
}

foreach(@ARGV) {
    doarg($_);
}

