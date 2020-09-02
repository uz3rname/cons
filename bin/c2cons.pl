#!/usr/bin/env perl

use strict;

my $types = {
    byte        => 'byte',
    char        => 'byte',
    int         => 'int',
    unsigned    => 'int',
    signed      => 'int',
    long        => 'int',
    short       => 'int',
    double      => 'real',
    real        => 'real',
    bool        => 'bool',
    _Bool       => 'bool',
    void        => 'void'
};

sub resolve_type {
    my $t = shift;
    if(my $v = $types->{ $t }) {
        if($v eq $t) {
            return $t;
        } else {
            return resolve_type($v);
        }
    } else {
        return $t;
    }
}

sub trim {
    my $s = shift;
    $s =~ s/^\s+|\s+$//g;
    return $s;
}

sub conv_type {
    my $t = shift;
    if($t =~ m/\*/) {
        while($t =~ m/([^\*]+)\*(.*)/) {
            if(my $v = $types->{ $t }) {
                $t = $v;
            } else {
                my $name = $1;
                my $rest = $2;
                my $x = resolve_type($name);
                $t = "(: ptr $x)$rest";
            }
        }
        return $t;
    } else {
        return resolve_type($t);
    }
}

sub fix_asterisks {
    my $s = shift;
    $s =~ s/([^\[ ]+)\s*\[[^\]]*\]/\*$1/g;
    $s =~ s/\s+\*/\*/g;
    $s =~ s/\*([^\* ])/\* $1/g;
    $s =~ s/__restrict//g;
    return $s;
}

sub get_arg {
    my $s = shift;
    $s = fix_asterisks($s);
    my @args = map { trim($_) } split(' ', $s);
    my $n = "$args[-1]";
    $n =~ s/\*//g;
    if(@args < 2 or exists($types->{ $n })) {
        return conv_type($args[-1]);
    } else {
        return conv_type($args[-2]);
    }
}

sub fix_name {
    my $s = shift;
    $s =~ s/_/-/g;
    return $s;
}

sub print_help {
        print <<EOF
Usage: $0 <file> ... [-p function prefix] [-o output file name]
EOF
        ;
        exit;
}

my $str = '';
my $func_prefix = '';
my $out_file = 0;

while(my $arg = shift @ARGV) {
    if($arg eq "-p") {
        $func_prefix = shift @ARGV;
    } elsif($arg eq "-o") {
        $out_file = shift @ARGV;
    } elsif($arg eq "-h") {
        print_help;
    } else {
        $str .= `cpp $arg | grep -v '^#'`;
    }
}

print_help if(not $str);

while($str =~ m/typedef\s+([^{};]+);/g) {
    my @words = split(' ', fix_asterisks($1));
    my $name = $words[-1];
    my $type = conv_type($words[-2]);
    if(not exists $types->{ "$words[-1]" }) {
        $types->{$name} = $type;
    }
}

open(STDOUT, ">$out_file") if($out_file);

#sub get_member {
#    my $s = shift;
#    $s = fix_asterisks($s);
#    my @args = map { trim($_) } split(' ', $s);
#    my $name = fix_name($args[-1]);
#    return "(.$name ".conv_type($args[-2]).")";
#}
#
#while($str =~ m/struct\s+([a-zA-Z_0-9]+)\s+{([^{}]+)}/g) {
#    my $s = trim($2);
#    my $name = $1;
#    $s =~ s/\n//g;
#    my @args = map { get_member(trim($_)) } split(';', $s);
#    $s = join("\n   ", @args);
#    $types { "$name*" } = "$name";
#    print("(type $name\n  ($s)\n");
#}

sub get_args {
    my $line = shift;
    my $del = shift;
    return map { get_arg(trim($_)) } split($del, $line);
}

while($str =~ m/extern\s+([^\({}]+\([^\){}]+\))/g) {
    my $proto = $1;
    my ($name, $type, @args);
    $proto =~ s/\n//g;
    $proto = fix_asterisks($proto);
    if($proto =~ m/([a-zA-Z0-9_\*]+\s+)+([a-zA-Z0-9_]+)\s*\(/) {
        $name = $2;
        $type = conv_type(trim($1));
    }
    if($proto =~ m/\((.+)\)/) {
        @args = get_args($1, ',');
    }
    my $arg_line = join(' ', @args);
    my $cname = fix_name($name);
    print("(extern ${func_prefix}${cname} ((:: $arg_line -> $type) \"$name\"))\n");
}

