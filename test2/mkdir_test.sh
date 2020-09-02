#!/bin/sh

tmp=$$

die() {
    rm -rf ./$tmp
    exit 1
}

./mkdir $tmp
[ -d "$tmp" ] || die
rm -rf ./$tmp
./mkdir -p $tmp/a/b/c
[ -d "${tmp}/a/b/c" ] || die
rm -rf ./$tmp

