#!/bin/sh

tmp=/tmp/$$.tmp
input=json-test.json

die() {
    rm -f "$tmp"
    exit 1
}

./json -p "$input" | diff -w - "$input" || die
#./json -p "$input" > "$tmp"
#./json -p "$input" | diff - "$tmp" || die
rm -f "$tmp"

