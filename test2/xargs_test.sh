#!/bin/sh

tmp=/tmp/$$

find .. -type '*.scm' | xargs cat > $tmp
find .. -type '*.scm' | ./xargs cat | diff - $tmp
r=$?
rm -f $tmp
[ x"$r" = "x0" ] || exit 1

