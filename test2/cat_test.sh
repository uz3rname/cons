#!/bin/sh

tmp=/tmp/$$.tmp

find .. -name '*.scm' |xargs cat|cat -n > $tmp
find .. -name '*.scm' |xargs ./cat|./cat -n|diff - $tmp >/dev/null || exit 1
rm $tmp

