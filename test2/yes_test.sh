#!/bin/sh

tmp=/tmp/$$

./yes $$ 2>/dev/null |head -n 100 >$tmp
yes $$ 2>/dev/null |head -n 100|diff - $tmp >/dev/null || exit 1

rm -f $tmp

