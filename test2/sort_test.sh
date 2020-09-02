#!/bin/sh

tmp=/tmp/$$
i=0

rm -f "$tmp"
while [ x"$i" != "x1000" ]; do
    echo $RANDOM >> "$tmp"
    i=$(($i + 1))
done

sort "$tmp" > "${tmp}.1"
./sort "$tmp" > "${tmp}.2"
diff "${tmp}.1" "${tmp}.2"
res=$?
rm -f "$tmp" "${tmp}.1" "${tmp}.2"
exit $res

