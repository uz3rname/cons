#!/bin/sh

echo 's x 1
s y 2
s z 3
w a.txt
q
'|./db 2>&1 >/dev/null
echo 'l a.txt
w b.txt
q
'|./db 2>&1 >/dev/null
diff a.txt b.txt
x=$?
rm a.txt b.txt
exit $x

