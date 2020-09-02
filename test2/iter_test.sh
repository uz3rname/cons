#!/bin/sh

cnt=10

stuff() {
    i="0"
    while read line; do
        x=$(echo $line|cut -f1 -d' ')
        y=$(echo $line|cut -f2 -d' ')
        z=$(echo $line|cut -f3 -d' ')
        [ "$y" = $(echo "$x^2"|bc -l|xargs printf "%.f") ] || exit 1
        [ $(printf "%.f" "$z") = $(echo "$x^3"|bc -l|xargs printf "%.f") ] || exit 1
        i=$((i + 1))
    done
    [ x"$i" = x"${cnt}" ] || exit 1
}

if which shuf; then
    ./iter|shuf|head -n ${cnt} |stuff
else
    ./iter|head -n ${cnt}|stuff
fi

