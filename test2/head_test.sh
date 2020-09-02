#!/bin/sh

file1=Makefile
file2=/etc/fstab
tmp=/tmp/$$.tmp

head $file1 $file2 > $tmp
./head $file1 $file2 | diff - $tmp >/dev/null || exit 1
head -n 10 $file1 $file2 > $tmp
./head -n 10 $file1 $file2 | diff - $tmp >/dev/null || exit 1
rm $tmp

