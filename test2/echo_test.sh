#!/bin/sh

[ "$(find .. | xargs echo)" = "$(find .. | xargs ./echo)" ] || exit 1
[ "$(find .. | xargs echo -n)" = "$(find .. | xargs ./echo -n)" ] || exit 1

