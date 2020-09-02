#!/bin/sh

if [ -z "$1" ]; then
    echo "Usage: $0 <library> [output]"
elif [ -n "$2" ]; then
    ar p "$1" .header > "$2"
else
    ar p "$1" .header
fi

