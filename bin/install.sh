#!/bin/sh

die() {
    echo $@
    exit 1
}

while [ -n "$1" ]; do
    case "$1" in
        -m)
            mode="$2"
            shift
            ;;
        -v)
            verbose=y
            ;;
        *)
            if [ -z "$src" ]; then
                src="$1"
            elif [ -z "$dest" ]; then
                dest="$1"
            else
                die "Invalid argument count"
            fi
            ;;
    esac
    shift
done

if [ -z "$src" ] || [ -z "$dest" ]; then
    die "Invalid argument count"
fi

mkdir -p `dirname "$dest"` || die "Couldn't create destination directory"
[ -f "$src" ] || die "Source file doesn't exist"
[ -n "$verbose" ] && echo "${src} -> ${dest}"
cp "$src" "$dest" || exit 1
[ -n "$mode" ] && chmod "$mode" "$dest"

