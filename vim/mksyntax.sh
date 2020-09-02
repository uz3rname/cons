#!/bin/sh

MOD_DIR="$1"
OUT="$2"

cat syntax/cons.vim.in > "$OUT"
find "$MOD_DIR" -type f -name '*.cons' |\
    xargs cat |\
    pcregrep -o1 -e "^\(def\s+([^ (]+)[ \n]*\(" |\
    sort |\
    uniq |\
    xargs -0 echo "syn keyword Function"|\
    tr $'\n' ' '|\
    sed 's/$/\n/' >> "$OUT"
find "$MOD_DIR" -type f -name '*.cons' |\
    xargs cat |\
    pcregrep -o1 -e "^\(extern\s+([^\n\s()]+)" |\
    sort |\
    uniq |\
    xargs -0 echo "syn keyword Function"|\
    tr $'\n' ' '|\
    sed 's/$/\n/' >> "$OUT"
find "$MOD_DIR" -type f -name '*.cons' |\
    xargs cat |\
    pcregrep -o1 -e "^\(macro\s+([^!]+)$" |\
    sort |\
    uniq |\
    xargs -0 echo "syn keyword Statement"|\
    tr $'\n' ' '|\
    sed 's/$/\n/' >> "$OUT"
find "$MOD_DIR" -type f -name '*.cons' |\
    xargs cat |\
    pcregrep -o2 -e "\((type|alias)\s+[\(]*([^\s]+)" |\
    sort |\
    uniq |\
    xargs -0 echo "syn keyword Type"|\
    tr $'\n' ' '|\
    sed 's/$/\n/' >> "$OUT"
find "$MOD_DIR" -type f -name '*.cons' |\
    xargs cat |\
    pcregrep -o2 -e "\((type|alias)\s+[\(]*([^\s]+)" |\
    sort |\
    uniq |\
    sed 's/^/\*/' |\
    xargs -0 echo "syn keyword Type"|\
    tr $'\n' ' '|\
    sed 's/$/\n/' >> "$OUT"
pcregrep -o1 -e "\s+[(']+([a-z0-9]+)[)]*$" ../bootstrap/inc/typedefs.scm |\
    sort |\
    uniq |\
    xargs -0 echo "syn keyword Type"|\
    tr $'\n' ' '|\
    sed 's/$/\n/' >> "$OUT"
pcregrep -o1 -e "\s+[(']+([a-z0-9]+)[)]*$" ../bootstrap/inc/typedefs.scm |\
    sort |\
    uniq |\
    sed 's/^/\*/' |\
    xargs -0 echo "syn keyword Type"|\
    tr $'\n' ' '|\
    sed 's/$/\n/' >> "$OUT"
find "$MOD_DIR" -type f -name '*.cons' |\
    xargs cat |\
    pcregrep -o1 -e "^\(macro\s+([^!]+)$" |\
    sort |\
    uniq |\
    tr '\n' ',' |\
    sed 's/,$/\n/; s/^/setl lispwords+=/' >> "$OUT"
cat<< EOF >> "$OUT"
setl lispwords-=cond
let b:current_syntax = "cons"
EOF


