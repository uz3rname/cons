#!/bin/sh

ok=''
report_str=''
failed=0
total=0
skipped=0

report() {
    case "$2" in
        ret)
            report_str="${report_str}${1}: wrong return value (expected ${3}, got ${4})\n"
            ;;
        output)
            report_str="${report_str}${1}: incorrect output (expected \"${3}\", got \"${4})\"\n"
            ;;
        script)
            report_str="${report_str}${1}: test script terminated with error\n"
            ;;
    esac
    case "$2" in
        null)
            echo -e "\033[33mskip\033[0m"
            skipped=$((skipped + 1))
            ;;
        ok)
            echo -e "\033[32mok\033[0m"
            ;;
        *)
            failed=$((failed + 1))
            echo -e "\033[31mfail\033[0m"
            ;;
    esac
}

read_param() {
    grep "${1}[ ]*=" "$2" | sed 's/^[^=]*=[ ]*//'
}

echo
echo '>>> Running tests'
echo

for f in $@; do
    printf "%-30s" "$f"
    ok='null'
    total=$((total + 1))
    expected=$(read_param result "$f.cons")
    output=$(read_param output "$f.cons")
    input=$(read_param input "$f.cons")
    if [ -x "${f}_test.sh" ]; then
        ./${f}_test.sh >/dev/null 2>&1
        if [ $? -eq 0 ]; then
            ok='ok'
        else
            report "$f" script
            continue
        fi
    else
        if [ -n "$output" ] || [ -n "$expected" ]; then
            if [ -z "$input" ]; then
                got=$(./$f)
            else
                got=$(echo "$input" | ./$f)
            fi
            result=$(echo $?)
            if [ -n "$expected" ]; then
                if [ "$expected" != "$result" ]; then
                    report "$f" ret "$expected" "$result"
                    continue
                else
                    ok='ok'
                fi
            fi
            if [ -n "$output" ]; then
                if [ "$output" != "$got" ]; then
                    report "$f" output "$output" "$got"
                    continue
                else
                    ok='ok'
                fi
            fi
        fi
    fi
    report "$f" "$ok"
done

echo
printf "%-30s %d\n" "Total tests:" "$total"
printf "%-30s %d\n" "Failed:" "$failed"
printf "%-30s %d\n" "Skipped:" "$skipped"
if [ $failed -ne 0 ]; then
    echo 
    echo "Details:"
    echo
    echo -e $report_str
    exit 1
fi

