#!/usr/bin/env sh
set -e

odin build .

pushd listings

for filename in ./listing_*.asm; do
    LISTING_BIN="${filename%.*}"
    nasm -o $LISTING_BIN $filename
    echo $filename
    echo $LISTING_BIN
    TEMP_ASM=$(mktemp)
    TEMP_BIN=$(mktemp)
    ../sim8088 $LISTING_BIN > $TEMP_ASM
    set +e
    nasm -o $TEMP_BIN $TEMP_ASM
    if [ "$?" != "0" ]; then
        echo "Invalid code:"
        cat $TEMP_ASM
        exit 1
    fi
    cmp -l $LISTING_BIN $TEMP_BIN
    EXIT_CODE="$?"
    set -e
    if [ "$EXIT_CODE" != "0" ]; then
        echo $RESULT
        echo "Generated asm:"
        cat $TEMP_ASM
        echo "Original asm:"
        cat $filename
        echo
        echo "Diff:"
        diff $filename $TEMP_ASM
        exit 1
    fi
    rm $TEMP_ASM
    rm $TEMP_BIN
done

popd
