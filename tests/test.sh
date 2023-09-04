#!/bin/bash

# 1) Copy all lc3tools-tests to LC3_TOOLS_ROOT/src/test/tests
# 2) Re-build lc3tools (cmake and make)
# 3) Run the unit tests with the corresponding input in codegen/input
# 4) yay!
# Must be a better way to do this

ROOT=/workspaces/lc3-compiler
TEST_DIR=$ROOT/tests 
LC3_TOOLS_ROOT=$ROOT/lc3tools

cp -a $TEST_DIR/lc3tools-tests/. $LC3_TOOLS_ROOT/src/test/tests/

(cd $LC3_TOOLS_ROOT/build/ &&  cmake -DCMAKE_BUILD_TYPE=Release .. && make)

for filename in $TEST_DIR/input/*.c; do
    # Run compiler on input to get asm
    echo lc3-compile -o $ROOT/tmp/out.asm $filename
    lc3-compile -o $ROOT/tmp/out.asm $filename

    # Run test on output asm
    echo $LC3_TOOLS_ROOT/build/bin/$(basename $filename .c) $ROOT/tmp/out.asm
    $LC3_TOOLS_ROOT/build/bin/$(basename $filename .c) $ROOT/tmp/out.asm
done


# CSMITH COMMAND:
# csmith --no-argc --no-arrays --no-bitfields --no-checksum --no-comma-operators --no-compound-assignment --no-consts --no-divs --no-embedded-assigns --no-pre-incr-operator --no-pre-decr-operator --no-post-incr-operator --no-post-decr-operator --no-jumps --no-longlong --no-int8 --no-uint8 --no-float --no-math64 --no-inline-function --no-muls --no-packed-struct --no-pointers --no-structs --no-unions --no-volatiles --no-const-pointers --no-global-variables --no-builtins