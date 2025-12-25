#!/bin/bash
# Generic pcc debug script
# Usage: pcc_debug.sh [options] <C code or file> [extra_args]
#   -ir    : Dump IR
#   -asm   : Dump assembly
#   -cmp   : Compare pcc asm with clang asm
#   -run   : Compile and run
#   -obj   : Compile to .o and show symbols
#   -link  : Try manual linking with clang
#   -v     : Verbose (show commands)
#   -func <name> : Show only named function
#   -grep <pattern> : Grep assembly for pattern
#   -I <dir> : Include directory
#   -L <dir> : Library directory
# Extra args after file are passed to compiler (e.g., libz.a)

PCC="${PCC:-pcc}"
WORKDIR=/tmp/pcc_debug_$$
mkdir -p $WORKDIR

# Parse options
DUMP_IR=0
DUMP_ASM=0
COMPARE=0
RUN=0
DUMP_OBJ=0
LINK_TEST=0
VERBOSE=0
FUNC_FILTER=""
GREP_PATTERN=""
INCLUDE_DIRS=""
LIB_DIRS=""

while [[ "$1" == -* ]]; do
    case "$1" in
        -ir) DUMP_IR=1; shift;;
        -asm) DUMP_ASM=1; shift;;
        -cmp) COMPARE=1; shift;;
        -run) RUN=1; shift;;
        -obj) DUMP_OBJ=1; shift;;
        -link) LINK_TEST=1; shift;;
        -v) VERBOSE=1; shift;;
        -func) FUNC_FILTER="$2"; shift 2;;
        -grep) GREP_PATTERN="$2"; shift 2;;
        -I) INCLUDE_DIRS="$INCLUDE_DIRS -I$2"; shift 2;;
        -L) LIB_DIRS="$LIB_DIRS -L$2"; shift 2;;
        *) echo "Unknown option: $1"; exit 1;;
    esac
done

# Remaining arg is the code/file
INPUT="$1"

if [ -f "$INPUT" ]; then
    TEST_FILE="$INPUT"
else
    # Treat as inline C code
    TEST_FILE="$WORKDIR/test.c"
    echo "$INPUT" > "$TEST_FILE"
fi

# Defaults if nothing specified
if [ $DUMP_IR -eq 0 ] && [ $DUMP_ASM -eq 0 ] && [ $COMPARE -eq 0 ] && [ $RUN -eq 0 ]; then
    DUMP_ASM=1
    RUN=1
fi

if [ $DUMP_IR -eq 1 ]; then
    echo "=== IR Dump ==="
    if [ -n "$FUNC_FILTER" ]; then
        $PCC --dump-ir -o /dev/null "$TEST_FILE" 2>&1 | awk "/define.*${FUNC_FILTER}\\(/,/^}/"
    else
        $PCC --dump-ir -o /dev/null "$TEST_FILE" 2>&1
    fi
    echo ""
fi

if [ $DUMP_ASM -eq 1 ]; then
    echo "=== Assembly ==="
    if [ -n "$FUNC_FILTER" ]; then
        $PCC -S -o - "$TEST_FILE" 2>&1 | awk "/_${FUNC_FILTER}:/,/\\.cfi_endproc/"
    elif [ -n "$GREP_PATTERN" ]; then
        echo "Searching for: $GREP_PATTERN"
        $PCC -S -o - "$TEST_FILE" 2>&1 | grep -n "$GREP_PATTERN"
    else
        $PCC -S -o - "$TEST_FILE" 2>&1 | head -100
    fi
    echo ""
fi

if [ $COMPARE -eq 1 ]; then
    echo "=== pcc Assembly ==="
    if [ -n "$FUNC_FILTER" ]; then
        $PCC -S -o - "$TEST_FILE" 2>&1 | awk "/_${FUNC_FILTER}:/,/\\.cfi_endproc/"
    else
        $PCC -S -o - "$TEST_FILE" 2>&1 | head -80
    fi
    echo ""
    echo "=== clang Assembly ==="
    if [ -n "$FUNC_FILTER" ]; then
        clang -S -o - "$TEST_FILE" 2>&1 | awk "/_${FUNC_FILTER}:/,/\\.cfi_endproc/"
    else
        clang -S -o - "$TEST_FILE" 2>&1 | head -80
    fi
    echo ""
fi

# Get extra args (everything after the input file)
shift
EXTRA_ARGS="$@"

if [ $DUMP_OBJ -eq 1 ]; then
    echo "=== Compile to .o ==="
    CMD="$PCC $INCLUDE_DIRS -c -o $WORKDIR/test.o $TEST_FILE"
    [ $VERBOSE -eq 1 ] && echo "$CMD"
    $CMD 2>&1
    if [ $? -eq 0 ]; then
        echo ""
        echo "=== Symbols in .o ==="
        nm $WORKDIR/test.o 2>&1
    fi
    echo ""
fi

if [ $LINK_TEST -eq 1 ]; then
    echo "=== Manual linking with clang ==="
    CMD="$PCC $INCLUDE_DIRS -c -o $WORKDIR/test.o $TEST_FILE"
    [ $VERBOSE -eq 1 ] && echo "Compile: $CMD"
    $CMD 2>&1
    if [ $? -eq 0 ]; then
        CMD="clang -o $WORKDIR/test_manual $WORKDIR/test.o $LIB_DIRS $EXTRA_ARGS"
        [ $VERBOSE -eq 1 ] && echo "Link: $CMD"
        $CMD 2>&1
        if [ $? -eq 0 ]; then
            echo "Manual linking succeeded!"
            echo ""
            echo "=== Running ==="
            "$WORKDIR/test_manual"
            echo "Exit code: $?"
        else
            echo "Manual linking failed"
        fi
    fi
    echo ""
fi

if [ $RUN -eq 1 ]; then
    echo "=== Compile and Run with pcc ==="
    CMD="$PCC $INCLUDE_DIRS $LIB_DIRS -o $WORKDIR/test $TEST_FILE $EXTRA_ARGS"
    [ $VERBOSE -eq 1 ] && echo "$CMD"
    $CMD 2>&1
    if [ $? -eq 0 ]; then
        "$WORKDIR/test"
        echo "Exit code: $?"
    else
        echo "Compilation failed"
    fi
    echo ""
    echo "=== Compile and Run with clang ==="
    CMD="clang $INCLUDE_DIRS $LIB_DIRS -o $WORKDIR/test_clang $TEST_FILE $EXTRA_ARGS"
    [ $VERBOSE -eq 1 ] && echo "$CMD"
    $CMD 2>&1
    if [ $? -eq 0 ]; then
        "$WORKDIR/test_clang"
        echo "Exit code: $?"
    else
        echo "Compilation failed"
    fi
fi

rm -rf $WORKDIR
