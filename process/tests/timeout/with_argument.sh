#!/bin/bash

if [ $# -eq 0 ]; then
    echo "error: enter some argument" >&2
    exit 1
fi

echo "You have passed: $1"
