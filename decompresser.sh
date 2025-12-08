#!/bin/bash

# Default values for flags
visual=false

# Check if exactly two arguments are provided
if [ "$#" -lt 2 ]; then
    echo "Usage: $0 <input> <output> [--visual]"
    exit 1
fi

# Mandatory arguments
input_file="$1"
output_file="$2"
shift 2

# Parse optional flags
while [ "$#" -gt 0 ]; do
    case "$1" in
        --visual)
            visual=true
            shift
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done


make clean
make decompression
./decompression "$input_file" "$output_file" "$visual"