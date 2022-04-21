#!/usr/bin/env bash

# Usage: See benchmark/README.md

# For bold output
function echobold {
    tput bold
    echo "$@"
    tput sgr0
}

# Read argument
benchcase=$1

# Prepare variables
revision=$(git rev-parse --short HEAD)
prefix="$benchcase-$revision"

echobold Selecting benchmark case...

# Set $exe and $datadir
case $benchcase in
    python)
        exe=./benchmark/naive.py
        datadir=./benchmark/data
        ;;
    rust)
        exe=./benchmark/rust/target/release/acbench-rust
        datadir=./benchmark/data
        ;;
    java)
        exe=./benchmark/java/bazel-bin/acbench
        datadir=./benchmark/data
        ;;
    haskell)
        exe=$(stack path --local-install-root)/bin/ac-bench
        datadir=./benchmark/data-utf8
        ;;
    rust-ffi)
        exe=$(stack path --local-install-root)/bin/ac-bench-ffi
        datadir=./benchmark/data-utf8
        ;;
    *)
        echobold "Invalid benchmark case, please pass one of {python,rust,java,haskell,rust-ffi}" >&2
        exit 1 
esac

# Absolute paths may help with debugging
exe_absolute=$(readlink -f "$exe")
datadir_absolute=$(readlink -f "$datadir")

# Print some debugging information
echobold "I will benchmark this executable:"
echo "$exe_absolute"
echobold "Using data from this directory:"
echo "$datadir_absolute"
echobold "And store the results in these files:"
echo "$prefix.{stats,results}"

# Run the benchmark
echobold "Running the benchmark..."
./benchmark/benchmark.py "$exe" --data-directory "$datadir_absolute" --prefix "$prefix"
echobold "Done!"