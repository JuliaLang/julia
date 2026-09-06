#!/bin/bash
# The zero-cost sweep: a julia that carries the region runtime, with no
# region in use, against a vanilla julia built from the same base, on the
# GCBenchmarks suite (https://github.com/JuliaCI/GCBenchmarks).
#
#   gcbench.sh <vanilla julia> <region julia> <GCBenchmarks checkout> [rounds]
#
# Six serial benchmarks run on one thread, five multithreaded ones on four.
# The two binaries alternate inside every round, so a change of the
# machine's load shows in both. Each run is one process; its wall time is
# the `times` field that the suite's @gctime macro prints, a UInt64 in
# hexadecimal (`times = 0x0000000075205ff5`). When REGIONS_TSV names a
# file, one row per run goes there:
#
#   set  bench  threads  binary  round  wall_ns
#
# results/plot.py takes the minimum over the rounds and draws the ratio.
# A run that prints no `times` field is reported as FAILED with the tail of
# its output and gets no row. The suite aborts a run itself when the stock
# collector reports memory pressure three times in ten seconds
# (gc_cb_on_pressure in util/utils.jl); on a machine where a benchmark
# thrashes the collector, that abort hits both binaries alike.
# CORE (default 29) pins the one-thread runs; MTCORES (default 24-27) the
# four-thread runs. GCBENCH_SCALE passes through to the suite (1 = full).
set -uo pipefail
VANILLA=$1; REGION=$2; SUITE=$3; ROUNDS=${4:-5}
CORE=${CORE:-29}; MTCORES=${MTCORES:-24-27}
SERIAL="append/append.jl linked/tree.jl strings/strings.jl bigint/pollard.jl \
        big_arrays/single_ref.jl big_arrays/many_refs.jl"
MULTI="binary_tree/tree_mutable.jl mergesort_parallel/mergesort_parallel.jl \
       mm_divide_and_conquer/mm_divide_and_conquer.jl big_arrays/objarray.jl \
       big_arrays/issue-52937.jl"

row() { [ -n "${REGIONS_TSV:-}" ] || return 0
        [ -s "$REGIONS_TSV" ] || printf '# set\tbench\tthreads\tbinary\tround\twall_ns\n' > "$REGIONS_TSV"
        printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$@" >> "$REGIONS_TSV"; }

# one <set> <bench> <threads> <binary name> <julia> <round>
one() {
    local set=$1 bench=$2 threads=$3 name=$4 julia=$5 round=$6
    local dir="$SUITE/benches/$set/$(dirname "$bench")" cores=$CORE
    [ "$threads" -gt 1 ] && cores=$MTCORES
    local out
    out=$(timeout 900 taskset -c "$cores" "$julia" --startup-file=no --project="$dir" \
              --threads="$threads" "$dir/$(basename "$bench")" 2>&1)
    local ns
    ns=$(printf '%s\n' "$out" | grep -oE 'times = (0x[0-9a-fA-F]+|[0-9]+)' | head -1 | sed 's/times = //')
    if [ -z "$ns" ]; then
        echo "FAILED $name $set/$bench round $round:" >&2
        printf '%s\n' "$out" | tail -12 >&2
        return 1
    fi
    ns=$((ns))  # bash reads the 0x prefix; the row holds decimal nanoseconds
    echo "$name $set/$bench threads=$threads round $round $ns"
    row "$set" "$bench" "$threads" "$name" "$round" "$ns"
}

for round in $(seq 1 "$ROUNDS"); do
    for bench in $SERIAL; do
        one serial "$bench" 1 vanilla "$VANILLA" "$round"
        one serial "$bench" 1 regions "$REGION" "$round"
    done
    for bench in $MULTI; do
        one multithreaded "$bench" 4 vanilla "$VANILLA" "$round"
        one multithreaded "$bench" 4 regions "$REGION" "$round"
    done
done
echo "gcbench: done"
