#!/bin/bash
# Interleaved A/B of default emission vs image-code reuse.
#   JULIA=... [SYS=...] bench.sh <name> <app-expr> [pairs=3]
# Runs `pairs` alternating off/on builds via reuse-build.sh (reuse toggled
# through JULIA_REUSE_IMAGE_CODE) and reports wall time, smoke status, sizes
# and reuse counters per arm. Interleaving controls for machine-load drift.
set -uo pipefail
HERE=$(dirname "$(readlink -f "$0")")
NAME=$1; APP=$2; PAIRS=${3:-3}
for ((i=1; i<=PAIRS; i++)); do
  for reuse in 0 1; do
    arm="${NAME}_$([ $reuse == 1 ] && echo on || echo off)$i"
    env JULIA_REUSE_DEBUG=1 JULIA_REUSE_IMAGE_CODE=$reuse \
        "$HERE/reuse-build.sh" "$arm.so" "$APP" > "$arm.log" 2>&1
    rc=$?
    smoke=$(grep -c "SMOKE OK" "$arm.log")
    size=$(stat -c%s "$arm.so" 2>/dev/null || echo -)
    mc=$(grep -oE "machine code [0-9]+ reused / [0-9]+ emitted" "$arm.log" | head -1)
    total=$(grep -E "^PHASE" "$arm.log" | awk -F': ' 'NR==1{t0=$2} END {printf "%.1f", $2-t0}')
    echo "ARM $arm: rc=$rc total=${total}s smoke=$smoke so=$size ${mc:-}"
  done
done
