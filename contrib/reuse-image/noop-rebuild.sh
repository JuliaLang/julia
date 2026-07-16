#!/bin/bash
# No-op rebuild workflow: boot from a sysimage, run nothing, and re-emit an
# equivalent sysimage with image-code reuse enabled. Iterating this is the
# fixed-point test for the reuse machinery: generation N+1 should reuse the
# code generation N emitted, so fresh emission should collapse after gen 1.
#
# The input sysimage must carry static relocations (--emit-relocs / -q); each
# generation's output is linked with -q as well so it can donate to the next.
#
# Usage:
#   JULIA=path/to/julia SYS=path/to/sys-q.so noop-rebuild.sh <workdir> [ngens]
set -euo pipefail
HERE=$(dirname "$(readlink -f "$0")")
JULIA=${JULIA:?set JULIA to the julia executable of a reuse-enabled build}
BINDIR=$(dirname "$(readlink -f "$JULIA")")
SYS=${SYS:?set SYS to a sysimage linked with --emit-relocs}
WORK=${1:?workdir}
NGENS=${2:-2}
mkdir -p "$WORK"

PRE='Sys.__init__(); Base.reinit_stdio(); Base.init_depot_path(); Base.init_load_path(); Base.init_active_project();'

CUR=$SYS
for gen in $(seq 1 "$NGENS"); do
    OUT_O=$WORK/gen$gen.o
    OUT_SO=$WORK/gen$gen.so
    echo "=== generation $gen (boot image: $CUR) ==="
    env JULIA_REUSE_IMAGE_CODE=1 JULIA_REUSE_DEBUG=1 JULIA_IMAGE_THREADS="${JULIA_IMAGE_THREADS:-8}" \
        "$JULIA" --startup-file=no -J "$CUR" --cpu-target=native \
        --output-o "$OUT_O" ${SPLIT_JI:+--output-ji "${OUT_SO%.so}.ji"} --output-incremental=no \
        -e "$PRE nothing" 2>&1 | grep -E "jl_emit_native_to_output|jl_reuse_image_code:" || true

    DONOR_OBJS=()
    DONORDIR=$WORK/gen${gen}_donors
    rm -rf "$DONORDIR" && mkdir -p "$DONORDIR"
    "$JULIA" --startup-file=no --threads=8 "$HERE/unlink.jl" \
        --batch="$OUT_O.reuse" --outdir="$DONORDIR" > /dev/null
    while IFS= read -r obj; do DONOR_OBJS+=("$obj"); done < <(ls "$DONORDIR"/donor_*.o 2>/dev/null)
    echo "    donors: ${#DONOR_OBJS[@]}"

    # -q so this generation can donate its code to the next
    c++ -shared -fPIC -o "$OUT_SO" -Wl,-q \
        -Wl,--whole-archive "$OUT_O" -Wl,--no-whole-archive \
        ${DONOR_OBJS[@]+"${DONOR_OBJS[@]}"} \
        -L"$BINDIR/../lib" -L"$BINDIR/../lib/julia" -ljulia-internal -ljulia

    "$JULIA" --startup-file=no -J "$OUT_SO" -e '
@assert 2 + 2 == 4
try; error("probe"); catch; @assert length(catch_backtrace()) > 3; end
println("    gen'"$gen"' boots: OK (", filesize("'"$OUT_SO"'"), " bytes)")'
    CUR=$OUT_SO
done
