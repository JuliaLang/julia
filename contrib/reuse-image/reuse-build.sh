#!/bin/bash
# Prototype driver for image-code reuse in sysimage builds.
#
# Builds a sysimage for APP (a Julia expression, e.g. "using Plots") while
# reusing the machine code of CodeInstances already compiled into loaded
# images (sysimage / pkgimages), instead of re-running LLVM over them.
#
# Requirements:
#   * Donor images must carry static relocations: precompile packages with
#     JULIA_IMAGE_EMIT_RELOCS=1, and link the sysimage used via -J with -q
#     (e.g. relink sys-o.a adding -Wl,-q) so Base code can be reused too.
#   * Linux/ELF, x86-64.
#
# Usage:
#   JULIA=path/to/julia [SYS=path/to/sys-q.so] [JULIA_PROJECT=...] \
#       reuse-build.sh <output.so> <app-expr>
set -euo pipefail
HERE=$(dirname "$(readlink -f "$0")")
JULIA=${JULIA:?set JULIA to the julia executable of a reuse-enabled build}
BINDIR=$(dirname "$(readlink -f "$JULIA")")
SYS=${SYS:-$BINDIR/../lib/julia/sys.so}
OUT_SO=$1
APP=$2
OUT_O=${OUT_SO%.so}.o

PRE='Sys.__init__(); Base.reinit_stdio(); Base.init_depot_path(); Base.init_load_path(); Base.init_active_project();'

stamp() { echo "PHASE $1: $(date +%s.%N)"; }
stamp "julia start"
echo "--- building object (reuse=${JULIA_REUSE_IMAGE_CODE:-1})"
env JULIA_REUSE_IMAGE_CODE="${JULIA_REUSE_IMAGE_CODE:-1}" JULIA_IMAGE_THREADS="${JULIA_IMAGE_THREADS:-8}" \
    "$JULIA" --startup-file=no -J "$SYS" --cpu-target=native \
    --output-o "$OUT_O" ${SPLIT_JI:+--output-ji "${OUT_SO%.so}.ji"} --output-incremental=no \
    -e "$PRE $APP; nothing"

stamp "build done"
DONOR_OBJS=()
if [ -f "$OUT_O.reuse" ] && grep -q "^DONOR" "$OUT_O.reuse"; then
    echo "--- unlinking donor images"
    DONORDIR=${OUT_SO%.so}_donors
    rm -rf "$DONORDIR" && mkdir -p "$DONORDIR"
    "$JULIA" --startup-file=no --threads=8 "$HERE/unlink.jl" \
        --batch="$OUT_O.reuse" --outdir="$DONORDIR"
    while IFS= read -r obj; do DONOR_OBJS+=("$obj"); done < <(ls "$DONORDIR"/donor_*.o)
fi

stamp "unlink done"
echo "--- linking"
c++ -shared -fPIC -o "$OUT_SO" \
    -Wl,--whole-archive "$OUT_O" -Wl,--no-whole-archive \
    ${DONOR_OBJS[@]+"${DONOR_OBJS[@]}"} \
    -L"$BINDIR/../lib" -L"$BINDIR/../lib/julia" -ljulia-internal -ljulia
stamp "link done"
echo "built $OUT_SO"
echo "--- boot smoke test"
"$JULIA" --startup-file=no -J "$OUT_SO" -e '
@assert 2 + 2 == 4
try; error("probe"); catch; @assert length(catch_backtrace()) > 3; end
println("SMOKE OK")'
stamp "boot test done"
