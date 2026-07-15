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

echo "--- building object (reuse enabled)"
env JULIA_REUSE_IMAGE_CODE=1 JULIA_IMAGE_THREADS=1 \
    "$JULIA" --startup-file=no -J "$SYS" --cpu-target=native \
    --output-o "$OUT_O" --output-incremental=no \
    -e "$PRE $APP; nothing"

DONOR_OBJS=()
if [ -f "$OUT_O.reuse" ]; then
    echo "--- unlinking donor images"
    while IFS=$'\t' read -r tag a b; do
        [ "$tag" == "DONOR" ] || continue
        obj=${OUT_SO%.so}_donor_${a%_}.o
        "$JULIA" --startup-file=no "$HERE/unlink.jl" "$b" "$obj" \
            --prefix="$a" --bind="$OUT_O.reuse"
        DONOR_OBJS+=("$obj")
    done < "$OUT_O.reuse"
fi

echo "--- linking"
c++ -shared -fPIC -o "$OUT_SO" \
    -Wl,--whole-archive "$OUT_O" -Wl,--no-whole-archive \
    ${DONOR_OBJS[@]+"${DONOR_OBJS[@]}"} \
    -L"$BINDIR/../lib" -L"$BINDIR/../lib/julia" -ljulia-internal -ljulia
echo "built $OUT_SO"
