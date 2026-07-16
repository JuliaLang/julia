# Image-code reuse for sysimage builds (prototype) — final architecture

Goal: a `--output-o` (JuliaC-style) sysimage build reuses the machine code of
CodeInstances that are already compiled into loaded images (sysimage /
pkgimages), instead of re-running LLVM over them. Measured prize: 55-75% of the
CIs in typical app builds are image-resident; skipping their emission saved
~35% wall / 162MB object on `using Plots`.

## Principles (converged with Cody over three design iterations)
1. All tables (fvar/gvar) and all serialized records are REBUILT by the new
   image with donor functions/objects as ordinary, fairly-linked entries.
   Only machine code is not regenerated.
2. Donor CodeInstances serialize and deserialize as completely normal CIs —
   nothing donor-shaped exists in the image format or at load time.
3. The loader is untouched. The unlinker is Julia-agnostic ELF surgery.

## Producer side
- Pkgimages link with `--emit-relocs` (env `JULIA_IMAGE_EMIT_RELOCS=1`, one
  flag in base/linking.jl). Cost measured on sys.so: +11.6% disk for the
  needed relocs (rest is .rela.debug_*, droppable), zero runtime cost.
- `jl_system_image_data` gets a dedicated `.jlsysdata` section so donors'
  dead serialized blobs can be dropped when their code is reused.
- (sys.so as donor: link sys-o.a with `-q` as well.)

## Build (JULIA_REUSE_IMAGE_CODE=1, Linux/ELF)
In jl_emit_native_to_output (src/aotcompile.cpp):
1. Plan: enumerate loaded images via the debug registry (CI ↔ fvar slot ↔
   code address). A FROM_IMAGE CI with max_world==typemax whose entry points
   resolve to symbols in a usable donor (-q relocs present, cpu target match)
   is planned for reuse.
2. Reused CIs enter `out.ci_funcs` as extern Function declarations of the
   donor's (prefixed, dup-uniquified) symbols → standard fvar table + fptr
   record pipeline; calls from fresh code bind directly at link time.
3. Donor heap-object gvar slots become ordinary entries in the new image's
   gvar table/record (extern GlobalVariable decls named by synthesized LABELs);
   the loader re-points them at this image's serialized objects normally.
4. Donor GOT-style slots — cross-image call slots and multiversioning clone
   slots — are filled at static link time (ABS64) with their live values'
   symbols (snapshotting the builder's target selection). Recorded as
   LABEL/BIND lines in a manifest (<output-o>.reuse).
5. Fresh functions referenced by donor slots are re-promoted to
   external-hidden at jl_dump_native (jl_create_native_impl internalizes and
   safe-name-mangles everything post-emission; BIND targets are captured
   post-mangling via makeSafeNameStr).

## Driver (scripts/e2e prototype)
julia --output-o app.o (reuse on) → for each DONOR line: unlink.jl donor.so →
donor.o (--prefix, --bind=manifest) → link app.o + donor .o's + libjulia into
app.so. Boot with -J app.so.

## Unlinker (unlink.jl, ~700 lines, Julia-agnostic)
--emit-relocs .so → ET_REL .o: keep alloc sections; drop linker/loader-owned
sections + .jlsysdata; crt carcasses localized; _GLOBAL_OFFSET_TABLE_/_DYNAMIC
→ UND; per-image runtime slots (jl_small_typeof, jl_pgcstack_*, jl_tls_offset)
→ UND (bind to consuming image's copies); promote+prefix defined syms
(.sym<idx> dedup rule shared with the builder); NOBITS materialized (BIND
targets .bss); .eh_frame rebuilt (drop reloc-less FDEs + terminator, remap CIE
pointers); LABEL defines synthesized syms at donor VAs; BIND applies ABS64
slot fills. Roundtrip of full sys.so: unlink 3.6s, relinked image boots and
passes smoke (gmp/pcre/BLAS/threads/JIT/GC/backtraces).

## Measured results (prototype, Zen4 Linux, native target, 8 emission threads)
Full pipeline (build + unlink + link + boot smoke), 3 interleaved OFF/ON pairs,
after the selection fast-path and the invalidation work below:
- no-op sysimage rebuild: 65s -> 8.0s (8.1x); 29,760 machine-code CIs reused,
  5 emitted (99.98% reuse). Excluding donor unlinking (cacheable): 2.6s (25x).
- `using Plots` sysimage (210-package dep tree): 128s -> 19.0s (6.7x);
  39,296 machine-code CIs reused, 171 emitted (99.57% reuse; the remainder is
  genuine invalidation fallout of package loading). Excluding unlinking:
  11.1s (11.5x). The image is a proper nonincremental sysimage (code .text
  28.8MB vs stock 16.2MB, heap 325MB vs 153MB embedded), boots interactively,
  `using Plots` is 0.0s and first-plot 0.16s vs 1.53s/0.17s on stock+pkgimages.
- Reuse-arm phase profile (Plots): julia 9.5s (selection 0.9, emission 0.3,
  heap serialization ~2.8, dump 1.5), unlink 7.8s, link 1.2s, boot 0.5s.
  Compilation is no longer the bottleneck; unlinking and heap serialization are.
- --emit-relocs producer cost on sys.so: +11.6% disk for the required reloc
  sections (rest is .rela.debug_*), zero runtime memory cost.
- Reuse-built .so is smaller than baseline only because the unlinker drops
  donor DWARF (fixable omission); .text is within 2MB of baseline.

## Invalidation work (what made 99%+ reuse possible)
Reuse can only be as good as image-code validity. Invalidation attribution
found and fixed three sources that re-derived thousands of CIs on every
`using Plots`:
1. Typeinf-world pinning misread as invalidation (finite max_world containing
   jl_typeinf_world is by construction) — eligibility/twin-skip made
   world-range-aware, and typeinf-world enqueueing made evidence-based
   (a root is compiled for the frozen world only with world-range evidence or
   as a cache-less compiler method; external interpreters' compiler
   specializations cached by pkgimages never qualify).
2. REPL's REPLInterpreter extending the AbstractInterpreter interface
   invalidated ~2,900 compiler CIs (the bootstrap-pre-inferred abstract core
   holds open method-match edges). Fixed with interface barriers: isa-guarded
   concrete fast path + Core.invokelatest to a @noinline single-method barrier
   (no edge -> no transitive cascade; plain @noinline barriers do NOT work,
   invalidation propagates through CI backedges). Now 7 CIs invalidated.
3. ColorVectorSpace's promote_rule(::Type{<:Real}, ::Type{<:AbstractGray})
   invalidated 1,160 CIs via two abstract promote_rule edges. Fix: flip the
   argument order (promote_type consults both; values identical, but a
   Gray-first signature cannot intersect concrete-Real-first edges). See
   ColorVectorSpace-promote_rule.patch (upstream PR candidate).
Residual 171 emissions: ~250-victim JSON.PtrString convert speculation break
plus smaller FixedPointNumbers / RelocatableFolders / SparseArrays / etc.
triggers — legitimate compiler speculation losses, not package bugs.

## Tools
- reuse-build.sh — end-to-end driver (PHASE timing stamps, boot smoke test,
  JULIA_REUSE_IMAGE_CODE toggleable for baselines).
- bench.sh — interleaved A/B (default emission vs reuse) with per-arm stats.
- unlink.jl — the ELF unlinker.

## Split heap images (JuliaLang/julia#61649)
The branch incorporates PR 61649: with SPLIT_JI=1 the drivers pass
--output-ji alongside --output-o, so the serialized heap is written directly
to the .ji instead of round-tripping through an LLVM constant array in the
object. A no-op rebuild then produces a 32MB .so (code + tables only) plus a
160MB .ji, booted via -J app.so with the sibling .ji; pipeline time is
unchanged (~18.5s) but peak memory drops and the heap write becomes a plain
stream — the intended substrate for delta/mmap heap-serialization tricks.
Reuse works identically in both modes.

## Known limitations (prototype)
- Multi-target output images: donor clone slots are snapshot-bound to the
  builder's selected target; folding donor slots into the image's own clone
  tables would restore load-time dispatch (follow-up).
- macOS/Windows: fall back to full emission (ELF-only unlink).
- Dropped-slot safety: cross-image call slots and clone slots are snapshot by
  symbol even when the callee was not selected for the image (the live value
  is era-consistent donor code); a slot is only left null if its target
  cannot be attributed to any donor symbol at all.
