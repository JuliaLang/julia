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
Full pipeline (build + unlink + link + boot test), interleaved ON/OFF repeats:
- no-op sysimage rebuild: 64s -> 34s (1.9x); 31,168 machine-code CIs reused,
  1,982 emitted (94% reuse; remainder is the per-boot invalidation tail).
- `using Dates, Test, Statistics` (sysimage + stdlib donors): 71s -> 40s (1.8x),
  images pass extended functional tests.
- `using Plots` (47 donor images): 138s -> 90s (1.5x), 28,021 machine-code CIs
  reused; the resulting image renders plots correctly.
- --emit-relocs producer cost on sys.so: +11.6% disk for the required reloc
  sections (rest is .rela.debug_*), zero runtime memory cost.
- Reuse-built images are currently smaller than baseline only because the
  unlinker drops donor DWARF (a fixable omission); they also carry the donor's
  unreused code as dead text until gc-sections support lands.

## Known limitations (prototype)
- JULIA_IMAGE_THREADS=1 required: the module partitioner drops declaration
  entries from shard tables (fix: assign decls to partition 0).
- Multi-target output images: donor clone slots are snapshot-bound to the
  builder's selected target; folding donor slots into the image's own clone
  tables would restore load-time dispatch (follow-up).
- macOS/Windows: fall back to full emission (ELF-only unlink).
- Dropped-slot safety: unresolvable slots are left null (latent crash if
  executed); should fall back to rejecting the affected donor CIs instead.
