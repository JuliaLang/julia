# [Session serialization and restore (design)](@id dev-sessions)

!!! note
    This is a design document for a proposed feature, together with notes from its
    proof-of-concept implementation (`save_session` / `julia --restore`), which
    lives on this branch. File and function references describe the machinery the
    design builds on, as of the time of writing.

## Goal

Allow a running interactive Julia session to be saved to disk and restored later as a
new process:

```julia-repl
julia> save_session()  # writes ~/.julia/sessions/v1.14/2026-08-17T12-30-05.ji and exits
```

```
$ julia --restore              # restores the most recent saved session
$ julia --restore=<file>       # restores a specific session file
```

After restore, the contents of `Main` (globals, functions, structs, modules), the
modules brought into scope with `using`, and every method defined during the session
are back, the session's packages are loaded, and the REPL continues. Operating-system
resources (open files, sockets, timers, running tasks, memory-mapped arrays, foreign
pointers) do not survive; see [What is not preserved](@ref sessions-not-preserved).

## Approach: an incremental session overlay

Four implementation strategies were considered:

1. **OS-level process checkpointing** (CRIU and similar). Rejected: Linux-only. There
   is no equivalent mechanism on Windows or macOS, and even on Linux, resources
   outside the process (devices, GPU state, some socket kinds) cannot be reliably
   restored.
2. **The `Serialization` stdlib.** Rejected as the core mechanism: named functions,
   structs, and modules defined in `Main` are serialized as name references only
   (`should_send_whole_type` in `stdlib/Serialization/src/Serialization.jl` sends full
   definitions only for anonymous function types), so restoring into a fresh process
   throws `UndefVarError` for every user definition. It is a value store, not a
   definition store.
3. **A flat, heap-only system image of the whole session**, booted in place of the
   sysimage. This was the first prototype: it is the smallest change to the
   system-image serializer (`src/staticdata.c`), and it proved out the save-side
   semantics, but it is not viable as the actual feature; see
   [The flat-image variant](@ref sessions-flat) for what it taught.
4. **An incremental session overlay**: write only the session-created state as an
   incremental image against the sysimage and loaded pkgimages, using the
   [package image](@ref pkgimages) machinery; restore boots a normal `julia` and
   loads the overlay the way a pkgimage loads. This is the implemented design.

Because the serialization format is the one used by [system images](@ref dev-sysimg)
and package images, which already work on all supported platforms, the feature is
platform-neutral by construction.

## Save path: `save_session()`

The overlay's worklist cannot be `Main` itself: `Main` is an object inside the
sysimage, and the incremental writer treats in-image objects as external references
(`jl_needs_serialization` returns early on `jl_object_in_image`), so a worklist of
`[Main]` serializes essentially nothing, and `Main`'s bindings would come back as
empty uniqued references besides. Runtime-created modules have none of these
problems: they serialize completely, with their bindings, as worklist roots.

`save_session` therefore *projects* the session state into a fresh module and saves
that:

* A throwaway `Main`-parented module gets one const, `SESSION_STATE`, holding the
  `(name, isconst, value)` triples of every binding `Main` owns, plus the
  `fullname` paths of the modules in `Main`'s `using` scope. Everything
  session-created is reachable from it. References *to* in-image objects (a user
  type's `.module` field, globalrefs in method IR, package types) serialize as
  sysimage/pkgimage linkage and resolve to the live objects on restore.
* Methods defined during the session are collected as external-extension methods
  ("extext", the pkgimage mechanism for methods extending functions the image does
  not own) by **object identity** rather than by claimed module: a REPL-defined
  method has `m.module === Main`, which the stock filter in
  `jl_collect_methcache_from_mod` (`src/staticdata_utils.c`) would skip because
  `Main` is in-image. Under session save, any still-valid method object that is not
  itself in an image is collected. This also captures `@eval Base ...` definitions.
* `jl_save_session_overlay` (`src/staticdata.c`) then calls
  `jl_create_system_image` with the projection module as the worklist and no native
  data: a heap-only incremental image, written with the standard cache-file header
  and trailing checksum so the standard loading machinery accepts it.

### Saving is a terminal act

`jl_save_system_image_to_stream` destroys live-heap state as it writes: it prunes
type caches and backedge lists in place. All serializer state is file-static and
non-reentrant, and the GC is disabled for the entire write. This is fine for a
process about to exit and fatal for one that keeps running, so `save_session` has
suspend-to-disk semantics: it writes the image and exits the process. A failure
inside the writer is likewise fatal (the heap is already damaged when it can fail).
Save-and-continue is possible future work; see [Future work](@ref sessions-future).

### Task policy

The serializer refuses Task objects (`jl_error("Task cannot be serialized")` in
`src/staticdata.c`), and a live session always has reachable tasks: listener tasks,
REPL tasks, whatever the user stored in globals. Under session save, every reachable
Task serializes as a reference to a single placeholder task that was never started
and is already done, written with its full C struct (GC-visible pointer fields
relocated, the hidden runtime tail zeroed with `tid == -1`). Restored task
references therefore satisfy every `istaskdone` guard in Base instead of aliasing a
live task; the root task keeps its existing special case and resolves to the
restoring process's root task. Waiting or running tasks lose their identity and
their stacks; this is the documented contract, since there is no portable way to
snapshot a native stack.

### Pointers, handles, and finalizers

`Ptr` fields of mutable structs are reset to `C_NULL` at write time (with `-1`
preserved for `MAP_FAILED`/`INVALID_HANDLE`), and the same rule covers
`Memory{Ptr{T}}` elements. The nulling is silent and incomplete: pointers stored as
integers or inside byte buffers are written verbatim (the canonical hazard is
`IOStream`, whose `ios::Vector{UInt8}` field is a raw `ios_t` struct containing a
`char*`). Finalizers and WeakRefs are not serialized at all (a long-standing TODO in
`src/staticdata.c`). The contract is therefore that OS-backed objects (`IOStream`,
`TTY`, `Timer`, `Process`, mmapped arrays, `Libdl` handles) do not survive a save;
making their failure modes louder (a pre-save warning walk, closed-state sentinels
via the `jl_set_precompile_field_replace` seam) is future work.

## Restore path: `julia --restore`

`--restore[=file]` does not change how the process boots: boot is a completely
normal sysimage boot, native code and all, and the C side only rejects conflicting
options (`-J`, `--output-*`). The session file is resolved in Base, where
`DEPOT_PATH` is authoritative: with no argument, the newest `*.ji` in the
`sessions` directory of any configured depot is chosen.

The driver, `Base.restore_session` (`base/sessions.jl`), runs from `exec_options`
after `startup.jl` and follows the pkgimage loading sequence:

1. Validate the cache header and file checksum, and read the dependency list
   (`parse_cache_header`).
2. Load each dependency at its recorded build id (`_tryrequire_from_serialized`):
   the overlay references the sysimage and pkgimages by identity, so the same images
   must be present before it loads. A changed package version or environment fails
   here with a build-id mismatch.
3. `jl_restore_incremental`, followed by the standard Julia-side revalidation
   (`ReinferUtils.insert_backedges_typeinf`); extext methods are inserted and
   activated by the C side, exactly as for a pkgimage.
4. Replay `SESSION_STATE` into the live `Main`: const bindings via
   `Core.eval(Main, Expr(:const, ...))`, non-const via `setglobal!`, then the
   `using` paths. No `Core.Main` rebinding and no module merging is needed; the
   projected values already reference the live `Main` through image linkage.

Nothing needs reinitialization: `Base.__init__` and every package `__init__` ran in
this process the normal way. One parity consequence: restore loads the session's
packages before the REPL starts, so packages whose `__init__` is sensitive to REPL
timing (for example, plotting packages that splice a GUI display into
`Base.Multimedia.displays` relative to the REPL's display) behave exactly as they do
when loaded from `startup.jl`, not as they do when loaded interactively.

REPL history needs no special handling: it already persists separately in
`logs/repl_history.jl`.

### Coverage and its limits

Preserved: `Main`'s bindings and `using` scope, session-defined types and methods
wherever they were defined (including into `Base` or packages), inferred IR for
session-defined code (re-JITs on first use), loaded packages via reload at matching
build ids.

Limits that follow from the overlay format:

* New or rebound globals in `Base`/package modules are dropped: in-image modules'
  bindings serialize as empty uniqued references by pkgimage design. Recoverable
  later by scanning for binding partitions created after boot and replaying them
  like `Main`'s.
* In-place mutations of package-owned objects (e.g. `push!` into a package's const
  table) are not capturable by an overlay at all; the reachability walk never enters
  in-image objects. Only a whole-heap snapshot carries those.
* Method *deletions* of image methods do not replay.
* Session-triggered inference of image-owned methods is not preserved (redone lazily
  after restore); carrying it needs `jl_precompile_toplevel_module`-style root
  keying from session start.
* Loading the same overlay twice into one process is unsupported (`jl_copy_roots`
  aborts on a duplicate worklist key).

## [The flat-image variant](@id sessions-flat)

The first prototype wrote the *entire* session heap as a full, native-code-free
`.ji` image (`jl_save_session_image`, kept in-tree for comparison) and booted from
it in place of the sysimage. It works, and it captures things the overlay cannot
(package-module globals, mutations of package-owned objects), but it fails as an
interactive feature on two counts discovered by measurement:

* Booting from a session image re-runs no module initializers (the session already
  ran them), so all OS-backed state (stdio, displays, load paths, listener tasks)
  must be re-established by a bespoke reinitialization path, distinct from both a
  normal boot and `__init__` replay.
* CodeInstances copied out of the sysimage and pkgimages mostly carry no inferred IR
  (native-code images drop it at build time), so a restored session pays full
  re-inference plus codegen for nearly all of Base and the REPL, and nothing caches
  the result across restores.

Measured on the same session, flat vs overlay: ~190MB vs ~10KB file, ~110s vs ~0.4s
restore, unusably slow vs fully responsive REPL. That gap is why the overlay is the
design and the flat form is a proving ground.

## Storage and validation

Session files live in the depot under a new Base-owned directory, versioned at
minor-release granularity like the compile cache:

```
~/.julia/sessions/v1.14/<timestamp>[-<name>].ji
```

Sessions may be named (`save_session("mywork")`), and `--restore=<name>` finds
the newest session whose name matches exactly, by prefix, or as a substring of
the file name (so timestamp fragments work too).

* Writes go to `DEPOT_PATH[1]` only; `--restore`'s most-recent search reads across
  all depot entries, matching the existing write-one/read-all depot convention.
* The overlay uses the standard incremental cache-file header, which for package
  images already records and checks the format version, platform, Julia version, GC
  ABI, and the build's git branch and commit, so a session file is restorable only
  by the identical Julia build, with no additional mechanism.
* Still to do: write-to-temporary-then-rename (the PoC writes the file directly),
  graceful degradation on a read-only depot, and the depot layout documentation in
  `base/initdefs.jl`.

## Command-line plumbing

`--restore[=file]` follows the `--worker[=cookie]` pattern in `src/jloptions.c`: a
flag field plus an optional string field in `jl_options_t`, mirrored in
`base/options.jl` (checked by a size assertion at sysimage build time). With GNU
getopt, optional arguments require the `--restore=file` spelling; the handler must
tolerate `optarg == NULL`.

`--restore` composes with other options as follows: `-J`/`--sysimage` and
`--output-*` together with `--restore` are errors; `-e`/`-E`/`-L` and the REPL run
after the state is replayed; `julia_cmd()` does not propagate `--restore` to
subprocesses.

At the REPL, Ctrl-X Ctrl-S on an empty prompt saves the session and exits, as the
save-and-exit parallel to Ctrl-D. Ctrl-Shift-D does the same in terminals that
encode it distinctly (CSI-u / modifyOtherKeys); most terminals send plain Ctrl-D
for it, and having the REPL request an enhanced keyboard encoding is not viable
piecemeal, since the kitty protocol's disambiguate flag re-encodes every
Ctrl-letter key and would break all existing bindings until LineEdit understands
those encodings wholesale.

## [What is not preserved](@id sessions-not-preserved)

* Running or waiting tasks: unfinished task references restore as one completed
  placeholder task (finished tasks keep their result and exception state, via a
  placeholder per task).
* Open files, sockets, pipes, timers, and child processes (defunct after restore).
* Memory-mapped arrays and raw pointers of any kind, including `Libdl` handles.
* Finalizers on restored objects.
* New or rebound globals in `Base`/package modules, and in-place mutations of
  package-owned objects (see [Coverage and its limits](@ref)).
* Docstrings attached to `Main` definitions (the doc metadata table is not yet
  projected).
* Native code for session-defined methods (re-JIT'ed on demand; a latency cost, not
  a correctness cost).

This matches the contract of every practical image-based system (Smalltalk images,
R's `save.image`): OS resources never survive; the design goal is that their loss is
loud rather than silent.

## [Future work](@id sessions-future)

* **Capture of session changes to `Base`/package globals**, by scanning binding
  partitions created after boot.
* **Save-and-continue**, either by making the writer non-destructive or by forking
  on POSIX platforms (Windows would still exit, or use the non-destructive writer).
* **Deeper handle sanitization.** `save_session` warns about non-survivable
  state before writing (`Base.session_save_report`), session images reset every
  raw `Ptr` value, and restore replaces top-level `IOStream`s with closed
  streams; still missing are replacement of handles nested inside immutable
  containers and closed-state sentinels for the remaining libuv handle types.
* **Docstring projection** for `Main` definitions.
* **Preserving session-triggered inference of image methods**, via session-keyed
  method roots.
* **Objcache key stability for restored code.** The global LLVM compilation cache
  (#61527) already stores the saving session's JIT output and is consulted after
  restore, but restored CodeInstances mostly derive different cache keys (measured
  2 of 19 hits on a small session), so the code recompiles. Making restored
  CodeInstances key-stable would eliminate the post-restore re-JIT cost without
  storing any native code in session files.
* **Re-restorable overlays** (a fresh worklist key per load, so the same session
  file can be restored twice into one process).

## Status

Implemented in the proof of concept: the overlay save and restore paths described
above, the `--restore` CLI and depot resolution, the Ctrl-Shift-D binding, the
placeholder-task policy, and the flat-image variant for comparison. Not yet done:
tests, NEWS, manual and man-page entries for `--restore`, and the storage
hardening listed above.
