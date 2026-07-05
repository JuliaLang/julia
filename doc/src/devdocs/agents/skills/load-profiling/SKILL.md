---
name: load-profiling
description: How to profile Julia load/precompile time reliably — which instruments to trust for locating vs sizing vs deciding, the known tracy distortions (wandering GC, inclusive totals, attach overhead), and the A/B doctrine for wall-time claims.
---

# Reliable profiling of Julia load time

Hard-won methodology from the 2026 load-time optimization campaign. Three
multi-day "opportunities" (ScanNewMethods, ACTIVATE_Tag sig-walk, GC-defer)
were phantoms of instrumentation; each survived zone profiling and in-process
timers, and died only under same-binary same-depot A/B walls. Use the right
instrument for each question.

## The three questions and their instruments

| Question | Instrument | Never use |
|---|---|---|
| *Where* does time go? | tracy timeline (attached), zone names | — |
| *How big* is a phase? | **counts backend self-time, n≥5 plain runs** | single tracy capture totals |
| *Is it worth removing?* | **same-depot A/B walls (n≥8–10/arm)** | any zone number |

A zone number is a search hint, not a size. A size is an upper bound, not a
win. Only the A/B is a win.

## Instrument 1: the timing-counts backend (primary sizing tool)

Build with both backends (`Make.user`):

```
WITH_TRACY := 1
WITH_TIMING_COUNTS := 1
```

`WITH_TIMING_COUNTS` changes only C flags — **make does not track flag
changes**, so force it: `make -C src clean && make -j`. Set `JULIA_TIMINGS=1`
in the environment to get the `JULIA TIMINGS` table on stderr at exit (it is
opt-in because an unconditional dump breaks tests that assert on subprocess
stderr, e.g. test/precompile.jl's Iterators check), with per-event **Self**
and **Total** cycles (TSC ≈ 3.0 GHz on the current box; calibrate per machine via
ROOT-total / process lifetime; here 1 Mcyc ≈ 0.33 ms).

- **Self excludes children** — a GC pause that fires inside a zone lands in
  the GC/GC_Mark events, not the enclosing zone's self. Self is the
  GC-normalized number.
- Runs standalone: no profiler attach, no `JULIA_WAIT_FOR_TRACY`, ~zero
  distortion. The self column closes: Σ(self) ≈ LOAD_Require total ≈ wall.
- Always aggregate **n≥5 runs** with `scratch-tools/timings-parse.py
  run1.err run2.err ...` (mean ± sd per event). A number without an sd is a
  single sample of a lottery (see below).

## Instrument 2: tracy (timeline/locating only)

- `tracy-csvexport <trace>` reports **inclusive** totals: parents
  double-count children (GC, SUBTYPE, INTERSECT). Do not diff inclusive
  totals across traces — different zone-set configurations change parent
  totals with no change in work.
- `tracy-csvexport -e` gives self-times, but on the **attached** process,
  which runs ~13% slower overall and non-uniformly: per-zone-event cost
  scales with instance count (measured: SUBTYPE self +28%, INTERSECT_EnvS
  +41% at 314–811k instances; LOAD_Relocs only +10%).
- Even `-e` self is polluted by **unzoned safepoint waits**: when a GC pause
  is executed by another thread, the waiting thread has no nested zone, so
  the pause lands in whatever zone that thread had open. The counts backend
  largely escapes this because the main thread (sole allocator) wins the GC
  race and nests its own GC event.
- Capture recipe: start julia with `JULIA_WAIT_FOR_TRACY=1` in background,
  then `tracy-capture -f -o out.tracy` (attaching late truncates the trace —
  a partial capture looks like a mysteriously cheap load).

## The wandering-GC lottery (why single captures lie)

~330–350 ms of GC runs during a full CairoMakie load, in a handful of
pauses. Each pause is billed inside whichever zone allocated the triggering
byte — a different zone on every run. Measured signature across 5 plain
runs: ADD_METHOD, ACTIVATE_TmapIns, LOAD_ScanNewMethods, LOAD_Pkgimg all
showed **total-sd ≈ 235 Mcyc (±78 ms) while their self-sd stayed ≤ 2%** —
the same pause rotating between them. Historical example: the
"ScanNewMethods 48 ms → 114 ms regression" that motivated a full
investigation was GC placement, not work growth (its true self is a stable
52 ms). If a zone's total moves between runs but its self doesn't, it's the
lottery, not a regression.

## The migration trap (why even true self-time ≠ removable wall)

Deleting a phase with X ms of *stable, real* self-time can move walls by ~0:
its cycles may substitute for stalls other phases would otherwise pay
(first-touch page faults, cache/TLB warming, lazy materialization — whoever
touches first gets billed). Measured example (scan on/off via
`JULIA_LOAD_SCAN=0`, the in-tree A/B gate, interleaved n=3/arm): the scan's
52 ms of stable self-time gave Δwall = 0.000 s (2.444 vs 2.444 mean),
Δinstructions = +156 M (≈ the scan's real work — it does execute), Δcycles
statistically zero — ~200 M extra instructions absorbed into existing
stalls. Hardware-counter check for this trap: measure Δinstructions vs
Δcycles with `scratch-tools/miniperf` (groups: `a` =
cycles/instructions/faults/task-clock, `b` = cache/dTLB/frontend-stall) —
if instructions rise by the phase's work but cycles barely move, the time
migrated; note counter multiplexing adds ~±1% jitter, so size expected
deltas against that. Perf context for this workload: IPC ≈ 1.4 (not memory-bound in
the classic sense), ~236k page faults, 27% frontend stalls, 10M dTLB misses;
`perf` binary is absent in the sandbox but `perf_event_paranoid=0`, so
perf_event_open works — miniperf wraps it.

## The A/B doctrine (the only decider)

Wall-time claims require: same binary, same depot, only an env gate
differing; n≥8–10 alternating runs per arm; compare means against the
measured noise floor (±8–15 ms here; `taskset` to fixed cores, e.g. 8–15).
Wire temporary gates in C (`getenv` + static memo) — Compiler-side gates
force a 15-minute sysimage rebuild and a full depot regen. Never trust an
A/B where the depot regenerated between arms: cache-layout luck alone is
worth ±50 ms.

## Calibration anchors (this box, 255-image CairoMakie workload, walls ≈2.5 s)

Stable self-times (plain counts runs): LOAD_Relocs ≈ 450 ms,
VERIFY_Prepass ≈ 300 ms, GC_Mark ≈ 325 ms, SUBTYPE ≈ 280 ms,
LOAD_Uniquing ≈ 253 ms, JIT_Compile ≈ 100 ms, VERIFY_Store ≈ 100 ms,
STALECHECK ≈ 84 ms, LOAD_ScanNewMethods ≈ 52 ms, ADD_METHOD ≈ 9 ms.
History says byte/record-volume cuts move walls; pure CPU-work removal
usually doesn't survive the A/B. Sharpest formulation (four A/Bs deep): a
phase's wall cost is its **cache/memory footprint**, not its instruction
count. Swapping expensive compute for cheap compute over the same data
measures ~0 (fdisj fast path: −73 ms of verdict compute, −6 ± 16 ms wall);
eliminating the phase's data-touching wholesale materializes (SC oracle:
−172 ms wall). Optimize by not touching data, not by touching it faster.

## Tooling paths

- `/workspace/scratch-tools/miniperf.c` (+ built binary) — perf_event_open
  wrapper; usage: `miniperf a|b <cmd...>`, counters print to stderr as `MP`.
- `/workspace/scratch-tools/timings-parse.py` — aggregates JULIA TIMINGS
  tables across runs.
- Keep tools and measurement logs in `/workspace/scratch-tools/`
  (git-excluded): `/tmp` scratchpads AND `/root/.claude/jobs/*/tmp` are
  pruned mid-session by the sandbox.
