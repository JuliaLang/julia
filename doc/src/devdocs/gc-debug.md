# GC Debug Build Tools

Julia's garbage collector includes a suite of debugging tools that are enabled
by building Julia with `WITH_GC_DEBUG_ENV=1`. These tools help developers
diagnose memory-safety bugs such as missing write barriers, use-after-free
errors, and incorrect GC roots.

## Building with GC Debug Support

Add the following line to your `Make.user` file before building Julia:

```make
WITH_GC_DEBUG_ENV=1
```

This sets the C preprocessor define `GC_DEBUG_ENV`, which enables several
debugging features described below. It also automatically enables `GC_VERIFY`
(see [GC Verification](@ref)).

To rebuild Julia after changing `Make.user`:

```sh
make -j
```

!!! note
    The AddressSanitizer build configuration (`contrib/asan/Make.user.asan`)
    enables `WITH_GC_DEBUG_ENV=1` by default, since ASAN replaces pool allocation
    with `malloc`/`free` and the GC debug env controls align with that model.

## Environment Variables

When Julia is built with `WITH_GC_DEBUG_ENV=1`, the following environment
variables are recognized at startup.

### `JULIA_GC_WAIT_FOR_DEBUGGER`

If set to any value other than `0`, the GC will pause and wait for a debugger
to attach whenever a critical GC error is detected (such as a write barrier
violation found by `GC_VERIFY`), instead of immediately aborting.

```sh
JULIA_GC_WAIT_FOR_DEBUGGER=1 ./julia myscript.jl
```

Once the process is sleeping, attach a debugger with:

```sh
gdb -p <PID>
```

### `JULIA_GC_ALLOC_POOL`, `JULIA_GC_ALLOC_OTHER`, `JULIA_GC_ALLOC_PRINT`

These three variables control when the GC is triggered or when statistics are
printed, based on allocation counts. They share a common format:

```
[r]<min>:<interv>:<max>
```

| Field    | Meaning |
|----------|---------|
| `min`    | Number of allocations before the first trigger |
| `interv` | Interval between subsequent triggers |
| `max`    | Maximum allocation count at which to trigger (default: no limit) |
| `r` prefix | Randomise trigger timing while preserving the same average frequency |

All three fields are optional. Examples:

| Setting              | Effect |
|----------------------|--------|
| `1`                  | Trigger on every allocation |
| `100:10`             | First trigger at allocation 100, then every 10 after that |
| `50:1:200`           | Trigger every allocation from 50 to 200 |
| `r1000:1000`         | Trigger approximately every 1000 allocations with random jitter |

**`JULIA_GC_ALLOC_POOL`** — Controls GC collection triggered by small
(pool-allocated) objects. The counter increments on each pool allocation.

**`JULIA_GC_ALLOC_OTHER`** — Controls GC collection triggered by large
(non-pool, `malloc`-backed) objects. The counter increments on each large
allocation.

**`JULIA_GC_ALLOC_PRINT`** — Controls when allocation statistics are printed
to `stderr`. On each trigger, a line like the following is emitted:

```
Allocations: 12345 (Pool: 10000; Other: 2345); GC: 7
```

#### Example: trigger GC on every allocation

This is the most aggressive stress-test mode. It runs a full GC cycle on every
single allocation, which makes bugs far more likely to be exposed:

```sh
JULIA_GC_ALLOC_POOL=1 JULIA_GC_ALLOC_OTHER=1 ./julia myscript.jl
```

#### Example: randomised stress testing

Using the `r` prefix distributes GC triggers randomly around the given interval,
which can expose bugs that only appear at specific allocation counts when using
a fixed interval:

```sh
JULIA_GC_ALLOC_POOL=r1:1 JULIA_GC_ALLOC_OTHER=r1:1 ./julia myscript.jl
```

## GC Verification (`GC_VERIFY`) {#GCVerify}

`WITH_GC_DEBUG_ENV=1` automatically enables `GC_VERIFY`. After every minor
(quick) GC, a full second GC pass is run that:

1. Clears all mark bits.
2. Re-marks from all roots.
3. Checks that every object that was about to be freed is now also unreachable
   in the fresh mark phase.

If an object is found to be alive in the second pass but was scheduled for
collection in the first pass, there is a missing write barrier. The verifier
will then backtrack the object graph to identify which parent object was written
without a corresponding `jl_gc_wb()` call, and print a message such as:

```
Missing write barrier found !
<parent> was written a reference to <child> that was not recorded
```

After printing the diagnostic, the process aborts (or waits for a debugger if
`JULIA_GC_WAIT_FOR_DEBUGGER=1`).

!!! note
    `GC_VERIFY` is limited to single-threaded GC. If `julia` is started with
    GC threads (`--gcthreads`), verification is silently skipped.

### Tips for debugging write barrier violations

1. **Reproduce with GC_VERIFY enabled.** Build with `WITH_GC_DEBUG_ENV=1` and
   run the failing workload. The verifier will catch the violation and print
   which object and slot were written without a write barrier.

2. **Disable ASLR for reproducible addresses.** If you need to set hardware
   watchpoints on specific memory locations:
   ```sh
   echo 0 | sudo tee /proc/sys/kernel/randomize_va_space
   ```
   Addresses will then be stable across runs (with the same binary and
   environment).

3. **Use hardware watchpoints in GDB.** Once you know the address of the slot
   that was written incorrectly, attach GDB and set a watchpoint:
   ```
   watch *slot_addr if *slot_addr == expected_val
   ```
   This stops execution at the exact moment the write occurs.

4. **Use `rr` for deterministic replay.** [`rr`](https://rr-project.org/) records
   a process execution and replays it perfectly — the same instructions, the same
   addresses — allowing you to run the program backwards in GDB. This is
   especially useful for GC bugs where the failure manifests far from the root
   cause:

   ```sh
   rr record ./julia myscript.jl
   rr replay
   ```

   Inside the replay session, use reverse-execution commands such as `reverse-continue`
   (`rc`) and `reverse-next` (`rn`) to step backwards from the crash to the moment
   the bad write occurred. Because `rr` replays with ASLR disabled and a fixed
   random seed, addresses are stable across record/replay cycles, which makes
   watchpoints reliable without having to manually disable ASLR.

## Related Tools

- [Using Valgrind with Julia](@ref) — detect memory errors and leaks using `MEMDEBUG` plus Valgrind's `memcheck` tool.
- [Sanitizer support](@ref) — build Julia with AddressSanitizer or ThreadSanitizer; the ASAN configuration automatically sets `WITH_GC_DEBUG_ENV=1`.
- [Static analyzer annotations for GC correctness in C code](@ref) — static analysis tool that checks for missing `JL_GC_PUSH` / write barrier annotations in C source files.
