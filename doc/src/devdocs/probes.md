# Instrumenting Julia with DTrace, and bpftrace

DTrace and bpftrace are tools that enable lightweight instrumentation of processes.
You can turn the instrumentation on and off while the process is running,
and with instrumentation off the overhead is minimal.

!!! compat "Julia 1.8"
    Support for probes was added in Julia 1.8

!!! note
    This documentation has been written from a Linux perspective, most of this
    should hold on Mac OS/Darwin and FreeBSD.

## Enabling support

On Linux install the `systemtap` package that has a version of `dtrace`.

```
WITH_DTRACE=1
```

### Verifying

```
> readelf -n usr/lib/libjulia-internal.so.1

Displaying notes found in: .note.gnu.build-id
  Owner                Data size 	Description
  GNU                  0x00000014	NT_GNU_BUILD_ID (unique build ID bitstring)
    Build ID: 57161002f35548772a87418d2385c284ceb3ead8

Displaying notes found in: .note.stapsdt
  Owner                Data size 	Description
  stapsdt              0x00000029	NT_STAPSDT (SystemTap probe descriptors)
    Provider: julia
    Name: gc__begin
    Location: 0x000000000013213e, Base: 0x00000000002bb4da, Semaphore: 0x0000000000346cac
    Arguments:
  stapsdt              0x00000032	NT_STAPSDT (SystemTap probe descriptors)
    Provider: julia
    Name: gc__stop_the_world
    Location: 0x0000000000132144, Base: 0x00000000002bb4da, Semaphore: 0x0000000000346cae
    Arguments:
  stapsdt              0x00000027	NT_STAPSDT (SystemTap probe descriptors)
    Provider: julia
    Name: gc__end
    Location: 0x000000000013214a, Base: 0x00000000002bb4da, Semaphore: 0x0000000000346cb0
    Arguments:
  stapsdt              0x0000002d	NT_STAPSDT (SystemTap probe descriptors)
    Provider: julia
    Name: gc__finalizer
    Location: 0x0000000000132150, Base: 0x00000000002bb4da, Semaphore: 0x0000000000346cb2
    Arguments:
```

## Adding probes in libjulia

Probes are declared in dtraces format in the file `src/uprobes.d`. The generated
header file is included in `src/julia_internal.h` and if you add probes you should
provide a noop implementation there.

The header will contain a semaphore `*_ENABLED` and the actual call to the probe.
If the probe arguments are expensive to compute you should first check if the
probe is enabled and then compute the arguments and call the probe.

```c
  if (JL_PROBE_{PROBE}_ENABLED())
    auto expensive_arg = ...;
    JL_PROBE_{PROBE}(expensive_arg);
```

If your probe has no arguments it is preferred to not include the semaphore check.
With USDT probes enabled the cost of a semaphore is a memory load, irrespective of
the fact that the probe is enabled or not.

```c
#define JL_PROBE_GC_BEGIN_ENABLED() __builtin_expect (julia_gc__begin_semaphore, 0)
__extension__ extern unsigned short julia_gc__begin_semaphore __attribute__ ((unused)) __attribute__ ((section (".probes")));
```

Whereas the probe itself is a noop sled that will be patched to a trampoline to
the probe handler.

## Available probes

### GC probes

1. `julia:gc__begin`: GC begins running on one thread and triggers stop-the-world.
2. `julia:gc__stop_the_world`: All threads have reached a safepoint and GC runs.
3. `julia:gc__mark__begin`: Beginning the mark phase
4. `julia:gc__mark_end(scanned_bytes, perm_scanned)`: Mark phase ended
5. `julia:gc__sweep_begin(full)`: Starting sweep
6. `julia:gc__sweep_end()`: Sweep phase finished
7. `julia:gc__end`: GC is finished, other threads continue work
8. `julia:gc__finalizer`: Initial GC thread has finished running finalizers

#### GC stop-the-world latency

An example `bpftrace` script is given in `contrib/gc_stop_the_world_latency.bt`
and it creates a histogram of the latency for all threads to reach a safepoint.

Running this Julia code, with `julia -t 2`

```
using Base.Threads

fib(x) = x <= 1 ? 1 : fib(x-1) + fib(x-2)

beaver = @spawn begin
    while true
        fib(30)
        # This safepoint is necessary until #41616, since otherwise this
        # loop will never yield to GC.
        GC.safepoint()
    end
end

allocator = @spawn begin
    while true
        zeros(1024)
    end
end

wait(allocator)
```

and in a second terminal

```
> sudo contrib/bpftrace/gc_stop_the_world_latency.bt
Attaching 4 probes...
Tracing Julia GC Stop-The-World Latency... Hit Ctrl-C to end.
^C


@usecs[1743412]:
[4, 8)               971 |@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@|
[8, 16)              837 |@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@        |
[16, 32)             129 |@@@@@@                                              |
[32, 64)              10 |                                                    |
[64, 128)              1 |                                                    |
```

We can see the latency distribution of the stop-the-world phase in the executed Julia process.

## Notes on using `bpftrace`

An example probe in the bpftrace format looks like:

```
usdt:usr/lib/libjulia-internal.so:julia:gc__begin
{
	@start[pid] = nsecs;
}
```

The probe declaration takes the kind `usdt`, then either the
path to the library or the PID, the provider name `julia`
and the probe name `gc__begin`. Note that I am using a
relative path to the `libjulia-internal.so`, but this might
need to be an absolute path on a production system.

## Useful references:

- [Julia Evans blog on Linux tracing systems](https://jvns.ca/blog/2017/07/05/linux-tracing-systems)
- [LWN article on USDT and BPF](https://lwn.net/Articles/753601/)
- [GDB support for probes](https://sourceware.org/gdb/onlinedocs/gdb/Static-Probe-Points.html)
- [Brendan Gregg -- Linux Performance](https://www.brendangregg.com/linuxperf.html)
