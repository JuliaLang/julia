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

On Linux install the `systemtap` package that has a version of `dtrace` and create a `Make.user` file containing

```
WITH_DTRACE=1
```

to enable USDT probes.

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
6. `julia:gc__sweep_end`: Sweep phase finished
7. `julia:gc__end`: GC is finished, other threads continue work
8. `julia:gc__finalizer`: Initial GC thread has finished running finalizers

### Task runtime probes

1. `julia:rt__run__task(task)`: Switching to task `task` on current thread.
2. `julia:rt__pause__task(task)`: Switching from task `task` on current thread.
3. `julia:rt__new__task(parent, child)`: Task `parent` created task `child` on current thread.
4. `julia:rt__start__task(task)`: Task `task` started for the first time with a new stack.
5. `julia:rt__finish__task(task)`: Task `task` finished and will no longer execute.
6. `julia:rt__start__process__events(task)`: Task `task` started processing libuv events.
7. `julia:rt__finish__process__events(task)`: Task `task` finished processing libuv events.

### Task queue probes

1. `julia:rt__taskq__insert(ptls, task)`: Thread `ptls` attempted to insert `task` into a PARTR multiq.
2. `julia:rt__taskq__get(ptls, task)`: Thread `ptls` popped `task` from a PARTR multiq.

### Thread sleep/wake probes

1. `julia:rt__sleep__check__wake(ptls, old_state)`: Thread (PTLS `ptls`) waking up, previously in state `old_state`.
2. `julia:rt__sleep__check__wakeup(ptls)`: Thread (PTLS `ptls`) woke itself up.
3. `julia:rt__sleep__check__sleep(ptls)`: Thread (PTLS `ptls`) is attempting to sleep.
4. `julia:rt__sleep__check__taskq__wake(ptls)`: Thread (PTLS `ptls`) fails to sleep due to tasks in PARTR multiq.
5. `julia:rt__sleep__check__task__wake(ptls)`: Thread (PTLS `ptls`) fails to sleep due to tasks in Base workqueue.
6. `julia:rt__sleep__check__uv__wake(ptls)`: Thread (PTLS `ptls`) fails to sleep due to libuv wakeup.

## Probe usage examples

### GC stop-the-world latency

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

### Task spawn monitor

It's sometimes useful to know when a task is spawning other tasks. This is very
easy to see with `rt__new__task`. The first argument to the probe, `parent`, is
the existing task which is creating a new task. This means that if you know the
address of the task you want to monitor, you can easily just look at the tasks
that that specific task spawned. Let's see how to do this; first let's start a
Julia session and get the PID and REPL's task address:

```
> julia
               _
   _       _ _(_)_     |  Documentation: https://docs.julialang.org
  (_)     | (_) (_)    |
   _ _   _| |_  __ _   |  Type "?" for help, "]?" for Pkg help.
  | | | | | | |/ _` |  |
  | | |_| | | | (_| |  |  Version 1.6.2 (2021-07-14)
 _/ |\__'_|_|_|\__'_|  |  Official https://julialang.org/ release
|__/                   |

1> getpid()
997825

2> current_task()
Task (runnable) @0x00007f524d088010
```

Now we can start `bpftrace` and have it monitor `rt__new__task` for *only* this parent:

`sudo bpftrace -p 997825 -e 'usdt:usr/lib/libjulia-internal.so:julia:rt__new__task /arg0==0x00007f524d088010/{ printf("Task: %x\n", arg0); }'`

(Note that in the above, `arg0` is the first argument, `parent`).

And if we spawn a single task:

`@async 1+1`

we see this task being created:

`Task: 4d088010`

However, if we spawn a bunch of tasks from that newly-spawned task:

```julia
@async for i in 1:10
   @async 1+1
end
```

we still only see one task from `bpftrace`:

`Task: 4d088010`

and it's still the same task we were monitoring! Of course, we can remove this
filter to see *all* newly-created tasks just as easily:

`sudo bpftrace -p 997825 -e 'usdt:usr/lib/libjulia-internal.so:julia:rt__new__task { printf("Task: %x\n", arg0); }'`

```
Task: 4d088010
Task: 4dc4e290
Task: 4dc4e290
Task: 4dc4e290
Task: 4dc4e290
Task: 4dc4e290
Task: 4dc4e290
Task: 4dc4e290
Task: 4dc4e290
Task: 4dc4e290
Task: 4dc4e290
```

We can see our root task, and the newly-spawned task as the parent of the ten
even newer tasks.

### Thundering herd detection

Task runtimes can often suffer from the "thundering herd" problem: when some
work is added to a quiet task runtime, all threads may be woken up from their
slumber, even if there isn't enough work for each thread to process. This can
cause extra latency and CPU cycles while all threads awaken (and simultaneously
go back to sleep, not finding any work to execute).

We can see this problem illustrated with `bpftrace` quite easily. First, in one terminal we start Julia with multiple threads (6 in this example), and get the PID of that process:

```
> julia -t 6
               _
   _       _ _(_)_     |  Documentation: https://docs.julialang.org
  (_)     | (_) (_)    |
   _ _   _| |_  __ _   |  Type "?" for help, "]?" for Pkg help.
  | | | | | | |/ _` |  |
  | | |_| | | | (_| |  |  Version 1.6.2 (2021-07-14)
 _/ |\__'_|_|_|\__'_|  |  Official https://julialang.org/ release
|__/                   |

1> getpid()
997825
```

And in another terminal we start `bpftrace` monitoring our process,
specifically probing the `rt__sleep__check__wake` hook:

`sudo bpftrace -p 997825 -e 'usdt:usr/lib/libjulia-internal.so:julia:rt__sleep__check__wake { printf("Thread wake up! %x\n", arg0); }'`

Now, we create and execute a single task in Julia:

`Threads.@spawn 1+1`

And in `bpftrace` we see printed out something like:

```
Thread wake up! 3f926100
Thread wake up! 3ebd5140
Thread wake up! 3f876130
Thread wake up! 3e2711a0
Thread wake up! 3e312190
```

Even though we only spawned a single task (which only one thread could process
at a time), we woke up all of our other threads! In the future, a smarter task
runtime might only wake up a single thread (or none at all; the spawning thread
could execute this task!), and we should see this behavior go away.

### Task Monitor with BPFnative.jl

BPFnative.jl is able to attach to USDT probe points just like `bpftrace`. There
is a demo available for monitoring the task runtime, GC, and thread sleep/wake
transitions [here](https://github.com/jpsamaroo/BPFnative.jl/blob/master/examples/task-runtime.jl).

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
