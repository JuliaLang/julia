```@meta
EditURL = "https://github.com/JuliaLang/julia/blob/master/stdlib/Profile/docs/src/index.md"
```

# [Profiling](@id lib-profiling)

## CPU Profiling

There are two main approaches to CPU profiling julia code:

## Via `@profile`

Where profiling is enabled for a given call via the `@profile` macro.

```julia-repl
julia> using Profile

julia> @profile foo()

julia> Profile.print()
Overhead ╎ [+additional indent] Count File:Line; Function
=========================================================
    ╎147  @Base/client.jl:506; _start()
        ╎ 147  @Base/client.jl:318; exec_options(opts::Base.JLOptions)
...
```

## Triggered During Execution

Tasks that are already running can also be profiled for a fixed time period at any user-triggered time.

To trigger the profiling:
- MacOS & FreeBSD (BSD-based platforms): Use `ctrl-t` or pass a `SIGINFO` signal to the julia process i.e. `% kill -INFO $julia_pid`
- Linux: Pass a `SIGUSR1` signal to the julia process i.e. `% kill -USR1 $julia_pid`
- Windows: Not currently supported.

First, a single stack trace at the instant that the signal was thrown is shown, then a 1 second profile is collected,
followed by the profile report at the next yield point, which may be at task completion for code without yield points
e.g. tight loops.

Optionally set environment variable [`JULIA_PROFILE_PEEK_HEAP_SNAPSHOT`](@ref JULIA_PROFILE_PEEK_HEAP_SNAPSHOT) to `1` to also automatically collect a
[heap snapshot](@ref Heap-Snapshots).

```julia-repl
julia> foo()
##== the user sends a trigger while foo is running ==##
load: 2.53  cmd: julia 88903 running 6.16u 0.97s

======================================================================================
Information request received. A stacktrace will print followed by a 1.0 second profile
======================================================================================

signal (29): Information request: 29
__psynch_cvwait at /usr/lib/system/libsystem_kernel.dylib (unknown line)
_pthread_cond_wait at /usr/lib/system/libsystem_pthread.dylib (unknown line)
...

======================================================================
Profile collected. A report will print if the Profile module is loaded
======================================================================

Overhead ╎ [+additional indent] Count File:Line; Function
=========================================================
Thread 1 Task 0x000000011687c010 Total snapshots: 572. Utilization: 100%
   ╎147 @Base/client.jl:506; _start()
       ╎ 147 @Base/client.jl:318; exec_options(opts::Base.JLOptions)
...

Thread 2 Task 0x0000000116960010 Total snapshots: 572. Utilization: 0%
   ╎572 @Base/task.jl:587; task_done_hook(t::Task)
      ╎ 572 @Base/task.jl:879; wait()
...
```

### Customization

The duration of the profiling can be adjusted via [`Profile.set_peek_duration`](@ref)

The profile report is broken down by thread and task. Pass a no-arg function to `Profile.peek_report[]` to override this.
i.e. `Profile.peek_report[] = () -> Profile.print()` to remove any grouping. This could also be overridden by an external
profile data consumer.

## Reference

```@docs
Profile.@profile
```

The methods in `Profile` are not exported and need to be called e.g. as `Profile.print()`.

```@docs
Profile.clear
Profile.print
Profile.init
Profile.fetch
Profile.retrieve
Profile.callers
Profile.clear_malloc_data
Profile.get_peek_duration
Profile.set_peek_duration
```

## Memory profiling

```@docs
Profile.Allocs.@profile
```

The methods in `Profile.Allocs` are not exported and need to be called e.g. as `Profile.Allocs.fetch()`.

```@docs
Profile.Allocs.clear
Profile.Allocs.print
Profile.Allocs.fetch
Profile.Allocs.start
Profile.Allocs.stop
```

## Heap Snapshots

```@docs
Profile.take_heap_snapshot
```

The methods in `Profile` are not exported and need to be called e.g. as `Profile.take_heap_snapshot()`.

```julia-repl
julia> using Profile

julia> Profile.take_heap_snapshot("snapshot.heapsnapshot")
```

Traces and records julia objects on the heap. This only records objects known to the Julia
garbage collector. Memory allocated by external libraries not managed by the garbage
collector will not show up in the snapshot.

To avoid OOMing while recording the snapshot, we added a streaming option to stream out the heap snapshot
into four files,

```julia-repl
julia> using Profile

julia> Profile.take_heap_snapshot("snapshot"; streaming=true)
```

where "snapshot" is the filepath as the prefix for the generated files.

Once the snapshot files are generated, they could be assembled offline with the following command:

```julia-repl
julia> using Profile

julia> Profile.HeapSnapshot.assemble_snapshot("snapshot", "snapshot.heapsnapshot")
```

The resulting heap snapshot file can be uploaded to chrome devtools to be viewed.
For more information, see the [chrome devtools docs](https://developer.chrome.com/docs/devtools/memory-problems/heap-snapshots/#view_snapshots).
An alternative for analyzing Chromium heap snapshots is with the VS Code extension
`ms-vscode.vscode-js-profile-flame`.

The Firefox heap snapshots are of a different format, and Firefox currently may
*not* be used for viewing the heap snapshots generated by Julia.
