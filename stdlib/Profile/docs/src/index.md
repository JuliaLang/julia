# [Profiling](@id lib-profiling)

The `Profile` module in Julia provides tools to help developers improve the performance of their code.
It measures the runtime of code and generates output to help you understand how much time is spent on individual lines.
The primary use of profiling is to identify "bottlenecks" in your code that can be optimized.

The Julia `Profile` standard library includes:

- A sampling profiler that measures execution times.
- An [allocation profiler](@ref allocation-profiler) that tracks memory allocation.

## Basic usage of runtime profiling

Let's work with a simple test case:

```julia-repl
julia> function myfunc()
           A = rand(200, 200, 400)
           maximum(A)
       end
```

It's a good idea to first run the code you intend to profile at least once (unless you want to
profile Julia's JIT-compiler):

```julia-repl
julia> myfunc() # run once to force compilation
```

Now we're ready to profile this function:

```julia-repl
julia> using Profile

julia> @profile myfunc()
```

We will use the text-based display that comes with the standard library.
for other visualization tools are listed in the [Visualization Tools](@ref profile-vis) section.

```julia-repl
julia> Profile.print()
80 ./event.jl:73; (::Base.REPL.##1#2{Base.REPL.REPLBackend})()
 80 ./REPL.jl:97; macro expansion
  80 ./REPL.jl:66; eval_user_input(::Any, ::Base.REPL.REPLBackend)
   80 ./boot.jl:235; eval(::Module, ::Any)
    80 ./<missing>:?; anonymous
     80 ./profile.jl:23; macro expansion
      52 ./REPL[1]:2; myfunc()
       38 ./random.jl:431; rand!(::MersenneTwister, ::Array{Float64,3}, ::Int64, ::Type{B...
        38 ./dSFMT.jl:84; dsfmt_fill_array_close_open!(::Base.dSFMT.DSFMT_state, ::Ptr{F...
       14 ./random.jl:278; rand
        14 ./random.jl:277; rand
         14 ./random.jl:366; rand
          14 ./random.jl:369; rand
      28 ./REPL[1]:3; myfunc()
       28 ./reduce.jl:270; _mapreduce(::Base.#identity, ::Base.#scalarmax, ::IndexLinear,...
        3  ./reduce.jl:426; mapreduce_impl(::Base.#identity, ::Base.#scalarmax, ::Array{F...
        25 ./reduce.jl:428; mapreduce_impl(::Base.#identity, ::Base.#scalarmax, ::Array{F...
```

Each line of this display represents a particular spot (line number) in the code. Indentation
is used to indicate the nested sequence of function calls, with more-indented lines being deeper
in the sequence of calls. In each line, the first "field" is the number of backtraces
(samples) taken *at this line or in any functions executed by this line*.
The second field is the file name and line number and the third field is the function name.
Note that the specific line numbers may change as Julia's
code changes; if you want to follow along, it's best to run this example yourself.

In this example, we can see that the top level function called is in the file `event.jl`. This is the
function that runs the REPL when you launch Julia. If you examine line 97 of `REPL.jl`,
you'll see this is where the function `eval_user_input()` is called. This is the function that evaluates
what you type at the REPL, and since we're working interactively these functions were invoked
when we entered `@profile myfunc()`. The next line reflects actions taken in the [`@profile`](@ref)
macro.

The first line shows that 80 backtraces were taken at line 73 of `event.jl`, but it's not that
this line was "expensive" on its own: the third line reveals that all 80 of these backtraces
were actually triggered inside its call to `eval_user_input`, and so on. To find out which operations
are actually taking the time, we need to look deeper in the call chain.

The first "important" line in this output is this one:

```
52 ./REPL[1]:2; myfunc()
```

`REPL` refers to the fact that we defined `myfunc` in the REPL, rather than putting it in a file;
if we had used a file, this would show the file name. The `[1]` shows that the function `myfunc`
was the first expression evaluated in this REPL session. Line 2 of `myfunc()` contains the call to
`rand`, and there were 52 (out of 80) backtraces that occurred at this line. Below that, you can
see a call to `dsfmt_fill_array_close_open!` inside `dSFMT.jl`.

A little further down, you see:

```
28 ./REPL[1]:3; myfunc()
```

Line 3 of `myfunc` contains the call to `maximum`, and there were 28 (out of 80) backtraces taken
here. Below that, you can see the specific places in `base/reduce.jl` that carry out the time-consuming
operations in the `maximum` function for this type of input data.

Overall, we can tentatively conclude that generating the random numbers is approximately twice as expensive
as finding the maximum element. We could increase our confidence in this result by
collecting more samples:

```julia-repl
julia> @profile (for i = 1:100; myfunc(); end)

julia> Profile.print()
[....]
 3821 ./REPL[1]:2; myfunc()
  3511 ./random.jl:431; rand!(::MersenneTwister, ::Array{Float64,3}, ::Int64, ::Type...
   3511 ./dSFMT.jl:84; dsfmt_fill_array_close_open!(::Base.dSFMT.DSFMT_state, ::Ptr...
  310  ./random.jl:278; rand
   [....]
 2893 ./REPL[1]:3; myfunc()
  2893 ./reduce.jl:270; _mapreduce(::Base.#identity, ::Base.#scalarmax, ::IndexLinea...
   [....]
```

In general, if you have `N` samples collected at a line, you can expect an uncertainty on the
order of `sqrt(N)` (barring other sources of noise, like how busy the computer is with other tasks).
The major exception to this rule is garbage collection, which runs infrequently but tends to be
quite expensive. (Since Julia's garbage collector is written in C, such events can be detected
using the `C=true` output mode described below, or by using [ProfileView.jl](https://github.com/timholy/ProfileView.jl).)

This illustrates the default "tree" dump; an alternative is the "flat" dump, which accumulates
counts independent of their nesting:

```julia-repl
julia> Profile.print(format=:flat)
 Count File          Line Function
  6714 ./<missing>     -1 anonymous
  6714 ./REPL.jl       66 eval_user_input(::Any, ::Base.REPL.REPLBackend)
  6714 ./REPL.jl       97 macro expansion
  3821 ./REPL[1]        2 myfunc()
  2893 ./REPL[1]        3 myfunc()
  6714 ./REPL[7]        1 macro expansion
  6714 ./boot.jl      235 eval(::Module, ::Any)
  3511 ./dSFMT.jl      84 dsfmt_fill_array_close_open!(::Base.dSFMT.DSFMT_s...
  6714 ./event.jl      73 (::Base.REPL.##1#2{Base.REPL.REPLBackend})()
  6714 ./profile.jl    23 macro expansion
  3511 ./random.jl    431 rand!(::MersenneTwister, ::Array{Float64,3}, ::In...
   310 ./random.jl    277 rand
   310 ./random.jl    278 rand
   310 ./random.jl    366 rand
   310 ./random.jl    369 rand
  2893 ./reduce.jl    270 _mapreduce(::Base.#identity, ::Base.#scalarmax, :...
     5 ./reduce.jl    420 mapreduce_impl(::Base.#identity, ::Base.#scalarma...
   253 ./reduce.jl    426 mapreduce_impl(::Base.#identity, ::Base.#scalarma...
  2592 ./reduce.jl    428 mapreduce_impl(::Base.#identity, ::Base.#scalarma...
    43 ./reduce.jl    429 mapreduce_impl(::Base.#identity, ::Base.#scalarma...
```

If your code has recursion, one potentially-confusing point is that a line in a "child" function
can accumulate more counts than there are total backtraces. Consider the following function definitions:

```julia
dumbsum(n::Integer) = n == 1 ? 1 : 1 + dumbsum(n-1)
dumbsum3() = dumbsum(3)
```

If you were to profile `dumbsum3`, and a backtrace was taken while it was executing `dumbsum(1)`,
the backtrace would look like this:

```julia
dumbsum3
    dumbsum(3)
        dumbsum(2)
            dumbsum(1)
```

Consequently, this child function gets 3 counts, even though the parent only gets one. The "tree"
representation makes this much clearer, and for this reason (among others) is probably the most
useful way to view the results.


## [Visualization Tools](@id profile-vis)

To see the profiling results, there are several graphical browsers.

- [VS Code](https://www.julia-vscode.org/) is a full IDE with built-in support for profile visualization
- [ProfileView.jl](https://github.com/timholy/ProfileView.jl) is a stand-alone visualizer based on GTK
- [ProfileVega.jl](https://github.com/davidanthoff/ProfileVega.jl) uses VegaLight and integrates well with Jupyter notebooks
- [StatProfilerHTML.jl](https://github.com/tkluck/StatProfilerHTML.jl) produces HTML and presents some additional summaries, and also integrates well with Jupyter notebooks
- [ProfileSVG.jl](https://github.com/timholy/ProfileSVG.jl) renders SVG
- [PProf.jl](https://github.com/JuliaPerf/PProf.jl) serves a local website for inspecting graphs, flamegraphs and more
- [ProfileCanvas.jl](https://github.com/pfitzseb/ProfileCanvas.jl) is a HTML canvas based profile viewer UI, used by the [Julia VS Code extension](https://www.julia-vscode.org/), but can also generate interactive HTML files.


## A note on sampling profilers

`Profile` implements a "sampling" or [statistical profiler](https://en.wikipedia.org/wiki/Profiling_(computer_programming)).
It operates by periodically capturing a backtrace during the execution of any task.
Each backtrace records the currently-running function and line number, as well as the complete chain of function calls that led to this line,
providing a "snapshot" of the current execution state.

The frequency of a particular line of code appearing in the set of all backtraces reflects the proportion of runtime spent on that line.
Essentially, the "cost" of a given line (or the sequence of function calls including this line) is proportional to how often it appears in the set of backtraces.

A sampling profiler does not provide complete line-by-line coverage because backtraces occur at intervals (1 ms on Unix systems and 10 ms on Windows by default).
Also, the data collected by a sampling profiler is subject to statistical noise due to the sparse sampling of all execution points.
Despite these limitations, sampling profilers have substantial strengths:

- You do not have to modify your code to take timing measurements.
- It can profile into Julia's core code and even (optionally) into C and Fortran libraries.
- By running "infrequently", the profiler incurs very little performance overhead, allowing your code to run at near-native speed.

## Accumulation and clearing

Results from [`@profile`](@ref) accumulate in a buffer; if you run multiple pieces of code under
[`@profile`](@ref), then [`Profile.print()`](@ref) will show you the combined results. This can
be very useful, but sometimes you want to start fresh; you can do so with [`Profile.clear()`](@ref).

## Options for controlling the display of profile results

[`Profile.print`](@ref) has more options than we've described so far. Let's see the full declaration:

```julia
function print(io::IO = stdout, data = fetch(); kwargs...)
```

Let's first discuss the two positional arguments, and later the keyword arguments:

  * `io` -- Allows you to save the results to a buffer, e.g. a file, but the default is to print to `stdout`
    (the console).
  * `data` -- Contains the data you want to analyze; by default that is obtained from [`Profile.fetch()`](@ref),
    which pulls out the backtraces from a pre-allocated buffer. For example, if you want to profile
    the profiler, you could say:

    ```julia
    data = copy(Profile.fetch())
    Profile.clear()
    @profile Profile.print(stdout, data) # Prints the previous results
    Profile.print()                      # Prints results from Profile.print()
    ```

The keyword arguments can be any combination of:

  * `format` -- Introduced above, determines whether backtraces are printed
     with (default, `:tree`) or without (`:flat`) indentation indicating tree
     structure.
  * `C` -- If `true`, backtraces from C and Fortran code are shown (normally they are excluded). Try running the introductory
    example with `Profile.print(C = true)`. This can be extremely helpful in deciding whether it's
    Julia code or C code that is causing a bottleneck; setting `C = true` also improves the interpretability
    of the nesting, at the cost of longer profile dumps.
  * `combine` -- Some lines of code contain multiple operations; for example, `s += A[i]` contains both an array
    reference (`A[i]`) and a sum operation. These correspond to different lines in the generated
    machine code, and hence there may be two or more different addresses captured during backtraces
    on this line. `combine = true` lumps them together, and is probably what you typically want, but
    you can generate an output separately for each unique instruction pointer with `combine = false`.
  * `maxdepth` -- Limits frames at a depth higher than `maxdepth` in the `:tree` format.
  * `sortedby` -- Controls the order in `:flat` format. `:filefuncline` (default) sorts by the source
    line, whereas `:count` sorts in order of number of collected samples.
  * `noisefloor` -- Limits frames that are below the heuristic noise floor of the sample (only applies to format `:tree`).
    A suggested value to try for this is 2.0 (the default is 0). This parameter hides samples for which `n <= noisefloor * √N`,
    where `n` is the number of samples on this line, and `N` is the number of samples for the callee.
  * `mincount` -- Limits frames with less than `mincount` occurrences.

File/function names are sometimes truncated (with `...`), and indentation is truncated with a
`+n` at the beginning, where `n` is the number of extra spaces that would have been inserted,
had there been room. If you want a complete profile of deeply-nested code, often a good idea is
to save to a file using a wide `displaysize` in an [`IOContext`](@ref):

```julia
open("/tmp/prof.txt", "w") do s
    Profile.print(IOContext(s, :displaysize => (24, 500)))
end
```

## Configuration

[`@profile`](@ref) just accumulates backtraces, and the analysis happens when you call [`Profile.print()`](@ref).
For a long-running computation, it's entirely possible that the pre-allocated buffer for storing
backtraces will be filled. If that happens, the backtraces stop but your computation continues.
As a consequence, you may miss some important profiling data (you will get a warning when that
happens).

You can obtain and configure the relevant parameters this way:

```julia
Profile.init() # returns the current settings
Profile.init(n = 10^7, delay = 0.01)
```

`n` is the total number of instruction pointers you can store, with a default value of `10^6`.
If your typical backtrace is 20 instruction pointers, then you can collect 50000 backtraces, which
suggests a statistical uncertainty of less than 1%. This may be good enough for most applications.

Consequently, you are more likely to need to modify `delay`, expressed in seconds, which sets
the amount of time that Julia gets between snapshots to perform the requested computations. A
very long-running job might not need frequent backtraces. The default setting is `delay = 0.001`.
Of course, you can decrease the delay as well as increase it; however, the overhead of profiling
grows once the delay becomes similar to the amount of time needed to take a backtrace (~30 microseconds
on the author's laptop).

### Customization

The duration of the profiling can be adjusted via [`Profile.set_peek_duration`](@ref)

The profile report is broken down by thread and task. Pass a no-arg function to `Profile.peek_report[]` to override this.
i.e. `Profile.peek_report[] = () -> Profile.print()` to remove any grouping. This could also be overridden by an external
profile data consumer.

## [Allocation Profiler](@id allocation-profiler)

!!! compat "Julia 1.8"
    This functionality requires at least Julia 1.8.

The allocation profiler records the stack trace, type, and size of each
allocation while it is running. It can be invoked with
[`Profile.Allocs.@profile`](@ref).

This information about the allocations is returned as an array of `Alloc`
objects, wrapped in an `AllocResults` object. The best way to visualize these is
currently with the [PProf.jl](https://github.com/JuliaPerf/PProf.jl) and
[ProfileCanvas.jl](https://github.com/pfitzseb/ProfileCanvas.jl) packages, which
can visualize the call stacks which are making the most allocations.

The allocation profiler does have significant overhead, so a `sample_rate`
argument can be passed to speed it up by making it skip some allocations.
Passing `sample_rate=1.0` will make it record everything (which is slow);
`sample_rate=0.1` will record only 10% of the allocations (faster), etc.

!!! note

    The current implementation of the Allocations Profiler _does not
    capture types for all allocations._ Allocations for which the profiler
    could not capture the type are represented as having type
    `Profile.Allocs.UnknownType`.

    You can read more about the missing types and the plan to improve this, here:
    [issue #43688](https://github.com/JuliaLang/julia/issues/43688).



## Triggering a Profile During Execution

Tasks that are already running can also be profiled for a fixed time period at any user-triggered time.

To trigger the profiling:
- MacOS & FreeBSD (BSD-based platforms): Use `ctrl-t` or pass a `SIGINFO` signal to the julia process i.e. `% kill -INFO $julia_pid`
- Linux: Pass a `SIGUSR1` signal to the julia process i.e. `% kill -USR1 $julia_pid`
- Windows: Not currently supported.

First, a single stack trace at the instant that the signal was thrown is shown, then a 1 second profile is collected,
followed by the profile report at the next yield point, which may be at task completion for code without yield points
e.g. tight loops.

Optionally set environment variable `JULIA_PROFILE_PEEK_HEAP_SNAPSHOT` to `1` to also automatically collect a
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

The resulting heap snapshot file can be uploaded to chrome devtools to be viewed.
For more information, see the [chrome devtools docs](https://developer.chrome.com/docs/devtools/memory-problems/heap-snapshots/#view_snapshots).
