# Profiling

!!! note
    For a comprehensive guide on profiling, refer to the [`Profile` standard library documentation](@ref lib-profiling).

Understanding performance is crucial in code optimization, and profiling is a key tool for this purpose.
Julia offers a built-in standard library, [`Profile`](@ref lib-profiling), designed specifically for profiling tasks.
This library facilitates both sampling and event-driven profiling of execution time and memory usage.

For an in-depth tutorial and reference guide, please refer to the [`Profile` documentation](@ref lib-profiling).

Besides the `Profile` library, there are additional utilities included in `Base` and certain command line modes
that can assist with code profiling:

## Profiling Macros in Julia

Julia's Base library provides a set of macros that help profile your code for time and memory usage. Here is a brief introduction to each of them:

- [`@time`](@ref): Measures the execution time of an expression, the number of allocations, and the total bytes its execution caused to be allocated. Also shows the percentage of time spent in garbage collection, compiling new code, or recompiling invalidated code.
- [`@timev`](@ref): A verbose version of `@time` that prints additional non-zero memory allocation counters.
- [`@allocated`](@ref): Evaluates an expression, discards the resulting value, and returns the total number of bytes allocated during the evaluation.
- [`@timed`](@ref): Executes an expression and returns the value of the expression, elapsed time, total bytes allocated, garbage collection time, and an object with various memory allocation counters.
- [`@elapsed`](@ref): Evaluates an expression, discards the resulting value, and returns the number of seconds it took to execute as a floating-point number.
- [`@allocations`](@ref): Evaluates an expression, discards the resulting value, and returns the total number of allocations made during the evaluation.

These macros enable direct measurement of memory allocation and execution time for each line of code, aiding in identifying performance bottlenecks.

## GC Logging

While [`@time`](@ref) logs high-level stats about memory usage and garbage collection over the course
of evaluating an expression, it can be useful to log each garbage collection event, to get an
intuitive sense of how often the garbage collector is running, how long it's running each time,
and how much garbage it collects each time. This can be enabled with
[`GC.enable_logging(true)`](@ref), which causes Julia to log to stderr every time
a garbage collection happens.

## Allocation Tracking on a Line-by-Line Basis

Julia offers a command-line option, `--track-allocation=<setting>`, for tracking memory allocations. This option allows you to choose from three settings:

- `none`: The default setting that does not measure allocation.
- `user`: Measures memory allocation everywhere except in Julia's core code.
- `all`: Measures memory allocation at each line of Julia code.

With this feature, memory allocation is measured for each line of compiled code.
When you exit Julia, the cumulative results are written to text files with the `.mem` extension, located in the same directory as the source files.
Each line in these files indicates the total number of bytes allocated.

For analysis of these results, you can utilize tools from the [`Coverage` package](https://github.com/JuliaCI/Coverage.jl),
such as the function for sorting lines by the number of bytes allocated.

When interpreting the results, consider the following:

- Under the `user` setting, the first line of any function called directly from the REPL may show allocation due to events occurring within the REPL code itself.
- JIT-compilation can also contribute to allocation counts because much of Julia's compiler is written in Julia, and compilation typically requires memory allocation.

To achieve accurate results, it is recommended to first force compilation by executing all commands you wish to analyze.
Then, use [`Profile.clear_malloc_data()`](@ref) to reset all allocation counters.
Finally, execute the commands of interest and exit Julia to generate the `.mem` files.

!!! note

    The `--track-allocation` option alters code generation to log allocations, which may change the allocation results compared to those without the option.
    Therefore, it's recommended to use the [allocation profiler in the `Profile` library](@ref lib-profiling) for more accurate allocation profiling.


## External Profiling

Currently Julia supports `Intel VTune`, `OProfile` and `perf` as external profiling tools.

Depending on the tool you choose, compile with `USE_INTEL_JITEVENTS`, `USE_OPROFILE_JITEVENTS` and
`USE_PERF_JITEVENTS` set to 1 in `Make.user`. Multiple flags are supported.

Before running Julia set the environment variable `ENABLE_JITPROFILING` to 1.

Now you have a multitude of ways to employ those tools!
For example with `OProfile` you can try a simple recording :

```
>ENABLE_JITPROFILING=1 sudo operf -Vdebug ./julia test/fastmath.jl
>opreport -l `which ./julia`
```

Or similarly with `perf` :

```
$ ENABLE_JITPROFILING=1 perf record -o /tmp/perf.data --call-graph dwarf -k 1 ./julia /test/fastmath.jl
$ perf inject --jit --input /tmp/perf.data --output /tmp/perf-jit.data
$ perf report --call-graph -G -i /tmp/perf-jit.data
```

There are many more interesting things that you can measure about your program, to get a comprehensive list
please read the [Linux perf examples page](https://www.brendangregg.com/perf.html).

Remember that perf saves for each execution a `perf.data` file that, even for small programs, can get
quite large. Also the perf LLVM module saves temporarily debug objects in `~/.debug/jit`, remember
to clean that folder frequently.
