# This file is a part of Julia. License is MIT: https://julialang.org/license

# This type must be kept in sync with the C struct in src/gc-interface.h
struct GC_Num
    allocd          ::Int64 # GC internal
    deferred_alloc  ::Int64 # GC internal
    freed           ::Int64 # GC internal
    malloc          ::Int64
    realloc         ::Int64
    poolalloc       ::Int64
    bigalloc        ::Int64
    freecall        ::Int64
    total_time      ::Int64
    total_allocd    ::Int64 # GC internal
    collect         ::Csize_t # GC internal
    pause           ::Cint
    full_sweep      ::Cint
    max_pause       ::Int64
    max_memory      ::Int64
    time_to_safepoint           ::Int64
    max_time_to_safepoint       ::Int64
    total_time_to_safepoint     ::Int64
    sweep_time      ::Int64
    mark_time       ::Int64
    stack_pool_sweep_time ::Int64
    total_sweep_time  ::Int64
    total_sweep_page_walk_time              ::Int64
    total_sweep_madvise_time                ::Int64
    total_sweep_free_mallocd_memory_time    ::Int64
    total_mark_time   ::Int64
    total_stack_pool_sweep_time::Int64
    last_full_sweep ::Int64
    last_incremental_sweep ::Int64
end

gc_num() = ccall(:jl_gc_num, GC_Num, ())

# This type is to represent differences in the counters, so fields may be negative
struct GC_Diff
    allocd      ::Int64 # Bytes allocated
    malloc      ::Int64 # Number of GC aware malloc()
    realloc     ::Int64 # Number of GC aware realloc()
    poolalloc   ::Int64 # Number of pool allocation
    bigalloc    ::Int64 # Number of big (non-pool) allocation
    freecall    ::Int64 # Number of GC aware free()
    total_time  ::Int64 # Time spent in garbage collection
    pause       ::Int64 # Number of GC pauses
    full_sweep  ::Int64 # Number of GC full collection
end

gc_total_bytes(gc_num::GC_Num) =
    gc_num.allocd + gc_num.deferred_alloc + gc_num.total_allocd

function GC_Diff(new::GC_Num, old::GC_Num)
    # logic from `jl_gc_total_bytes`
    old_allocd = gc_total_bytes(old)
    new_allocd = gc_total_bytes(new)
    return GC_Diff(new_allocd       - old_allocd,
                   new.malloc       - old.malloc,
                   new.realloc      - old.realloc,
                   new.poolalloc    - old.poolalloc,
                   new.bigalloc     - old.bigalloc,
                   new.freecall     - old.freecall,
                   new.total_time   - old.total_time,
                   new.pause        - old.pause,
                   new.full_sweep   - old.full_sweep)
end

function gc_alloc_count(diff::GC_Diff)
    diff.malloc + diff.realloc + diff.poolalloc + diff.bigalloc
end

# cumulative total time spent on compilation and recompilation, in nanoseconds
function cumulative_compile_time_ns()
    comp = ccall(:jl_cumulative_compile_time_ns, UInt64, ())
    recomp = ccall(:jl_cumulative_recompile_time_ns, UInt64, ())
    return comp, recomp
end

function cumulative_compile_timing(b::Bool)
    if b
        ccall(:jl_cumulative_compile_timing_enable, Cvoid, ())
    else
        ccall(:jl_cumulative_compile_timing_disable, Cvoid, ())
    end
    return
end

# total time spend in garbage collection, in nanoseconds
gc_time_ns() = ccall(:jl_gc_total_hrtime, UInt64, ())

"""
    Base.gc_live_bytes()

Return the total size (in bytes) of objects currently in memory.
This is computed as the total size of live objects after
the last garbage collection, plus the number of bytes allocated
since then.
"""
function gc_live_bytes()
    num = gc_num()
    Int(ccall(:jl_gc_live_bytes, Int64, ())) + num.allocd + num.deferred_alloc
end

# must be kept in sync with the value from `src/julia_threads.h``
const JL_GC_N_MAX_POOLS = 51
function gc_page_utilization_data()
    page_utilization_raw = cglobal(:jl_gc_page_utilization_stats, Float64)
    return Base.unsafe_wrap(Array, page_utilization_raw, JL_GC_N_MAX_POOLS, own=false)
end


const USING_STOCK_GC = occursin("stock", unsafe_string(ccall(:jl_gc_active_impl, Ptr{UInt8}, ())))
# Full sweep reasons are currently only available for the stock GC
@static if USING_STOCK_GC
# must be kept in sync with `src/gc-stock.h``
const FULL_SWEEP_REASONS = [:FULL_SWEEP_REASON_SWEEP_ALWAYS_FULL, :FULL_SWEEP_REASON_FORCED_FULL_SWEEP,
                            :FULL_SWEEP_REASON_USER_MAX_EXCEEDED, :FULL_SWEEP_REASON_LARGE_PROMOTION_RATE]
end

"""
    Base.full_sweep_reasons()

Return a dictionary of the number of times each full sweep reason has occurred.

The reasons are:
- `:FULL_SWEEP_REASON_SWEEP_ALWAYS_FULL`: Full sweep was caused due to `always_full` being set in the GC debug environment
- `:FULL_SWEEP_REASON_FORCED_FULL_SWEEP`: Full sweep was forced by `GC.gc(true)`
- `:FULL_SWEEP_REASON_USER_MAX_EXCEEDED`: Full sweep was forced due to the system reaching the heap soft size limit
- `:FULL_SWEEP_REASON_LARGE_PROMOTION_RATE`: Full sweep was forced by a large promotion rate across GC generations

Note that the set of reasons is not guaranteed to be stable across minor versions of Julia.
"""
function full_sweep_reasons()
    d = Dict{Symbol, Int64}()
    # populate the dictionary according to the reasons above for the stock GC
    # otherwise return an empty dictionary for now
    @static if USING_STOCK_GC
        reason = cglobal(:jl_full_sweep_reasons, UInt64)
        reasons_as_array = Base.unsafe_wrap(Vector{UInt64}, reason, length(FULL_SWEEP_REASONS), own=false)
        for (i, r) in enumerate(FULL_SWEEP_REASONS)
            d[r] = reasons_as_array[i]
        end
    end
    return d
end

"""
    Base.jit_total_bytes()

Return the total amount (in bytes) allocated by the just-in-time compiler
for e.g. native code and data.
"""
function jit_total_bytes()
    return ccall(:jl_jit_total_bytes, Csize_t, ())
end

# print elapsed time, return expression value
const _mem_units = ["byte", "KiB", "MiB", "GiB", "TiB", "PiB"]
const _cnt_units = ["", " k", " M", " G", " T", " P"]
function prettyprint_getunits(value, numunits, factor)
    if value == 0 || value == 1
        return (value, 1)
    end
    unit = ceil(Int, log(value) / log(factor))
    unit = min(numunits, unit)
    number = value/factor^(unit-1)
    return number, unit
end

function padded_nonzero_print(value, str, always_print = true)
    if always_print || value != 0
        blanks = "                "[1:(19 - length(str))]
        println(str, ":", blanks, value)
    end
end

"""
    format_bytes(bytes; binary=true)

Format a given number of bytes into a human-readable string.

# Arguments
- `bytes`: The number of bytes to format.
- `binary=true`: If `true`, formats the bytes in binary units (powers of 1024). If `false`, uses decimal units (powers of 1000).

# Returns
`String`: A human-readable string representation of the bytes, formatted in either binary or decimal units based on the `binary` argument.

# Examples
```jldoctest
julia> Base.format_bytes(1024)
"1024 bytes"

julia> Base.format_bytes(10000)
"9.766 KiB"

julia> Base.format_bytes(10000, binary=false)
"10.000 kB"
```
"""
function format_bytes(bytes; binary=true) # also used by InteractiveUtils
    units = binary ? _mem_units : _cnt_units
    factor = binary ? 1024 : 1000
    bytes, mb = prettyprint_getunits(bytes, length(units), Int64(factor))
    if mb == 1
        return string(Int(bytes), " ", _mem_units[mb], bytes==1 ? "" : "s")
    else
        return string(Ryu.writefixed(Float64(bytes), 3), binary ? " $(units[mb])" : "$(units[mb])B")
    end
end

function time_print(io::IO, elapsedtime, bytes=0, gctime=0, allocs=0, lock_conflicts=0, compile_time=0, recompile_time=0, newline=false;
                    msg::Union{String,Nothing}=nothing)
    timestr = Ryu.writefixed(Float64(elapsedtime/1e9), 6)
    str = sprint() do io
        if msg isa String
            print(io, msg, ": ")
        else
            print(io, length(timestr) < 10 ? (" "^(10 - length(timestr))) : "")
        end
        print(io, timestr, " seconds")
        parens = bytes != 0 || allocs != 0 || gctime > 0 || lock_conflicts > 0 || compile_time > 0
        parens && print(io, " (")
        if bytes != 0 || allocs != 0
            allocs, ma = prettyprint_getunits(allocs, length(_cnt_units), Int64(1000))
            if ma == 1
                print(io, Int(allocs), _cnt_units[ma], allocs==1 ? " allocation: " : " allocations: ")
            else
                print(io, Ryu.writefixed(Float64(allocs), 2), _cnt_units[ma], " allocations: ")
            end
            print(io, format_bytes(bytes))
        end
        if gctime > 0
            if bytes != 0 || allocs != 0
                print(io, ", ")
            end
            print(io, Ryu.writefixed(Float64(100*gctime/elapsedtime), 2), "% gc time")
        end
        if lock_conflicts > 0
            if bytes != 0 || allocs != 0 || gctime > 0
                print(io, ", ")
            end
            plural = lock_conflicts == 1 ? "" : "s"
            print(io, lock_conflicts, " lock conflict$plural")
        end
        if compile_time > 0
            if bytes != 0 || allocs != 0 || gctime > 0 || lock_conflicts > 0
                print(io, ", ")
            end
            print(io, Ryu.writefixed(Float64(100*compile_time/elapsedtime), 2), "% compilation time")
        end
        if recompile_time > 0
            perc = Float64(100 * recompile_time / compile_time)
            # use "<1" to avoid the confusing UX of reporting 0% when it's >0%
            print(io, ": ", perc < 1 ? "<1" : Ryu.writefixed(perc, 0), "% of which was recompilation")
        end
        parens && print(io, ")")
        newline && print(io, "\n")
    end
    print(io, str)
    nothing
end

function timev_print(elapsedtime, diff::GC_Diff, lock_conflicts, compile_times; msg::Union{String,Nothing}=nothing)
    allocs = gc_alloc_count(diff)
    compile_time = first(compile_times)
    recompile_time = last(compile_times)
    time_print(stdout, elapsedtime, diff.allocd, diff.total_time, allocs, lock_conflicts, compile_time, recompile_time, true; msg)
    padded_nonzero_print(elapsedtime,       "elapsed time (ns)")
    padded_nonzero_print(diff.total_time,   "gc time (ns)")
    padded_nonzero_print(diff.allocd,       "bytes allocated")
    padded_nonzero_print(diff.poolalloc,    "pool allocs")
    padded_nonzero_print(diff.bigalloc,     "non-pool GC allocs")
    padded_nonzero_print(diff.malloc,       "malloc() calls", false)
    padded_nonzero_print(diff.realloc,      "realloc() calls", false)
    # always print number of frees if there are mallocs
    padded_nonzero_print(diff.freecall,     "free() calls", diff.malloc > 0)
    minor_collects = diff.pause - diff.full_sweep
    padded_nonzero_print(minor_collects,    "minor collections")
    padded_nonzero_print(diff.full_sweep,   "full collections")
end

# Like a try-finally block, except without introducing the try scope
# NOTE: This is deprecated and should not be used from user logic. A proper solution to
# this problem will be introduced in https://github.com/JuliaLang/julia/pull/39217
macro __tryfinally(ex, fin)
    Expr(:tryfinally,
       :($(esc(ex))),
       :($(esc(fin)))
       )
end

"""
    @time expr
    @time "description" expr

A macro to execute an expression, printing the time it took to execute, the number of
allocations, and the total number of bytes its execution caused to be allocated, before
returning the value of the expression. Any time spent garbage collecting (gc), compiling
new code, or recompiling invalidated code is shown as a percentage. Any lock conflicts
where a [`ReentrantLock`](@ref) had to wait are shown as a count.

Optionally provide a description string to print before the time report.

In some cases the system will look inside the `@time` expression and compile some of the
called code before execution of the top-level expression begins. When that happens, some
compilation time will not be counted. To include this time you can run `@time @eval ...`.

See also [`@showtime`](@ref), [`@timev`](@ref), [`@timed`](@ref), [`@elapsed`](@ref),
[`@allocated`](@ref), and [`@allocations`](@ref).

!!! note
    For more serious benchmarking, consider the `@btime` macro from the BenchmarkTools.jl
    package which among other things evaluates the function multiple times in order to
    reduce noise.

!!! compat "Julia 1.8"
    The option to add a description was introduced in Julia 1.8.

    Recompilation time being shown separately from compilation time was introduced in Julia 1.8

!!! compat "Julia 1.11"
    The reporting of any lock conflicts was added in Julia 1.11.

```julia-repl
julia> x = rand(10,10);

julia> @time x * x;
  0.606588 seconds (2.19 M allocations: 116.555 MiB, 3.75% gc time, 99.94% compilation time)

julia> @time x * x;
  0.000009 seconds (1 allocation: 896 bytes)

julia> @time begin
           sleep(0.3)
           1+1
       end
  0.301395 seconds (8 allocations: 336 bytes)
2

julia> @time "A one second sleep" sleep(1)
A one second sleep: 1.005750 seconds (5 allocations: 144 bytes)

julia> for loop in 1:3
            @time loop sleep(1)
        end
1: 1.006760 seconds (5 allocations: 144 bytes)
2: 1.001263 seconds (5 allocations: 144 bytes)
3: 1.003676 seconds (5 allocations: 144 bytes)
```
"""
macro time(ex)
    quote
        @time nothing $(esc(ex))
    end
end
macro time(msg, ex)
    quote
        local ret = @timed $(esc(ex))
        local _msg = $(esc(msg))
        local _msg_str = _msg === nothing ? _msg : string(_msg)
        time_print(stdout, ret.time*1e9, ret.gcstats.allocd, ret.gcstats.total_time, gc_alloc_count(ret.gcstats), ret.lock_conflicts, ret.compile_time*1e9, ret.recompile_time*1e9, true; msg=_msg_str)
        ret.value
    end
end

"""
    @showtime expr

Like `@time` but also prints the expression being evaluated for reference.

!!! compat "Julia 1.8"
    This macro was added in Julia 1.8.

See also [`@time`](@ref).

```julia-repl
julia> @showtime sleep(1)
sleep(1): 1.002164 seconds (4 allocations: 128 bytes)
```
"""
macro showtime(ex)
    quote
        @time $(sprint(show_unquoted,ex)) $(esc(ex))
    end
end

"""
    @timev expr
    @timev "description" expr

This is a verbose version of the `@time` macro. It first prints the same information as
`@time`, then any non-zero memory allocation counters, and then returns the value of the
expression.

Optionally provide a description string to print before the time report.

!!! compat "Julia 1.8"
    The option to add a description was introduced in Julia 1.8.

See also [`@time`](@ref), [`@timed`](@ref), [`@elapsed`](@ref),
[`@allocated`](@ref), and [`@allocations`](@ref).

```julia-repl
julia> x = rand(10,10);

julia> @timev x * x;
  0.546770 seconds (2.20 M allocations: 116.632 MiB, 4.23% gc time, 99.94% compilation time)
elapsed time (ns): 546769547
gc time (ns):      23115606
bytes allocated:   122297811
pool allocs:       2197930
non-pool GC allocs:1327
malloc() calls:    36
realloc() calls:   5
GC pauses:         3

julia> @timev x * x;
  0.000010 seconds (1 allocation: 896 bytes)
elapsed time (ns): 9848
bytes allocated:   896
pool allocs:       1
```
"""
macro timev(ex)
    quote
        @timev nothing $(esc(ex))
    end
end
macro timev(msg, ex)
    quote
        local ret = @timed $(esc(ex))
        local _msg = $(esc(msg))
        local _msg_str = _msg === nothing ? _msg : string(_msg)
        timev_print(ret.time*1e9, ret.gcstats, ret.lock_conflicts, (ret.compile_time*1e9, ret.recompile_time*1e9); msg=_msg_str)
        ret.value
    end
end

"""
    @elapsed

A macro to evaluate an expression, discarding the resulting value, instead returning the
number of seconds it took to execute as a floating-point number.

In some cases the system will look inside the `@elapsed` expression and compile some of the
called code before execution of the top-level expression begins. When that happens, some
compilation time will not be counted. To include this time you can run `@elapsed @eval ...`.

See also [`@time`](@ref), [`@timev`](@ref), [`@timed`](@ref),
[`@allocated`](@ref), and [`@allocations`](@ref).

```julia-repl
julia> @elapsed sleep(0.3)
0.301391426
```
"""
macro elapsed(ex)
    quote
        Experimental.@force_compile
        local t0 = time_ns()
        $(esc(ex))
        (time_ns() - t0) / 1e9
    end
end

# total number of bytes allocated so far
gc_bytes(b::Ref{Int64}) = ccall(:jl_gc_get_total_bytes, Cvoid, (Ptr{Int64},), b)
# NOTE: gc_bytes() is deprecated
function gc_bytes()
    b = Ref{Int64}()
    gc_bytes(b)
    b[]
end

"""
    @allocated

A macro to evaluate an expression, discarding the resulting value, instead returning the
total number of bytes allocated during evaluation of the expression.

See also [`@allocations`](@ref), [`@time`](@ref), [`@timev`](@ref), [`@timed`](@ref),
and [`@elapsed`](@ref).

```julia-repl
julia> @allocated rand(10^6)
8000080
```
"""
macro allocated(ex)
    quote
        Experimental.@force_compile
        local b0 = Ref{Int64}(0)
        local b1 = Ref{Int64}(0)
        gc_bytes(b0)
        $(esc(ex))
        gc_bytes(b1)
        b1[] - b0[]
    end
end

"""
    @allocations

A macro to evaluate an expression, discard the resulting value, and instead return the
total number of allocations during evaluation of the expression.

See also [`@allocated`](@ref), [`@time`](@ref), [`@timev`](@ref), [`@timed`](@ref),
and [`@elapsed`](@ref).

```julia-repl
julia> @allocations rand(10^6)
2
```

!!! compat "Julia 1.9"
    This macro was added in Julia 1.9.
"""
macro allocations(ex)
    quote
        Experimental.@force_compile
        local stats = Base.gc_num()
        $(esc(ex))
        local diff = Base.GC_Diff(Base.gc_num(), stats)
        Base.gc_alloc_count(diff)
    end
end

"""
    @lock_conflicts

A macro to evaluate an expression, discard the resulting value, and instead return the
total number of lock conflicts during evaluation, where a lock attempt on a [`ReentrantLock`](@ref)
resulted in a wait because the lock was already held.

See also [`@time`](@ref), [`@timev`](@ref) and [`@timed`](@ref).

```julia-repl
julia> @lock_conflicts begin
    l = ReentrantLock()
    Threads.@threads for i in 1:Threads.nthreads()
        lock(l) do
        sleep(1)
        end
    end
end
5
```

!!! compat "Julia 1.11"
    This macro was added in Julia 1.11.
"""
macro lock_conflicts(ex)
    quote
        Threads.lock_profiling(true)
        local lock_conflicts = Threads.LOCK_CONFLICT_COUNT[]
        try
            $(esc(ex))
        finally
            Threads.lock_profiling(false)
        end
        Threads.LOCK_CONFLICT_COUNT[] - lock_conflicts
    end
end

"""
    @timed

A macro to execute an expression, and return the value of the expression, elapsed time in seconds,
total bytes allocated, garbage collection time, an object with various memory allocation
counters, compilation time in seconds, and recompilation time in seconds. Any lock conflicts
where a [`ReentrantLock`](@ref) had to wait are shown as a count.

In some cases the system will look inside the `@timed` expression and compile some of the
called code before execution of the top-level expression begins. When that happens, some
compilation time will not be counted. To include this time you can run `@timed @eval ...`.

See also [`@time`](@ref), [`@timev`](@ref), [`@elapsed`](@ref),
[`@allocated`](@ref), [`@allocations`](@ref), and [`@lock_conflicts`](@ref).

```julia-repl
julia> stats = @timed rand(10^6);

julia> stats.time
0.006634834

julia> stats.bytes
8000256

julia> stats.gctime
0.0055765

julia> propertynames(stats.gcstats)
(:allocd, :malloc, :realloc, :poolalloc, :bigalloc, :freecall, :total_time, :pause, :full_sweep)

julia> stats.gcstats.total_time
5576500

julia> stats.compile_time
0.0

julia> stats.recompile_time
0.0

```

!!! compat "Julia 1.5"
    The return type of this macro was changed from `Tuple` to `NamedTuple` in Julia 1.5.

!!! compat "Julia 1.11"
    The `lock_conflicts`, `compile_time`, and `recompile_time` fields were added in Julia 1.11.
"""
macro timed(ex)
    quote
        Experimental.@force_compile
        Threads.lock_profiling(true)
        local lock_conflicts = Threads.LOCK_CONFLICT_COUNT[]
        local stats = gc_num()
        local elapsedtime = time_ns()
        cumulative_compile_timing(true)
        local compile_elapsedtimes = cumulative_compile_time_ns()
        local val = @__tryfinally($(esc(ex)),
            (elapsedtime = time_ns() - elapsedtime;
            cumulative_compile_timing(false);
            compile_elapsedtimes = cumulative_compile_time_ns() .- compile_elapsedtimes;
            lock_conflicts = Threads.LOCK_CONFLICT_COUNT[] - lock_conflicts;
            Threads.lock_profiling(false))
        )
        local diff = GC_Diff(gc_num(), stats)
        (
            value=val,
            time=elapsedtime/1e9,
            bytes=diff.allocd,
            gctime=diff.total_time/1e9,
            gcstats=diff,
            lock_conflicts=lock_conflicts,
            compile_time=compile_elapsedtimes[1]/1e9,
            recompile_time=compile_elapsedtimes[2]/1e9
        )
    end
end

# Exported, documented, and tested in InteractiveUtils
# here so it's possible to time/trace all imports, including InteractiveUtils and its deps
macro time_imports(ex)
    quote
        try
            Base.Threads.atomic_add!(Base.TIMING_IMPORTS, 1)
            $(esc(ex))
        finally
            Base.Threads.atomic_sub!(Base.TIMING_IMPORTS, 1)
        end
    end
end

macro trace_compile(ex)
    quote
        try
            ccall(:jl_force_trace_compile_timing_enable, Cvoid, ())
            $(esc(ex))
        finally
            ccall(:jl_force_trace_compile_timing_disable, Cvoid, ())
        end
    end
end

macro trace_dispatch(ex)
    quote
        try
            ccall(:jl_force_trace_dispatch_enable, Cvoid, ())
            $(esc(ex))
        finally
            ccall(:jl_force_trace_dispatch_disable, Cvoid, ())
        end
    end
end
