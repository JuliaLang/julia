# This file is a part of Julia. License is MIT: https://julialang.org/license

# This type must be kept in sync with the C struct in src/gc.h
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
    since_sweep     ::Int64 # GC internal
    collect         ::Csize_t # GC internal
    pause           ::Cint
    full_sweep      ::Cint
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
    # logic from `src/gc.c:jl_gc_total_bytes`
    old_allocd = gc_total_bytes(old)
    new_allocd = gc_total_bytes(new)
    return GC_Diff(new_allocd - old_allocd,
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

function padded_nonzero_print(value, str)
    if value != 0
        blanks = "                "[1:(18 - length(str))]
        println(str, ":", blanks, value)
    end
end


function format_bytes(bytes) # also used by InteractiveUtils
    bytes, mb = prettyprint_getunits(bytes, length(_mem_units), Int64(1024))
    if mb == 1
        return string(Int(bytes), " ", _mem_units[mb], bytes==1 ? "" : "s")
    else
        return string(Ryu.writefixed(Float64(bytes), 3), " ", _mem_units[mb])
    end
end

function time_print(elapsedtime, bytes=0, gctime=0, allocs=0)
    timestr = Ryu.writefixed(Float64(elapsedtime/1e9), 6)
    length(timestr) < 10 && print(" "^(10 - length(timestr)))
    print(timestr, " seconds")
    if bytes != 0 || allocs != 0
        allocs, ma = prettyprint_getunits(allocs, length(_cnt_units), Int64(1000))
        if ma == 1
            print(" (", Int(allocs), _cnt_units[ma], allocs==1 ? " allocation: " : " allocations: ")
        else
            print(" (", Ryu.writefixed(Float64(allocs), 2), _cnt_units[ma], " allocations: ")
        end
        print(format_bytes(bytes))
    end
    if gctime > 0
        print(", ", Ryu.writefixed(Float64(100*gctime/elapsedtime), 2), "% gc time")
    end
    if bytes != 0 || allocs != 0
        print(")")
    end
    nothing
end

function timev_print(elapsedtime, diff::GC_Diff)
    allocs = gc_alloc_count(diff)
    time_print(elapsedtime, diff.allocd, diff.total_time, allocs)
    print("\nelapsed time (ns): $elapsedtime\n")
    padded_nonzero_print(diff.total_time,   "gc time (ns)")
    padded_nonzero_print(diff.allocd,       "bytes allocated")
    padded_nonzero_print(diff.poolalloc,    "pool allocs")
    padded_nonzero_print(diff.bigalloc,     "non-pool GC allocs")
    padded_nonzero_print(diff.malloc,       "malloc() calls")
    padded_nonzero_print(diff.realloc,      "realloc() calls")
    padded_nonzero_print(diff.freecall,     "free() calls")
    padded_nonzero_print(diff.pause,        "GC pauses")
    padded_nonzero_print(diff.full_sweep,   "full collections")
end

"""
    @time

A macro to execute an expression, printing the time it took to execute, the number of
allocations, and the total number of bytes its execution caused to be allocated, before
returning the value of the expression.

See also [`@timev`](@ref), [`@timed`](@ref), [`@elapsed`](@ref), and
[`@allocated`](@ref).

!!! note
    For more serious benchmarking, consider the `@btime` macro from the BenchmarkTools.jl
    package which among other things evaluates the function multiple times in order to
    reduce noise.

```julia-repl
julia> @time rand(10^6);
  0.001525 seconds (7 allocations: 7.630 MiB)

julia> @time begin
           sleep(0.3)
           1+1
       end
  0.301395 seconds (8 allocations: 336 bytes)
2
```
"""
macro time(ex)
    quote
        while false; end # compiler heuristic: compile this block (alter this if the heuristic changes)
        local stats = gc_num()
        local elapsedtime = time_ns()
        local val = $(esc(ex))
        elapsedtime = time_ns() - elapsedtime
        local diff = GC_Diff(gc_num(), stats)
        time_print(elapsedtime, diff.allocd, diff.total_time,
                   gc_alloc_count(diff))
        println()
        val
    end
end

"""
    @timev

This is a verbose version of the `@time` macro. It first prints the same information as
`@time`, then any non-zero memory allocation counters, and then returns the value of the
expression.

See also [`@time`](@ref), [`@timed`](@ref), [`@elapsed`](@ref), and
[`@allocated`](@ref).

```julia-repl
julia> @timev rand(10^6);
  0.001006 seconds (7 allocations: 7.630 MiB)
elapsed time (ns): 1005567
bytes allocated:   8000256
pool allocs:       6
malloc() calls:    1
```
"""
macro timev(ex)
    quote
        while false; end # compiler heuristic: compile this block (alter this if the heuristic changes)
        local stats = gc_num()
        local elapsedtime = time_ns()
        local val = $(esc(ex))
        elapsedtime = time_ns() - elapsedtime
        timev_print(elapsedtime, GC_Diff(gc_num(), stats))
        val
    end
end

"""
    @elapsed

A macro to evaluate an expression, discarding the resulting value, instead returning the
number of seconds it took to execute as a floating-point number.

See also [`@time`](@ref), [`@timev`](@ref), [`@timed`](@ref),
and [`@allocated`](@ref).

```julia-repl
julia> @elapsed sleep(0.3)
0.301391426
```
"""
macro elapsed(ex)
    quote
        while false; end # compiler heuristic: compile this block (alter this if the heuristic changes)
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

See also [`@time`](@ref), [`@timev`](@ref), [`@timed`](@ref),
and [`@elapsed`](@ref).

```julia-repl
julia> @allocated rand(10^6)
8000080
```
"""
macro allocated(ex)
    quote
        while false; end # compiler heuristic: compile this block (alter this if the heuristic changes)
        local b0 = Ref{Int64}(0)
        local b1 = Ref{Int64}(0)
        gc_bytes(b0)
        $(esc(ex))
        gc_bytes(b1)
        b1[] - b0[]
    end
end

"""
    @timed

A macro to execute an expression, and return the value of the expression, elapsed time,
total bytes allocated, garbage collection time, and an object with various memory allocation
counters.

See also [`@time`](@ref), [`@timev`](@ref), [`@elapsed`](@ref), and
[`@allocated`](@ref).

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
```

!!! compat "Julia 1.5"
    The return type of this macro was changed from `Tuple` to `NamedTuple` in Julia 1.5.
"""
macro timed(ex)
    quote
        while false; end # compiler heuristic: compile this block (alter this if the heuristic changes)
        local stats = gc_num()
        local elapsedtime = time_ns()
        local val = $(esc(ex))
        elapsedtime = time_ns() - elapsedtime
        local diff = GC_Diff(gc_num(), stats)
        (value=val, time=elapsedtime/1e9, bytes=diff.allocd, gctime=diff.total_time/1e9, gcstats=diff)
    end
end
