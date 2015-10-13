# This file is a part of Julia. License is MIT: http://julialang.org/license

# timing

# time() in libc.jl

# high-resolution relative time, in nanoseconds
time_ns() = ccall(:jl_hrtime, UInt64, ())

# This type must be kept in sync with the C struct in src/gc.c
immutable GC_Num
    allocd      ::Int64 # GC internal
    freed       ::Int64 # GC internal
    malloc      ::UInt64
    realloc     ::UInt64
    poolalloc   ::UInt64
    bigalloc    ::UInt64
    freecall    ::UInt64
    total_time  ::UInt64
    total_allocd::UInt64 # GC internal
    since_sweep ::UInt64 # GC internal
    collect     ::Csize_t # GC internal
    pause       ::Cint
    full_sweep  ::Cint
end

gc_num() = ccall(:jl_gc_num, GC_Num, ())

# This type is to represent differences in the counters, so fields may be negative
immutable GC_Diff
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

function GC_Diff(new::GC_Num, old::GC_Num)
    # logic from gc.c:jl_gc_total_bytes
    old_allocd = old.allocd + Int64(old.collect) + Int64(old.total_allocd)
    new_allocd = new.allocd + Int64(new.collect) + Int64(new.total_allocd)
    return GC_Diff(new_allocd - old_allocd,
                   Int64(new.malloc       - old.malloc),
                   Int64(new.realloc      - old.realloc),
                   Int64(new.poolalloc    - old.poolalloc),
                   Int64(new.bigalloc     - old.bigalloc),
                   Int64(new.freecall     - old.freecall),
                   Int64(new.total_time   - old.total_time),
                   new.pause              - old.pause,
                   new.full_sweep         - old.full_sweep)
end

function gc_alloc_count(diff::GC_Diff)
    diff.malloc + diff.realloc + diff.poolalloc + diff.bigalloc
end


# total time spend in garbage collection, in nanoseconds
gc_time_ns() = ccall(:jl_gc_total_hrtime, UInt64, ())

# total number of bytes allocated so far
gc_bytes() = ccall(:jl_gc_total_bytes, Int64, ())

function tic()
    t0 = time_ns()
    task_local_storage(:TIMERS, (t0, get(task_local_storage(), :TIMERS, ())))
    return t0
end

function toq()
    t1 = time_ns()
    timers = get(task_local_storage(), :TIMERS, ())
    if is(timers,())
        error("toc() without tic()")
    end
    t0 = timers[1]::UInt64
    task_local_storage(:TIMERS, timers[2])
    (t1-t0)/1e9
end

function toc()
    t = toq()
    println("elapsed time: ", t, " seconds")
    return t
end

# print elapsed time, return expression value
const _mem_units = ["byte", "KB", "MB", "GB", "TB", "PB"]
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

function padded_nonzero_print(value,str)
    if value != 0
        blanks = "                "[1:18-length(str)]
        println("$str:$blanks$value")
    end
end

function time_print(elapsedtime, bytes, gctime, allocs)
    @printf("%10.6f seconds", elapsedtime/1e9)
    if bytes != 0 || allocs != 0
        bytes, mb = prettyprint_getunits(bytes, length(_mem_units), Int64(1024))
        allocs, ma = prettyprint_getunits(allocs, length(_cnt_units), Int64(1000))
        if ma == 1
            @printf(" (%d%s allocation%s: ", allocs, _cnt_units[ma], allocs==1 ? "" : "s")
        else
            @printf(" (%.2f%s allocations: ", allocs, _cnt_units[ma])
        end
        if mb == 1
            @printf("%d %s%s", bytes, _mem_units[mb], bytes==1 ? "" : "s")
        else
            @printf("%.3f %s", bytes, _mem_units[mb])
        end
        if gctime > 0
            @printf(", %.2f%% gc time", 100*gctime/elapsedtime)
        end
        print(")")
    elseif gctime > 0
        @printf(", %.2f%% gc time", 100*gctime/elapsedtime)
    end
    println()
end

function timev_print(elapsedtime, diff::GC_Diff)
    allocs = gc_alloc_count(diff)
    time_print(elapsedtime, diff.allocd, diff.total_time, allocs)
    print("elapsed time (ns): $elapsedtime\n")
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

macro time(ex)
    quote
        local stats = gc_num()
        local elapsedtime = time_ns()
        local val = $(esc(ex))
        elapsedtime = time_ns() - elapsedtime
        local diff = GC_Diff(gc_num(), stats)
        time_print(elapsedtime, diff.allocd, diff.total_time,
                   gc_alloc_count(diff))
        val
    end
end

macro timev(ex)
    quote
        local stats = gc_num()
        local elapsedtime = time_ns()
        local val = $(esc(ex))
        elapsedtime = time_ns() - elapsedtime
        timev_print(elapsedtime, GC_Diff(gc_num(), stats))
        val
    end
end

# print nothing, return elapsed time
macro elapsed(ex)
    quote
        local t0 = time_ns()
        local val = $(esc(ex))
        (time_ns()-t0)/1e9
    end
end

# measure bytes allocated without *most* contamination from compilation
# Note: This reports a different value from the @time macros, because
# it wraps the call in a function, however, this means that things
# like:  @allocated y = foo()
# will not work correctly, because it will set y in the context of
# the local function made by the macro, not the current function

macro allocated(ex)
    quote
        let
            local f
            function f()
                b0 = gc_bytes()
                $(esc(ex))
                gc_bytes() - b0
            end
            f()
        end
    end
end

# print nothing, return value, elapsed time, bytes allocated & gc time
macro timed(ex)
    quote
        local stats = gc_num()
        local elapsedtime = time_ns()
        local val = $(esc(ex))
        elapsedtime = time_ns() - elapsedtime
        local diff = GC_Diff(gc_num(), stats)
        val, elapsedtime/1e9, diff.allocd, diff.total_time/1e9, diff
    end
end

# BLAS utility routines
function blas_vendor()
    try
        cglobal((:openblas_set_num_threads, Base.libblas_name), Void)
        return :openblas
    end
    try
        cglobal((:openblas_set_num_threads64_, Base.libblas_name), Void)
        return :openblas64
    end
    try
        cglobal((:MKL_Set_Num_Threads, Base.libblas_name), Void)
        return :mkl
    end
    return :unknown
end

if blas_vendor() == :openblas64
    blasfunc(x) = string(x)*"64_"
    openblas_get_config() = strip(bytestring( ccall((:openblas_get_config64_, Base.libblas_name), Ptr{UInt8}, () )))
else
    blasfunc(x) = string(x)
    openblas_get_config() = strip(bytestring( ccall((:openblas_get_config, Base.libblas_name), Ptr{UInt8}, () )))
end

function blas_set_num_threads(n::Integer)
    blas = blas_vendor()
    if blas == :openblas
        return ccall((:openblas_set_num_threads, Base.libblas_name), Void, (Int32,), n)
    elseif blas == :openblas64
        return ccall((:openblas_set_num_threads64_, Base.libblas_name), Void, (Int32,), n)
    elseif blas == :mkl
        # MKL may let us set the number of threads in several ways
        return ccall((:MKL_Set_Num_Threads, Base.libblas_name), Void, (Cint,), n)
    end

    # OSX BLAS looks at an environment variable
    @osx_only ENV["VECLIB_MAXIMUM_THREADS"] = n

    return nothing
end

function check_blas()
    blas = blas_vendor()
    if blas == :openblas || blas == :openblas64
        openblas_config = openblas_get_config()
        openblas64 = ismatch(r".*USE64BITINT.*", openblas_config)
        if Base.USE_BLAS64 != openblas64
            if !openblas64
                println("ERROR: OpenBLAS was not built with 64bit integer support.")
                println("You're seeing this error because Julia was built with USE_BLAS64=1")
                println("Please rebuild Julia with USE_BLAS64=0")
            else
                println("ERROR: Julia was not built with support for OpenBLAS with 64bit integer support")
                println("You're seeing this error because Julia was built with USE_BLAS64=0")
                println("Please rebuild Julia with USE_BLAS64=1")
            end
            println("Quitting.")
            quit()
        end
    elseif blas == :mkl
        if Base.USE_BLAS64
            ENV["MKL_INTERFACE_LAYER"] = "ILP64"
        end
    end

    #
    # Check if BlasInt is the expected bitsize, by triggering an error
    #
    (_, info) = LinAlg.LAPACK.potrf!('U', [1.0 0.0; 0.0 -1.0])
    if info != 2 # mangled info code
        if info == 2^33
            error("""BLAS and LAPACK are compiled with 32-bit integer support, but Julia expects 64-bit integers. Please build Julia with USE_BLAS64=0.""")
        elseif info == 0
            error("""BLAS and LAPACK are compiled with 64-bit integer support but Julia expects 32-bit integers. Please build Julia with USE_BLAS64=1.""")
        else
            error("""The LAPACK library produced an undefined error code. Please verify the installation of BLAS and LAPACK.""")
        end
    end

end

function fftw_vendor()
    if Base.libfftw_name == "libmkl_rt"
        return :mkl
    else
        return :fftw
    end
end


## printing with color ##

function with_output_color(f::Function, color::Symbol, io::IO, args...)
    buf = IOBuffer()
    have_color && print(buf, get(text_colors, color, color_normal))
    try f(buf, args...)
    finally
        have_color && print(buf, color_normal)
        print(io, takebuf_string(buf))
    end
end

print_with_color(color::Symbol, io::IO, msg::AbstractString...) =
    with_output_color(print, color, io, msg...)
print_with_color(color::Symbol, msg::AbstractString...) =
    print_with_color(color, STDOUT, msg...)
println_with_color(color::Symbol, io::IO, msg::AbstractString...) =
    with_output_color(println, color, io, msg...)
println_with_color(color::Symbol, msg::AbstractString...) =
    println_with_color(color, STDOUT, msg...)

## warnings and messages ##

function info(io::IO, msg...; prefix="INFO: ")
    println_with_color(info_color(), io, prefix, chomp(string(msg...)))
end
info(msg...; prefix="INFO: ") = info(STDERR, msg..., prefix=prefix)

# print a warning only once

const have_warned = Set()

warn_once(io::IO, msg...) = warn(io, msg..., once=true)
warn_once(msg...) = warn(STDERR, msg..., once=true)

function warn(io::IO, msg...;
              prefix="WARNING: ", once=false, key=nothing, bt=nothing,
              filename=nothing, lineno::Int=0)
    str = chomp(string(msg...))
    if once
        if key === nothing
            key = str
        end
        (key in have_warned) && return
        push!(have_warned, key)
    end
    print_with_color(warn_color(), io, prefix, str)
    if bt !== nothing
        show_backtrace(io, bt)
    end
    if filename !== nothing
        print(io, "\nwhile loading $filename, in expression starting on line $lineno")
    end
    println(io)
    return
end
warn(msg...; kw...) = warn(STDERR, msg...; kw...)

warn(io::IO, err::Exception; prefix="ERROR: ", kw...) =
    warn(io, sprint(buf->showerror(buf, err)), prefix=prefix; kw...)

warn(err::Exception; prefix="ERROR: ", kw...) =
    warn(STDERR, err, prefix=prefix; kw...)

function julia_cmd(julia=joinpath(JULIA_HOME, julia_exename()))
    opts = JLOptions()
    cpu_target = bytestring(opts.cpu_target)
    image_file = bytestring(opts.image_file)
    `$julia -C$cpu_target -J$image_file`
end

julia_exename() = ccall(:jl_is_debugbuild,Cint,())==0 ? "julia" : "julia-debug"
