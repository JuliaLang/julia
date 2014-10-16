# timing

# system date in seconds
time() = ccall(:clock_now, Float64, ())

# high-resolution relative time, in nanoseconds
time_ns() = ccall(:jl_hrtime, Uint64, ())

# total time spend in garbage collection, in nanoseconds
gc_time_ns() = ccall(:jl_gc_total_hrtime, Uint64, ())

# total number of bytes allocated so far
gc_bytes() = ccall(:jl_gc_total_bytes, Int64, ())

gc_num_pause() = ccall(:jl_gc_num_pause, Int64, ())
gc_num_full_sweep() = ccall(:jl_gc_num_full_sweep, Int64, ())

# reset the malloc log. Used to avoid counting memory allocated during compilation.
clear_malloc_data() = ccall(:jl_clear_malloc_data, Void, ())

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
    t0 = timers[1]::Uint64
    task_local_storage(:TIMERS, timers[2])
    (t1-t0)/1e9
end

function toc()
    t = toq()
    println("elapsed time: ", t, " seconds")
    return t
end

# print elapsed time, return expression value
const _units = ["bytes", "kB", "MB"]
function time_print(t, b, g, np, nfs)
    i = 1
    while b > 1024 && i < length(_units)
        b = div(b, 1024)
        i += 1
    end
    if 0 < g
        @printf("elapsed time: %s seconds (%d %s allocated, %.2f%% gc time in %d pauses with %d full sweep)\n", t/1e9, b, _units[i], 100*g/t, np, nfs)
    else
        @printf("elapsed time: %s seconds (%d %s allocated)\n", t/1e9, b, _units[i])
    end
end

macro time(ex)
    quote
        local b0 = gc_bytes()
        local t0 = time_ns()
        local g0 = gc_time_ns()
        local n0 = gc_num_pause()
        local nfs0 = gc_num_full_sweep()
        local val = $(esc(ex))
        local nfs1 = gc_num_full_sweep()
        local n1 = gc_num_pause()
        local g1 = gc_time_ns()
        local t1 = time_ns()
        local b1 = gc_bytes()
        time_print(t1-t0, b1-b0, g1-g0, n1-n0, nfs1-nfs0)
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

# measure bytes allocated without any contamination from compilation
macro allocated(ex)
    quote
        let
            local f
            function f()
                b0 = gc_bytes()
                $(esc(ex))
                b1 = gc_bytes()
                b1-b0
            end
            f()
        end
    end
end

# print nothing, return value, elapsed time, bytes allocated & gc time
macro timed(ex)
    quote
        local b0 = gc_bytes()
        local t0 = time_ns()
        local g0 = gc_time_ns()
        local val = $(esc(ex))
        local g1 = gc_time_ns()
        local t1 = time_ns()
        local b1 = gc_bytes()
        val, (t1-t0)/1e9, b1-b0, (g1-g0)/1e9
    end
end

# BLAS utility routines
function blas_vendor()
    try
        cglobal((:openblas_set_num_threads, Base.libblas_name), Void)
        return :openblas
    end
    try
        cglobal((:MKL_Set_Num_Threads, Base.libblas_name), Void)
        return :mkl
    end
    return :unknown
end

openblas_get_config() = strip(bytestring( ccall((:openblas_get_config, Base.libblas_name), Ptr{Uint8}, () )))

function blas_set_num_threads(n::Integer)
    blas = blas_vendor()
    if blas == :openblas
        return ccall((:openblas_set_num_threads, Base.libblas_name), Void, (Int32,), n)
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
    if blas == :openblas
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

print_with_color(color::Symbol, io::IO, msg::String...) =
    with_output_color(print, color, io, msg...)
print_with_color(color::Symbol, msg::String...) =
    print_with_color(color, STDOUT, msg...)
println_with_color(color::Symbol, io::IO, msg::String...) =
    with_output_color(println, color, io, msg...)
println_with_color(color::Symbol, msg::String...) =
    println_with_color(color, STDOUT, msg...)

## warnings and messages ##

function info(msg::String...; prefix="INFO: ")
    println_with_color(:blue, STDERR, prefix, chomp(string(msg...)))
end

# print a warning only once

const have_warned = Set()
warn_once(msg::String...) = warn(msg..., once=true)

function warn(msg::String...; prefix="WARNING: ", once=false, key=nothing, bt=nothing)
    str = chomp(bytestring(msg...))
    if once
        if key === nothing
            key = str
        end
        (key in have_warned) && return
        push!(have_warned, key)
    end
    with_output_color(:red, STDERR) do io
        print(io, prefix, str)
        if bt !== nothing
            show_backtrace(io, bt)
        end
        println(io)
    end
end

warn(err::Exception; prefix="ERROR: ", kw...) =
    warn(sprint(io->showerror(io,err)), prefix=prefix; kw...)
