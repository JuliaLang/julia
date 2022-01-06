module Allocs

using Base.StackTraces: StackTrace, StackFrame, lookup
using Base: InterpreterIP

# raw results

# matches RawBacktrace on the C side
struct RawBacktrace
    data::Ptr{Csize_t} # in C: *jl_bt_element_t
    size::Csize_t
end

# matches RawAlloc on the C side
struct RawAlloc
    type::Ptr{Type}
    backtrace::RawBacktrace
    size::Csize_t
end

struct FreeInfo
    type::Ptr{Type}
    count::UInt
end

# matches RawAllocResults on the C side
struct RawAllocResults
    allocs::Ptr{RawAlloc}
    num_allocs::Csize_t

    frees::Ptr{FreeInfo}
    num_frees::Csize_t
end

"""
    AllocProfile.@profile [skip_every=10_000] ex

Profile allocations that happen during `my_function`, returning
both the result and and AllocResults struct.
"""
macro profile(opts, ex)
    _prof_expr(ex, opts)
end
macro profile(ex)
    _prof_expr(ex, :(sample_rate=1.0))
end

function _prof_expr(expr, opts)
    quote
        $start(; $(esc(opts)))
        local res = $(esc(expr))
        $stop()
        res
    end
end

function start(; sample_rate::Float64)
    ccall(:jl_start_alloc_profile, Cvoid, (Cdouble,), sample_rate)
end

function stop()
    ccall(:jl_stop_alloc_profile, RawAllocResults, ())
end

function clear()
    ccall(:jl_free_alloc_profile, Cvoid, ())
end

function fetch()
    raw_results = ccall(:jl_fetch_alloc_profile, RawAllocResults, ())
    decoded_results = decode(raw_results)
    return decoded_results
end

# decoded results

struct Alloc
    type::Any
    stacktrace::StackTrace
    size::Int
end

struct AllocResults
    allocs::Vector{Alloc}
    frees::Dict{Type,UInt}
end

function Base.show(io::IO, ::AllocResults)
    print(io, "AllocResults")
end

const BacktraceEntry = Union{Ptr{Cvoid}, InterpreterIP}
const BacktraceCache = Dict{BacktraceEntry,Vector{StackFrame}}

# copied from julia_internal.h
const JL_BUFF_TAG = UInt(0x4eadc000)

struct CorruptType end
struct BufferType end

function load_type(ptr::Ptr{Type})
    if UInt(ptr) < UInt(4096)
        return CorruptType
    elseif UInt(ptr) == JL_BUFF_TAG
        return BufferType
    end
    return unsafe_pointer_to_objref(ptr)
end

function decode_alloc(cache::BacktraceCache, raw_alloc::RawAlloc)::Alloc
    Alloc(
        load_type(raw_alloc.type),
        stacktrace_memoized(cache, load_backtrace(raw_alloc.backtrace)),
        UInt(raw_alloc.size)
    )
end

function decode(raw_results::RawAllocResults)::AllocResults
    cache = BacktraceCache()
    allocs = [
        decode_alloc(cache, unsafe_load(raw_results.allocs, i))
        for i in 1:raw_results.num_allocs
    ]

    frees = Dict{Type,UInt}()
    for i in 1:raw_results.num_frees
        free = unsafe_load(raw_results.frees, i)
        type = load_type(free.type)
        frees[type] = free.count
    end

    return AllocResults(
        allocs,
        frees
    )
end

function load_backtrace(trace::RawBacktrace)::Vector{Ptr{Cvoid}}
    out = Vector{Ptr{Cvoid}}()
    for i in 1:trace.size
        push!(out, unsafe_load(trace.data, i))
    end

    return out
end

function stacktrace_memoized(
    cache::BacktraceCache,
    trace::Vector{Ptr{Cvoid}},
    c_funcs::Bool=true
)::StackTrace
    stack = StackTrace()
    for ip in trace
        frames = get(cache, ip) do
            res = lookup(ip)
            cache[ip] = res
            return res
        end
        for frame in frames
            # Skip frames that come from C calls.
            if c_funcs || !frame.from_c
                push!(stack, frame)
            end
        end
    end
    return stack
end

# Precompile once for the package cache,
precompile(start, ())
precompile(stop, ())

function __init__()
    # And once when loading the package, to get the full machine code precompiled.
    # TOOD: Although actually, we probably don't need this since this package will be
    # precompiled into the sysimg, so the top-level statements will be enough to get the
    # machine code codegen precompiled as well. :)
    # We can delete this function once we make this package a stdlib.
    precompile(start, ())
    precompile(stop, ())
end

end
