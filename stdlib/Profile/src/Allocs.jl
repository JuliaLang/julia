module Allocs

using Base.StackTraces: StackTrace, StackFrame, lookup
using Base: InterpreterIP

# --- Raw results structs, originally defined in C ---

# The C jl_bt_element_t object contains either an IP pointer (size_t) or a void*.
const BTElement = Csize_t;

# matches RawBacktrace on the C side
struct RawBacktrace
    data::Ptr{BTElement} # in C: *jl_bt_element_t
    size::Csize_t
end

# matches RawAlloc on the C side
struct RawAlloc
    type::Ptr{Type}
    backtrace::RawBacktrace
    size::Csize_t
end

# matches RawAllocResults on the C side
struct RawAllocResults
    allocs::Ptr{RawAlloc}
    num_allocs::Csize_t
end

"""
    Profile.Allocs.@profile [sample_rate=0.0001] expr

Profile allocations that happen during `expr`, returning
both the result and and AllocResults struct.

```julia
julia> Profile.Allocs.@profile sample_rate=0.01 peakflops()
1.03733270279065e11

julia> results = Profile.Allocs.fetch();

julia> last(sort(results.allocs, by=x->x.size))
Profile.Allocs.Alloc(Vector{Any}, Base.StackTraces.StackFrame[_new_array_ at array.c:127, ...], 5576)
```
"""
macro profile(opts, ex)
    _prof_expr(ex, opts)
end
macro profile(ex)
    _prof_expr(ex, :(sample_rate=0.0001))
end

function _prof_expr(expr, opts)
    quote
        $start(; $(esc(opts)))
        local res = $(esc(expr))
        $stop()
        res
    end
end

function start(; sample_rate::Number)
    ccall(:jl_start_alloc_profile, Cvoid, (Cdouble,), Float64(sample_rate))
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
end

# Without this, the Alloc's stacktrace prints for lines and lines and lines..
function Base.show(io::IO, a::Alloc)
    stacktrace_sample = length(a.stacktrace) >= 1 ? "$(a.stacktrace[1]), ..." : ""
    print(io, "$Alloc($(a.type), $StackFrame[$stacktrace_sample], $(a.size))")
end

const BacktraceCache = Dict{BTElement,Vector{StackFrame}}

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
    return AllocResults(allocs)
end

function load_backtrace(trace::RawBacktrace)::Vector{BTElement}
    out = Vector{BTElement}()
    for i in 1:trace.size
        push!(out, unsafe_load(trace.data, i))
    end

    return out
end

function stacktrace_memoized(
    cache::BacktraceCache,
    trace::Vector{BTElement},
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
@assert precompile(start, ())
@assert precompile(stop, ())
@assert precompile(fetch, ())

end
