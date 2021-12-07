module AllocProfile

using Base.StackTraces
using GC: RawAllocResults, RawAlloc, RawBacktrace

# raw results

# matches RawBacktrace on the C side
struct RawBacktrace
    data::Ptr{Nothing} # jl_bt_element_t
    size::Csize_t
end

# matches RawAlloc on the C side
struct RawAlloc
    type_address::Csize_t
    backtrace::RawBacktrace
    size::Csize_t
end

# matches RawAllocResults on the C side
struct RawAllocResults
    num_allocs::Csize_t
    allocs::Ptr{RawAlloc}
end

function start_alloc_profile(skip_every::Int=0)
    ccall(:jl_start_alloc_profile, Cvoid, (Cint,), skip_every)
end

function stop_alloc_profile()
    raw_results = ccall(:jl_stop_alloc_profile, RawAllocResults, ())
    decoded_results = decode_alloc_results(raw_results)
    ccall(:jl_free_alloc_profile, Cvoid, ())
    return decoded_results
end

# decoded results

struct Alloc
    type::Type
    stacktrace::Vector{StackTraces.StackFrame}
    size::Int
end

function decode_alloc_results(raw_results::RawAllocResults)::Vector{Alloc}
    out = Vector{Alloc}()
    for i in 1:raw_results.num_allocs
        raw_alloc = reinterpret(RawAlloc, unsafe_load(raw_results.allocs, i))
        push!(out, Alloc(
            Int,
            Vector{StackTraces.StackFrame}(),
            5
        ))
    end
    return out
end

end
