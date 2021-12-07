module AllocProfile

using Base.StackTraces
using GC: RawAllocResults, RawAlloc, RawBacktrace

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
