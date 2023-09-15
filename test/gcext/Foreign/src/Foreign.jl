# This file is a part of Julia. License is MIT: https://julialang.org/license

module Foreign

using Libdl

const foreignlib = joinpath(ENV["BINDIR"], "foreignlib.$(dlext)")

const FObj = ccall((:declare_foreign, foreignlib), Any, (Any, Any, Any), :FObj, @__MODULE__, Any)
FObj() = ccall((:allocate_foreign, foreignlib), Any, (Ptr{Cvoid}, Csize_t, Any,), Core.getptls(), sizeof(Ptr{Cvoid}), FObj)::FObj

export FObj

get_nmark()  = ccall((:nmark_counter, foreignlib),  Cint, ())
get_nsweep() = ccall((:nsweep_counter, foreignlib), Cint, ())

function __init__()
    @assert ccall((:reinit_foreign, foreignlib), Cint, (Any,), FObj) == 1
end

allocs(N) = [Foreign.FObj() for _ in 1:N]

function test(N)
    x = allocs(N)
    Core.donotdelete(x)
    x = nothing
end

end # module Foreign
