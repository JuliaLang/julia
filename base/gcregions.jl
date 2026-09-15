# This file is a part of Julia. License is MIT: https://julialang.org/license

# The hooks of Base into the GC regions (src/gc-regions.h). A runtime built
# with WITH_GC_REGIONS defines Core.GC_REGIONS; without it each hook below
# is the plain call.
const GC_REGIONS = Core.isdefinedglobal(Core, :GC_REGIONS)

if GC_REGIONS
    # A buffer that replaces another takes the lifetime of the one it
    # replaces, so it is allocated in the GC region of its container, not in
    # the region of an open window; otherwise an ordinary `push!` or rehash
    # would be an escape. The borrow installs the region of `like` for `f`;
    # it is not a window, and a task must not yield inside one.
    function with_region_of(f::F, like, args...) where {F}
        lent = ccall(:jl_gc_region_borrow, Cint, (Cint,), ccall(:jl_gc_region_of, Cint, (Any,), like))
        try
            return f(args...)
        finally
            ccall(:jl_gc_region_unborrow, Cvoid, (Cint,), lent)
        end
    end
    # The window stays open and region 0 is installed until the resume: for
    # state that a region-0 object links, such as a task or a global.
    _region_window_suspend() = ccall(:jl_gc_region_suspend, Cint, ())
    _region_window_resume(parked::Cint) = ccall(:jl_gc_region_resume, Cvoid, (Cint,), parked)
    function with_region_window_suspended(f::F, args...) where {F}
        parked = _region_window_suspend()
        try
            return f(args...)
        finally
            _region_window_resume(parked)
        end
    end
else
    with_region_of(f::F, like, args...) where {F} = f(args...)
    _region_window_suspend() = Cint(0)
    _region_window_resume(::Cint) = nothing
    with_region_window_suspended(f::F, args...) where {F} = f(args...)
end

# A `Memory` of type `M` and length `n`, allocated where `like` lives.
memory_for(like, ::Type{M}, n::Int) where {M<:GenericMemory} = with_region_of(M, like, undef, n)
