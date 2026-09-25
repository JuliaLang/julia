# This file is a part of Julia. License is MIT: https://julialang.org/license

# The hooks of Base into the GC regions (src/gc-regions.h). A runtime built
# with WITH_GC_REGIONS defines Core.GC_REGIONS; without it each hook below
# is the plain call.
const GC_REGIONS = Core.isdefinedglobal(Core, :GC_REGIONS)
# False in a build with WITH_GC_REGION_BARRIER=0: the regions without their
# escape barrier, for a program that was validated with the barrier on.
const GC_REGION_BARRIER = Core.isdefinedglobal(Core, :GC_REGION_BARRIER)

if GC_REGIONS
    end
    # The window stays open and region 0 is installed until the resume: for
    # state that a region-0 object links, such as a task or a global.
    _region_window_suspend() = ccall(:jl_gc_region_suspend, Cint, ())
    _region_window_resume(parked::Cint) = ccall(:jl_gc_region_resume, Cvoid, (Cint,), parked)
    function with_region_window_suspended(f::F) where {F}
        parked = _region_window_suspend()
        try
            return f()
        finally
            _region_window_resume(parked)
        end
    end
else
    _region_window_suspend() = Cint(0)
    _region_window_resume(::Cint) = nothing
    with_region_window_suspended(f::F) where {F} = f()
end

