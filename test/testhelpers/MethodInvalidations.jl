# This file is a part of Julia. License is MIT: https://julialang.org/license

# This test file can be used to test for method invalidations caused by the insertion of new
# methods. The file is partially based on code from SnoopCompileCore.jl and SnoopCompile.jl,
# see those packages for more information.

module MethodInvalidations

export @method_invalidations, uinvalidated, invalidated

macro method_invalidations(expr)
    quote
        local list = ccall(:jl_debug_method_invalidation, Any, (Cint,), 1)
        Expr(:tryfinally,
            $(esc(expr)),
            ccall(:jl_debug_method_invalidation, Any, (Cint,), 0)
        )
        list
    end
end

function _from_corecompiler(mi::Core.MethodInstance)
    fn = fullname(mi.def.module)
    length(fn) < 2 && return false
    fn[1] === :Core || return false
    return fn[2] === :Compiler
end

function uinvalidated(invlist; exclude_corecompiler::Bool=true)
    umis = Set{Core.MethodInstance}()
    for (i, item) in enumerate(invlist)
        if isa(item, Core.MethodInstance)
            if invlist[i+1] != "invalidate_mt_cache"
                if !exclude_corecompiler || !_from_corecompiler(item)
                    push!(umis, item)
                end
            end
        end
    end
    return umis
end

"""
    invalidated(invs, f)

Return whether one or more methods for `f` were invalidated according to the list of
invalidations `invs`.
"""
function invalidated(invs, f)
    unique_invs = uinvalidated(invs)
    unique_methods = getproperty.(unique_invs, :def)
    for m in methods(f)
        if m in unique_methods
            return true
        end
    end
    return false
end

end # module
