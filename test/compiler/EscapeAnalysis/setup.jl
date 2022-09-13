include(normpath(@__DIR__, "..", "irutils.jl"))
include(normpath(@__DIR__, "EAUtils.jl"))
using Test, Core.Compiler.EscapeAnalysis, .EAUtils
import Core: Argument, SSAValue, ReturnNode
const EA = Core.Compiler.EscapeAnalysis
import .EA: ignore_argescape

isT(T) = (@nospecialize x) -> x === T
isreturn(@nospecialize x) = isa(x, Core.ReturnNode) && isdefined(x, :val)
isthrow(@nospecialize x) = Meta.isexpr(x, :call) && Core.Compiler.is_throw_call(x)
isϕ(@nospecialize x) = isa(x, Core.PhiNode)
function with_normalized_name(@nospecialize(f), @nospecialize(x))
    if Meta.isexpr(x, :foreigncall)
        name = x.args[1]
        nn = EA.normalize(name)
        return isa(nn, Symbol) && f(nn)
    end
    return false
end
isarrayalloc(@nospecialize x) = with_normalized_name(nn->!isnothing(Core.Compiler.alloc_array_ndims(nn)), x)
isarrayresize(@nospecialize x) = with_normalized_name(nn->!isnothing(EA.array_resize_info(nn)), x)
isarraycopy(@nospecialize x) = with_normalized_name(nn->EA.is_array_copy(nn), x)

"""
    is_load_forwardable(x::EscapeInfo) -> Bool

Queries if `x` is elibigle for store-to-load forwarding optimization.
"""
function is_load_forwardable(x::EA.EscapeInfo)
    AliasInfo = x.AliasInfo
    # NOTE technically we also need to check `!has_thrown_escape(x)` here as well,
    # but we can also do equivalent check during forwarding
    return isa(AliasInfo, EA.IndexableFields) || isa(AliasInfo, EA.IndexableElements)
end

let setup_ex = quote
        mutable struct SafeRef{T}
            x::T
        end
        Base.getindex(s::SafeRef) = getfield(s, 1)
        Base.setindex!(s::SafeRef, x) = setfield!(s, 1, x)

        mutable struct SafeRefs{S,T}
            x1::S
            x2::T
        end
        Base.getindex(s::SafeRefs, idx::Int) = getfield(s, idx)
        Base.setindex!(s::SafeRefs, x, idx::Int) = setfield!(s, idx, x)

        global GV::Any
        const global GR = Ref{Any}()
    end
    global function EATModule(setup_ex = setup_ex)
        M = Module()
        Core.eval(M, setup_ex)
        return M
    end
    Core.eval(@__MODULE__, setup_ex)
end
