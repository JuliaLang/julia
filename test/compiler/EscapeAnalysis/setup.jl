const use_core_compiler = true

if use_core_compiler
    const EscapeAnalysis = Core.Compiler.EscapeAnalysis
else
    include(normpath(Sys.BINDIR, "..", "..", "base", "compiler", "ssair", "EscapeAnalysis", "EscapeAnalysis.jl"))
end

include("EAUtils.jl")
include("../irutils.jl")

using Test, .EscapeAnalysis, .EAUtils
using .EscapeAnalysis: ignore_argescape

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
