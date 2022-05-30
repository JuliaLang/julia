# This file is a part of Julia. License is MIT: https://julialang.org/license

if false
    import Base: Base, @show
else
    macro show(s)
        return :(println(stdout, $(QuoteNode(s)), " = ", $(esc(s))))
    end
end

include("compiler/ssair/slot2ssa.jl")
include("compiler/ssair/inlining.jl")
include("compiler/ssair/verify.jl")
include("compiler/ssair/legacy.jl")
include("compiler/ssair/EscapeAnalysis/EscapeAnalysis.jl")
include("compiler/ssair/passes.jl")
