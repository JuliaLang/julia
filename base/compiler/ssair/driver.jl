# This file is a part of Julia. License is MIT: https://julialang.org/license

using Core: LineInfoNode

if false
    import Base: Base, @show
else
    macro show(s)
        return :(println(stdout, $(QuoteNode(s)), " = ", $(esc(s))))
    end
end

include("compiler/ssair/basicblock.jl")
include("compiler/ssair/domtree.jl")
include("compiler/ssair/ir.jl")
include("compiler/ssair/slot2ssa.jl")
include("compiler/ssair/inlining.jl")
include("compiler/ssair/verify.jl")
include("compiler/ssair/legacy.jl")
function try_compute_field end # imported by EscapeAnalysis
include("compiler/ssair/EscapeAnalysis/EscapeAnalysis.jl")
include("compiler/ssair/passes.jl")
# @isdefined(Base) && include("compiler/ssair/show.jl")
