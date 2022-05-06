# This file is a part of Julia. License is MIT: https://julialang.org/license

if false
    import Base: Base, @show
else
    macro show(s)
        return :(println(stdout, $(QuoteNode(s)), " = ", $(esc(s))))
    end
end

function argextype end # imported by EscapeAnalysis
function stmt_effect_free end # imported by EscapeAnalysis
function alloc_array_ndims end # imported by EscapeAnalysis
function try_compute_field end # imported by EscapeAnalysis

include("compiler/ssair/basicblock.jl")
include("compiler/ssair/domtree.jl")
include("compiler/ssair/ir.jl")
include("compiler/ssair/slot2ssa.jl")
include("compiler/ssair/inlining.jl")
include("compiler/ssair/verify.jl")
include("compiler/ssair/legacy.jl")
include("compiler/ssair/EscapeAnalysis/EscapeAnalysis.jl")
include("compiler/ssair/passes.jl")
