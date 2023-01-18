# This file is a part of Julia. License is MIT: https://julialang.org/license

if false
    import Base: Base, @show
else
    macro show(ex...)
        blk = Expr(:block)
        for s in ex
            push!(blk.args, :(println(stdout, $(QuoteNode(s)), " = ",
                                              begin local value = $(esc(s)) end)))
        end
        isempty(ex) || push!(blk.args, :value)
        blk
    end
end

include("compiler/ssair/heap.jl")
include("compiler/ssair/slot2ssa.jl")
include("compiler/ssair/inlining.jl")
include("compiler/ssair/verify.jl")
include("compiler/ssair/legacy.jl")
include("compiler/ssair/EscapeAnalysis/EscapeAnalysis.jl")
include("compiler/ssair/passes.jl")
include("compiler/ssair/irinterp.jl")
