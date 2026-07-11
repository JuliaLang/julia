# The one-stack demo (unifiedir-design.md §3.7 Level 2): one substrate, one
# porcelain, one kind registry, one provenance walk, one GC — from source
# text to optimized IR with surface-highlighted diagnostics.
#
#   JuliaSyntax parse → JuliaLowering front half →
#   JuliaLowering.UnifiedBackend.lower_to_ir (records :source CURSORS per
#   statement) → Compiler.Unified inference + UnifiedIR optimization
#   (promote_cells!/dce!/compact!) → pick an optimized statement → ONE
#   generic provenance walk (IR stmt → syntax node → :source chain →
#   SourceRef) → JuliaSyntax.highlight() of the exact surface text →
#   collect_syntax! (AST-lifetime GC) → the same highlight, byte-identical.
#
# Run with the built binary:   ./julia UnifiedIR/demo/provenance_demo.jl
# (the top-level packages live in the julia share directory, which is a
# directory environment but not on the default load path)
pushfirst!(LOAD_PATH, joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia"))
using UnifiedIR, JuliaSyntax, JuliaLowering
import Compiler
const UnifiedLowering = JuliaLowering.UnifiedBackend
const UnifiedCompiler = Compiler.load_unified!()


src = """
function process(v, n)
    total = 0
    for i in 1:n
        total = total + v[i] * i
    end
    return total
end
"""

ms = UnifiedLowering.lower_to_ir(Main, src; filename = "demo.jl")
ir = ms[1].ir
println("lowered `", ms[1].name, "`: ", UnifiedIR.nstmts(ir), " statements")

# every emitted statement carries :source
col = UnifiedIR.getattr(ir, :source)
@assert all(haskey(col, i) for i in 1:UnifiedIR.nstmts(ir))
println("every statement carries a :source cursor: OK")

# inference + optimization on the SAME rows
UnifiedCompiler.infer_ir!(ir, Any[typeof(sum), Vector{Int}, Int])  # (#self#, v, n)
UnifiedIR.promote_cells!(ir)
UnifiedIR.dce!(ir)
ir, rs = UnifiedIR.compact!(ir)
println("after infer + promote_cells! + dce! + compact!: ", UnifiedIR.nstmts(ir), " statements")
col = UnifiedIR.getattr(ir, :source)
live = [i for i in 1:UnifiedIR.nstmts(ir)]
have = count(i -> haskey(col, i), live)
println("statements with provenance after optimization: ", have, "/", length(live))

# pick the surviving multiplication and highlight its surface text
function find_stmt(ir, pat)
    for i in 1:UnifiedIR.nstmts(ir)
        s = UnifiedIR.StmtId(Int32(i))
        UnifiedIR.stmt_kind(ir, s) === UnifiedIR.K"call" || continue
        st = UnifiedIR.Tree(ir, i)
        t = UnifiedIR.provenance_terminal(st)   # the ONE seam-crossing walk
        t isa JuliaSyntax.SourceRef || continue
        txt = JuliaSyntax.sourcetext(JuliaSyntax.prov_end(UnifiedIR.prov(st)))
        txt == pat && return i
    end
    return 0
end
target = find_stmt(ir, "v[i] * i")
target == 0 && (target = find_stmt(ir, "total + v[i] * i"))
@assert target != 0
expected = JuliaSyntax.sourcetext(JuliaSyntax.prov_end(UnifiedIR.prov(UnifiedIR.Tree(ir, target))))
@assert occursin("v[i] * i", expected)   # highlighted text matches the source snippet
st = UnifiedIR.Tree(ir, target)
term = UnifiedIR.provenance_terminal(st)::JuliaSyntax.SourceRef
excerpt = String(JuliaSyntax.sourcetext(JuliaSyntax.prov_end(UnifiedIR.prov(st))))
println("\noptimized statement %", target, " (",
        UnifiedIR.kindname(UnifiedIR.stmt_kind(ir, UnifiedIR.StmtId(Int32(target)))),
        ") came from surface text: ", repr(excerpt))
println("highlight():")
JuliaSyntax.highlight(stdout, term; note = "this IR statement came from here")
println()

# bonus: print_ir with per-statement source excerpts through the annotation hook
println("\n--- print_ir with source excerpts ---")
ann = (pir, s) -> begin
    c = UnifiedIR.getattr(pir, :source)
    haskey(c, Int(s.id)) || return nothing
    st = UnifiedIR.Tree(pir, Int(s.id))
    t = UnifiedIR.provenance_terminal(st)
    t isa JuliaSyntax.SourceRef || return nothing
    txt = String(JuliaSyntax.sourcetext(JuliaSyntax.prov_end(UnifiedIR.prov(st))))
    txt = replace(txt, r"\s+" => " ")            # one-line excerpt
    length(txt) > 44 ? first(txt, 44) * "…" : txt
end
UnifiedIR.print_ir(IOContext(stdout, :stmt_annotate => ann), ir)

# ---------------------------------------------------------------------------
# AST-lifetime GC (§3.7 Level 2 step 3): collect the lowering graph against
# the live IR's provenance — the CodeInstance-finalization policy prototype.
# One registry, one porcelain, one printer, one GC.
# ---------------------------------------------------------------------------
println("\n--- collect_syntax! (AST-lifetime GC) ---")
gsyn = UnifiedIR.syntax_graph(UnifiedIR.getattr(ir, :source)[1])
sub = UnifiedIR.substrate(gsyn)
hl_before = sprint(io -> JuliaSyntax.highlight(io, UnifiedIR.provenance_terminal(UnifiedIR.Tree(ir, target))))
n0 = UnifiedIR.nnodes(sub); b0 = Base.summarysize(gsyn)
remap = UnifiedIR.collect_syntax!(gsyn, (ir,))          # policy = :conservative
n1 = UnifiedIR.nnodes(sub); b1 = Base.summarysize(gsyn)
println("conservative: ", n0, " -> ", n1, " nodes (",
        round(100 * (n0 - n1) / n0, digits = 1), "% reclaimed), ",
        b0, " -> ", b1, " bytes (", round(100 * (b0 - b1) / b0, digits = 1), "%)")
hl_after = sprint(io -> JuliaSyntax.highlight(io, UnifiedIR.provenance_terminal(UnifiedIR.Tree(ir, target))))
@assert hl_before == hl_after                            # provenance survived collection
println("highlight() after collection is byte-identical: OK")

# the :prune policy on a fresh pipeline run
ms2 = UnifiedLowering.lower_to_ir(Main, src; filename = "demo.jl")
ir2p = ms2[1].ir
UnifiedCompiler.infer_ir!(ir2p, Any[typeof(sum), Vector{Int}, Int])
UnifiedIR.promote_cells!(ir2p); UnifiedIR.dce!(ir2p)
ir2p, _ = UnifiedIR.compact!(ir2p)
g2 = UnifiedIR.syntax_graph(UnifiedIR.getattr(ir2p, :source)[1])
sub2 = UnifiedIR.substrate(g2)
n0p = UnifiedIR.nnodes(sub2); b0p = Base.summarysize(g2)
UnifiedIR.collect_syntax!(g2, (ir2p,); policy = :prune)
n1p = UnifiedIR.nnodes(sub2); b1p = Base.summarysize(g2)
println("prune:        ", n0p, " -> ", n1p, " nodes (",
        round(100 * (n0p - n1p) / n0p, digits = 1), "% reclaimed), ",
        b0p, " -> ", b1p, " bytes (", round(100 * (b0p - b1p) / b0p, digits = 1), "%)")
t2 = UnifiedIR.provenance_terminal(UnifiedIR.Tree(ir2p, UnifiedIR.nstmts(ir2p) - 1))
println("post-prune walk still terminates at: ", typeof(t2))

println("\nre-highlight after conservative collection (provenance survived the GC):")
JuliaSyntax.highlight(stdout, UnifiedIR.provenance_terminal(UnifiedIR.Tree(ir, target));
                      note = "still exactly here, after AST GC")
println()
println("\nONE STACK: one registry, one porcelain, one printer, one GC — done.")
