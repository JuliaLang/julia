# Fresh-process acceptance table (§6 join-point completeness).
#
# Every number here is computed inside THIS process, which is expected to be
# a cold `julia --startup-file=no` invocation:
#
#   ./julia --startup-file=no Compiler/bench/unified_fresh.jl
#
# The corpus/oracle/fuzzer/DF legs live in unified_completeness.jl (equally
# cold-runnable); this script covers the typed_ir surface, headed by the
# escape_string family that motivated sealed-exit threading.
import Compiler
const U = Compiler.load_unified!()
import UnifiedIR
using UnifiedIR: StmtId, @K_str

cellops(ir) = count(i -> UnifiedIR.stmt_kind(ir, StmtId(Int32(i))) in
                    (K"cell", K"cell_set", K"cell_get", K"cell_new", K"cell_isdefined"),
                    1:UnifiedIR.nstmts(ir))

function measure(nm, f, ats)
    ir = U.typed_ir(f, ats)
    UnifiedIR.verify_ir(ir; level = 1)
    res = U.classify_residual_cells(ir)
    bad = [r for (_, r) in res if !(r in U.RESIDUAL_OK)]
    println(rpad(nm, 34), " stmts=", lpad(UnifiedIR.nstmts(ir), 4),
            " cellops=", lpad(cellops(ir), 3),
            " residuals=", isempty(res) ? "-" : join(("$(r)" for (_, r) in res), ","),
            isempty(bad) ? "" : "  <-- BUG CLASSES PRESENT")
    return (UnifiedIR.nstmts(ir), cellops(ir), length(res), length(bad))
end

println("== escape_string (kwbody), all cached specializations ==")
kwbody = Base.bodyfunction(which(escape_string, Tuple{IO, AbstractString, Any}))
specs = Any[
    Any[Any, Bool, Bool, typeof(escape_string), IOBuffer, String, Any],
    Any[Tuple{}, Bool, Bool, typeof(escape_string), IOBuffer, String, Tuple{Char, Char}],
    Any[Tuple{}, Bool, Bool, typeof(escape_string), IOContext{IOBuffer}, String, String],
    Any[Tuple{}, Bool, Bool, typeof(escape_string), IOBuffer, SubString{String}, Tuple{Char, Char}],
    Any[Tuple{}, Bool, Bool, typeof(escape_string), IOContext{IOBuffer}, String, Tuple{Char, Char}],
    Any[Tuple{}, Bool, Bool, typeof(escape_string), IOBuffer, String, String],
    Any[Tuple{}, Bool, Bool, typeof(escape_string), IOBuffer, String, Tuple{Char}],
    Any[Tuple{}, Bool, Bool, typeof(escape_string), IOContext{IOBuffer}, SubString{String}, Tuple{Char, Char}],
    Any[Tuple{}, Bool, Bool, typeof(escape_string), IOContext{IOStream}, String, Tuple{Char, Char}],
    Any[Tuple{}, Bool, Bool, typeof(escape_string), IOContext{IOStream}, String, String],
]
totbad = 0
firstshape = nothing
for (i, ats) in enumerate(specs)
    r = measure("escape_string #$i", kwbody, ats)
    global totbad += r[4]
    i == 1 && (global firstshape = r)
end
# in-process determinism: the same query twice must agree exactly
r2 = U.typed_ir(kwbody, specs[1])
shape2 = (UnifiedIR.nstmts(r2), cellops(r2), length(U.classify_residual_cells(r2)), 0)
println("determinism (same query twice): ",
        firstshape[1:3] == shape2[1:3] ? "IDENTICAL" : "DIFFERS $(firstshape) vs $(shape2)")

println("\n== standard bodies ==")
totbad += measure("gcd", Base.gcd, Any[Int, Int])[4]
totbad += measure("_gcd", Base._gcd, Any[Int, Int])[4]
totbad += measure("countlines kw", Base.var"#countlines#389", Any[Char, typeof(countlines), IOStream])[4]
totbad += measure("code_lowered kw", Base.var"#code_lowered#201",
                  Any[Bool, Symbol, typeof(code_lowered), Core.TypeEgal{Tuple{Base.UnifiedCompiler.var"#134#135", Int64}}])[4]

println("\nBUG-CLASS RESIDUALS TOTAL: ", totbad, totbad == 0 ? "  (acceptance: PASS)" : "  (acceptance: FAIL)")
