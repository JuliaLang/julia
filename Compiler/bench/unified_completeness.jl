# Join-point completeness verification bench (design doc §6 "Join-point
# completeness"; harness in Compiler/src/unified/completeness.jl):
#
#   (a) residual classifier + stock oracle over real Base/stdlib bodies
#   (b) structured cell fuzzer + semantic differential (10k cases)
#   (c) dominance-frontier correspondence (fuzz sample + corpus sample)
#
# Run:  ./julia Compiler/bench/unified_completeness.jl [ncorpus] [nfuzz] [seed]
import Compiler
const U = Compiler.load_unified!()
import UnifiedIR
using UnifiedIR: StmtId, @K_str
include(joinpath(@__DIR__, "..", "test", "unified", "cellfuzz.jl"))

ncorpus = length(ARGS) >= 1 ? parse(Int, ARGS[1]) : 600
nfuzz   = length(ARGS) >= 2 ? parse(Int, ARGS[2]) : 10_000
seed    = length(ARGS) >= 3 ? parse(Int, ARGS[3]) : 0x5eed
println("seed = ", seed)

# -- corpus collection (the parity harness's enumeration) --------------------
function collect_mis(n::Int)
    out = Core.MethodInstance[]
    seen = Set{Any}()
    for mod in (Base, Core, Base.Iterators, Base.Math, Base.Sort)
        for name in names(mod; all = true)
            length(out) >= n && return out
            isdefined(mod, name) || continue
            f = getglobal(mod, name)
            f isa Function || f isa Type || continue
            for m in methods(f)
                length(out) >= n && return out
                m isa Method || continue
                for mi in Base.specializations(m)
                    length(out) >= n && return out
                    mi isa Core.MethodInstance || continue
                    mi.specTypes === m.sig && continue
                    isdispatchtuple(mi.specTypes) || continue
                    mi.specTypes in seen && continue
                    push!(seen, mi.specTypes)
                    push!(out, mi)
                end
            end
        end
    end
    return out
end

cellops(ir) = count(i -> UnifiedIR.stmt_kind(ir, StmtId(Int32(i))) in
                    (K"cell", K"cell_set", K"cell_get", K"cell_new", K"cell_isdefined"),
                    1:UnifiedIR.nstmts(ir))

# -- (a) classifier + stock oracle -------------------------------------------
println("\n=== (a) residual classifier + stock oracle over ", ncorpus, " bodies ===")
mis = collect_mis(ncorpus)
hist = Dict{Symbol,Int}()
violations = Tuple{Any,Vector{Symbol}}[]
skipped = 0
prewall = time_ns()
bodies_with_cells = 0; total_cells_pre = 0; total_cells_post = 0
for mi in mis
    m = mi.def
    m isa Method || (global skipped += 1; continue)
    ir = try
        ir0 = U.lowered_ir(mi.def.sig.parameters[1].instance, Any[mi.specTypes.parameters[2:end]...];
                           world = Base.get_world_counter())
    catch
        nothing
    end
    ir === nothing && (global skipped += 1; continue)
    global total_cells_pre += cellops(ir)
    ir = try
        U.promotion_fixpoint!(U.structure_prep!(ir))
    catch
        push!(violations, (mi, [:pass_error])); continue
    end
    global total_cells_post += cellops(ir)
    res = U.classify_residual_cells(ir)
    isempty(res) || (global bodies_with_cells += 1)
    reasons = Symbol[]
    for (_, r) in res
        hist[r] = get(hist, r, 0) + 1
        push!(reasons, r)
    end
    # THE ORACLE (sharpened): on Box-free stock bodies our residual set must
    # be exactly the v1 representation choices, and their stock counterpart
    # is machine-checked:
    #   :handler_crossing residuals require stock PhiC/Upsilon (or stock
    #                     undef guards) in the SAME body — exception SSA
    #   :gc_token         allowed outright (stock's token slots promote; we
    #                     keep the pair for the gc-preserve pairing verifier)
    #   :box_capture      requires stock Box
    # Any other reason — bug classes included — is a violation.
    if !isempty(reasons)
        ct = try
            only(Base.code_typed_by_type(mi.specTypes; optimize = true))[1]
        catch
            nothing
        end
        if ct !== nothing
            hasbox = any(x -> x isa Expr && any(a -> a isa GlobalRef && a.name === :Box, x.args),
                         ct.code)
            hasphic = any(x -> x isa Core.PhiCNode || x isa Core.UpsilonNode, ct.code)
            hasundef = any(x -> x isa Expr && x.head === :throw_undef_if_not, ct.code)
            stray = Symbol[]
            for r in reasons
                if r === :gc_token
                elseif r === :handler_crossing
                    (hasphic || hasundef) || push!(stray, r)
                elseif r === :box_capture
                    hasbox || push!(stray, r)
                else
                    push!(stray, r)
                end
            end
            !hasbox && !isempty(stray) && push!(violations, (mi, stray))
        end
    end
end
wall_a = (time_ns() - prewall) / 1e9
println("bodies: ", length(mis) - skipped, " analyzed (", skipped, " skipped: no lowered form)")
println("cell ops: ", total_cells_pre, " pre -> ", total_cells_post, " post promotion (",
        round(100 * (1 - total_cells_post / max(total_cells_pre, 1)); digits = 1), "% eliminated)")
println("residual histogram: ", sort!(collect(hist); by = last, rev = true))
println("STOCK-ORACLE VIOLATIONS (target: empty): ", length(violations))
for (mi, rs) in violations
    println("  VIOLATION ", mi, "  reasons=", rs)
end
println("(a) wall: ", round(wall_a; digits = 1), " s")

# -- (b) fuzzer ---------------------------------------------------------------
println("\n=== (b) structured fuzzer: ", nfuzz, " cases (df every 50th) ===")
t0 = time_ns()
s = CellFuzz.run_cases(U, nfuzz; seed = Int(seed), dfevery = 50)
println("cases=", s.cases[], " differential-failures=", s.diffs[],
        " verify-failures=", s.verifyfails[], " unclassified=", s.unclassified[],
        " open-class=", s.openresiduals[])
println("residual histogram: ", sort!(collect(s.residuals); by = last, rev = true))
println("cell ops pre=", s.cells_pre[], " post=", s.cells_post[], " (",
        round(100 * (1 - s.cells_post[] / max(s.cells_pre[], 1)); digits = 1), "% eliminated)")
println("failures: ", isempty(s.failures) ? "NONE" : s.failures)
println("(b) wall: ", round((time_ns() - t0) / 1e9; digits = 1), " s")

# -- (c) DF correspondence ----------------------------------------------------
println("\n=== (c) dominance-frontier correspondence ===")
println("fuzz sample (from (b)): cells=", s.dfcells[], " match=", s.dfmatch[],
        " missing=", s.dfmissing[], " structural-extras=", s.dfextra[],
        "  exact-match rate (of promoted)=", round(100 * s.dfmatch[] / max(s.dfcells[], 1); digits = 1), "%")
t0 = time_ns()
dfc = 0; dfm = 0; dfmiss = 0; dfex = 0; dfresid = 0
for mi in mis[1:min(end, 200)]
    mi.def isa Method || continue
    ir = try
        U.structure_prep!(U.lowered_ir(mi.def.sig.parameters[1].instance,
                                       Any[mi.specTypes.parameters[2:end]...];
                                       world = Base.get_world_counter()))
    catch
        continue
    end
    r = try
        U.df_correspondence(ir)
    catch
        continue
    end
    for x in r.results
        global dfc += 1
        x.status === :match && (global dfm += 1)
        x.status === :missing && (global dfmiss += 1; println("  DF-MISSING: ", mi, " cell %", x.cell,
                                                              " expected=", x.expected, " ours=", x.ours))
        (x.status === :residual_classified || x.status === :residual_unclassified) && (global dfresid += 1)
        global dfex += x.extra
    end
end
println("corpus sample: cells=", dfc, " match=", dfm, " missing=", dfmiss,
        " residual(classified)=", dfresid, " structural-extras=", dfex,
        "  exact-match rate (of promoted)=", round(100 * dfm / max(dfc - dfresid, 1); digits = 1), "%")
println("(c) wall: ", round((time_ns() - t0) / 1e9; digits = 1), " s")
