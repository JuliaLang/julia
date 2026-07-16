# The closure-capture zoo (julia#15276): precise boxed-vs-value capture
# decisions from the shared UnifiedIR mem2reg machinery, demonstrated against
# stock (flisp) lowering.
#
#   ./julia --startup-file=no UnifiedIR/demo/capture_zoo.jl
#
# Per case: the shared-container decision both lowerings make (Core.Box
# allocations in lowered code / closure-type field types), the inferred
# return type before/after, an EXECUTION differential (semantics must be
# byte-identical: mutation visibility, undef errors, multi-shot loops), and
# microbenchmarks of representative cases.
#
# Shares have TWO representations on our side (Part 3, the authorized
# mutable-struct lowering): a variable mutably captured by exactly ONE
# closure whose creation dominates its later home accesses merges into that
# closure as an untyped MUTABLE FIELD (`x::Any (mutable field)` below — one
# allocation instead of Box + closure); anything else (cross-closure
# sharing, multi-instance loops, recursion) keeps the classical `Core.Box`.
pushfirst!(LOAD_PATH, joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia"))
using JuliaLowering
using InteractiveUtils

const ZOO = raw"""
# 0. THE julia#15276 function (abmult, also the manual's "performance of
#    captured variable" example): the argument is reassigned before the
#    closure exists, so stock's assigned-once test boxes it — but every
#    store precedes the creation, so the mem2reg criterion proves a value
#    capture through the if-join. No `let r = r` workaround needed.
function abmult(r::Int)
    if r < 0
        r = -r
    end
    f = x -> x * r
    return f
end
# 1. assigned in both if-arms before capture: the join is a single value
function zoo1(c)
    local x
    if c; x = 1; else; x = 2; end
    cl = () -> x
    return cl()
end
# 2. try/catch definite assignment: value capture through the exception join
function zoo2(a)
    local x
    try
        x = sqrt(a)
    catch
        x = -1.0
    end
    cl = () -> x
    return cl()
end
# 3. mutation after creation but BEFORE FIRST USE: the creation statement
#    SINKS to just before the first use (the one code motion lowering is
#    allowed), the store lands before the sunk creation, and the capture
#    becomes a VALUE capture -- of the post-store value 2 (the execution
#    differential is the soundness sentinel)
function zoo3()
    x = 1
    f = () -> x
    x = 2
    return f()
end
# 3b. same store, but the closure is USED first: the use blocks the sink,
#     the store stays after the creation, must stay SHARED -- and since
#     exactly one closure captures x, the share is a mutable FIELD of the
#     (mutable struct) closure, not a separate Core.Box: the post-creation
#     home store compiles to setfield!(f, :x, 2)
function zoo3b()
    x = 1
    f = () -> x
    a = f()
    x = 2
    return (a, f())
end
# 4. closure created in a loop, var stored later in the body: multi-shot
#    backedge, must stay shared -- and KEEPS Core.Box: x's scope is outside
#    the loop while the creation repeats inside it, so all instances must
#    alias ONE location (the mutable-field merge is refused)
function zoo4(n)
    fs = Any[]
    local x::Int = 0
    for i in 1:n
        push!(fs, () -> x)
        x = i
    end
    return Any[f() for f in fs]
end
# 5. closure writing its capture: must stay shared (declared-type variant --
#    the declaration constrains the stored VALUES, never the container);
#    single capturer => merged mutable field, stores still funnel through
#    convert
function zoo5()
    local x::Int = 0
    inc = () -> (x = x + 1)
    inc(); inc(); inc()
    return x
end
# 5b. same, without a declared type
function zoo5b()
    x = 0
    inc = () -> (x = x + 1)
    inc(); inc(); inc()
    return x
end
# 6. maybe-undef capture: must stay shared (UndefVarError at USE, not at
#    capture); merges as an uninitialized trailing mutable field --
#    `new` omits it, creation conditionally initializes it, and the guarded
#    read still throws UndefVarError naming `x`
function zoo6(c)
    local x
    if c; x = 1; end
    f = () -> x
    return f
end
# 7. realistic: comprehension capture after conditional assignment
function zoo7(c)
    if c; x = 1; else; x = 2; end
    return [x + i for i in 1:3]
end
"""

module ZStock end   # flisp lowering
module ZOurs end    # JuliaLowering with the mem2reg capture analysis
Base.include_string(ZStock, ZOO, "zoo.jl")
JuliaLowering.include_string(ZOurs, ZOO, "zoo.jl")

# ---- decisions ------------------------------------------------------------

count_boxes_lowered(f, tt) =
    sum(ci -> count(s -> occursin("Core.Box", string(s)), ci.code),
        Base.code_lowered(f, tt))

function closure_fields(m::Module, fname)
    out = String[]
    for name in names(m; all = true)
        isdefined(m, name) || continue
        t = getglobal(m, name)
        (t isa Type && t <: Function && occursin(string(fname) * "#", string(name))) ||
            continue
        base = Base.unwrap_unionall(t)
        ft = fieldtypes(base)
        isempty(ft) && continue
        descr(i, n, T) = T isa TypeVar          ? "$n::<value capture, type param>" :
                         !isconst(base, i)      ? "$n::$T (mutable field)" :
                                                  "$n::$T"
        push!(out, join((descr(i, n, T) for (i, (n, T)) in
                         enumerate(zip(fieldnames(base), ft))), ", "))
    end
    return isempty(out) ? "-" : join(out, "; ")
end

rt_of(f, tt) = Base.return_types(f, tt)[1]

println("== capture decisions and inferred return types ==")
trunc26(x) = (t = string(x); length(t) > 25 ? t[1:22] * "..." : t)
println(rpad("case", 8), rpad("stock boxes", 13), rpad("stock rt", 26),
        rpad("ours rt", 26), "our closure fields")
for (name, tt) in [(:abmult, (Int,)), (:zoo1, (Bool,)), (:zoo2, (Float64,)), (:zoo3, ()),
                   (:zoo3b, ()), (:zoo4, (Int,)), (:zoo5, ()), (:zoo5b, ()),
                   (:zoo6, (Bool,)), (:zoo7, (Bool,))]
    fs = getglobal(ZStock, name)
    fo = getglobal(ZOurs, name)
    println(rpad(string(name), 8),
            rpad(string(count_boxes_lowered(fs, tt)), 13),
            rpad(trunc26(rt_of(fs, tt)), 26),
            rpad(trunc26(rt_of(fo, tt)), 26),
            closure_fields(ZOurs, name))
end

# ---- semantics differential ------------------------------------------------

outcome(f) = try
    (:ok, f())
catch e
    (:err, sprint(showerror, e))
end

println("\n== execution differential (stock vs ours; must MATCH) ==")
ndiff = 0
for (label, run) in [
    ("abmult(-3)(2)", M -> M.abmult(-3)(2)),   # the issue function: 6
    ("abmult(5)(7)", M -> M.abmult(5)(7)),
    ("zoo1(true)",   M -> M.zoo1(true)),
    ("zoo1(false)",  M -> M.zoo1(false)),
    ("zoo2(4.0)",    M -> M.zoo2(4.0)),
    ("zoo2(-4.0)",   M -> M.zoo2(-4.0)),      # DomainError path
    ("zoo3()",       M -> M.zoo3()),          # sunk creation snapshots 2
    ("zoo3b()",      M -> M.zoo3b()),         # use blocks sink: (1, 2)
    ("zoo4(3)",      M -> M.zoo4(3)),         # multi-shot: all see final x
    ("zoo5()",       M -> M.zoo5()),          # writes propagate out: 3
    ("zoo5b()",      M -> M.zoo5b()),
    ("zoo6(false)()", M -> M.zoo6(false)()),  # UndefVarError at USE
    ("zoo6(true)()", M -> M.zoo6(true)()),
    ("zoo7(true)",   M -> M.zoo7(true)),
    ("zoo7(false)",  M -> M.zoo7(false)),
]
    a = outcome(() -> run(ZStock))
    b = outcome(() -> run(ZOurs))
    ok = isequal(a, b)
    ok || global ndiff += 1
    println(rpad(label, 15), ok ? "MATCH   " : "DIFF!!  ", a)
end
println(ndiff == 0 ? "SEMANTICS: all cases match" :
        "SEMANTICS: $ndiff DIFFERENTIAL FAILURES")

# ---- the analysis IR itself --------------------------------------------------
# What the decision machinery actually sees: the enclosing body lowered to
# UnifiedIR in capture-analysis mode -- candidates as `cell_shared` cells and
# each closure-creation site a REAL `closure` region op whose deferred body
# is the capture footprint (one `cell_get` per captured variable) -- BEFORE
# and AFTER the shared mem2reg fixpoint. `promote_capture_cells!` decides
# value capture structurally: a candidate whose in-deferred reads were
# rewritten to values (the cell demoted and dissolved) is a legal VALUE
# capture; a surviving `cell_shared` keeps the shared container.

using UnifiedIR

function show_analysis_ir(title, src)
    println("\n---- $title ----")
    phase_ir = Dict{Symbol,String}()
    JuliaLowering.ACP_TRACE[] = (phase, lam, ir) ->
        (phase_ir[phase] = sprint(UnifiedIR.print_ir, ir))
    try
        m = Module()
        JuliaLowering.include_string(m, src, "trace.jl")
    finally
        JuliaLowering.ACP_TRACE[] = nothing
    end
    println("== analysis IR as emitted (closure regions, before mem2reg):")
    print(get(phase_ir, :before, "(no analysis ran)\n"))
    println("== after UnifiedIR.promote_fixpoint! (the shared machinery):")
    print(get(phase_ir, :after, "(no analysis ran)\n"))
end

println("\n== the analysis IR, before/after the shared mem2reg fixpoint ==")
show_analysis_ir("zoo1: if-arm join -> the capture read RESOLVES (value capture)", raw"""
function t1(c)
    local x
    if c; x = 1; else; x = 2; end
    cl = () -> x
    return cl()
end
""")
show_analysis_ir("""zoo3: the creation statement was SUNK past the store before this IR
     was even emitted (sink_closure_definitions! on the scoped tree — the
     one code motion lowering is allowed), so the closure op sits AFTER the
     `cell_set` and promote_capture_cells! proves the value capture at the
     sunk position; the resolved value is the post-store 2""", raw"""
function t3()
    x = 1
    f = () -> x
    x = 2
    return f()
end
""")
show_analysis_ir("""zoo3b: a USE between creation and store blocks the sink (the call
     mentions f), so the closure op stays put, criterion (b) inside
     promote_capture_cells! sees the `cell_set` AFTER it, and the value
     rewrite is refused — the shared cell survives""", raw"""
function t3b()
    x = 1
    f = () -> x
    a = f()
    x = 2
    return (a, f())
end
""")
show_analysis_ir("""zoo6: maybe-undef capture -> the cell SURVIVES the fixpoint (no
     definedness-as-data in analysis mode), so the read stays memory and the
     variable keeps the shared container with use-time UndefVarError""", raw"""
function t6(c)
    local x
    if c; x = 1; end
    f = () -> x
    return f
end
""")

# ---- microbenchmarks --------------------------------------------------------

const BENCH = raw"""
@noinline apply_it(f) = f()
@noinline call_n(f, n) = (for i in 1:n; f(i); end; nothing)
function bench_abmult(n)        # THE issue function, called in a hot loop
    if n < 0; n = -n; end
    f = x -> x * n
    s = 0
    for i in 1:n
        s = s ⊻ f(i)            # xor keeps LLVM from closed-forming the loop
    end
    s
end
function bench_join(n)          # zoo1 shape in a hot loop (escaping closure)
    local x
    if n > 0; x = n; else; x = -n; end
    f = () -> x
    s = 0
    for i in 1:n
        s = s ⊻ (apply_it(f) + i)
    end
    s
end
function bench_counter(n)       # zoo5 shape (shared-capture counter)
    local c::Int = 0
    acc = i -> (c = c ⊻ (c + i))   # not reducible to closed form
    call_n(acc, n)
    c
end
function bench_mutcap(n)        # Part 3 shape: escaping shared-capture
    fs = Vector{Any}(undef, n)  # closures created in a hot loop. Stock
    for i in 1:n                # allocates a Core.Box AND the closure per
        local x = 0             # element (and pays box->contents on every
        f = () -> (x = x + i)   # capture access); the merged mutable field
        fs[i] = f               # is ONE object and ONE indirection.
    end
    s = 0
    for f in fs
        s += f()
    end
    s
end
"""
Base.include_string(ZStock, BENCH, "bench.jl")
JuliaLowering.include_string(ZOurs, BENCH, "bench.jl")

const BENCH_SINK = Ref{Any}(0)  # consume results: a discarded, provably
                                # effect-free loop is dead code once it
                                # infers, and ours infers
function bench(f, n)
    BENCH_SINK[] = f(n)         # compile
    best = Inf
    for _ in 1:5
        t = @elapsed (BENCH_SINK[] = f(n))
        best = min(best, t)
    end
    return best
end

alloc_of(f, n) = (BENCH_SINK[] = f(n); @allocated (BENCH_SINK[] = f(n)))

println("\n== microbenchmarks (n = 1_000_000, best of 5) ==")
for name in (:bench_abmult, :bench_join, :bench_counter, :bench_mutcap)
    n = 1_000_000
    fs = getglobal(ZStock, name)
    fo = getglobal(ZOurs, name)
    ts = bench(fs, n)
    to = bench(fo, n)
    extra = if name === :bench_mutcap
        # the allocation win: Box + closure per iteration vs closure only
        as = alloc_of(fs, n)
        ao = alloc_of(fo, n)
        string("  alloc stock ", round(as / 2^20; digits = 1), " MiB",
               " ours ", round(ao / 2^20; digits = 1), " MiB (",
               round(as / ao; digits = 2), "x)")
    else
        ""
    end
    println(rpad(string(name), 15),
            "stock ", rpad(string(round(ts * 1000; digits = 2), " ms"), 12),
            "ours ", rpad(string(round(to * 1000; digits = 2), " ms"), 12),
            "speedup ", round(ts / to; digits = 1), "x", extra)
end

# ---- the late pipeline: inference over the closure regions themselves ------
# (Spliced into UnifiedIR/demo/capture_zoo.jl before the final exit; the
# `ndiff` accounting joins the demo's exit code.)
#
# Compiler.Unified's experimental late pipeline consumes the SAME
# pre-materialization region IR the backend produces (real `closure` ops +
# residual `cell_shared` cells, after the capture-decision fixpoint, before
# materialization) and runs unified inference extended to descend into
# deferred regions: shared-cell contents become a structural fixpoint over
# every store site — home and deferred alike — and visible closure call
# sites take the body's inferred return type. NO typed containers come out
# of this (a nominal closure's fields cannot be typed: a later-world store
# may produce a type unknowable at definition time — docs/closures.md); the
# win is inference-internal PRECISION, sound exactly when the closure is
# non-escaping and no world barrier sits between creation and call.

import Compiler
const ULATE = Compiler.load_unified!()

const ZOO_LATE = [
    # (label, source, expected-late-rettype-string)
    ("zoo5b", raw"""
     function zl5b()
         x = 0
         inc = () -> (x = x + 1)
         inc(); inc(); inc()
         return x
     end"""),
    ("zoo5-decl", raw"""
     function zl5()
         local x::Int = 0
         inc = () -> (x = x + 1)
         inc(); inc(); inc()
         return x
     end"""),
    ("zoo5b-escape", raw"""
     function zl5esc()
         x = 0
         inc = () -> (x = x + 1)
         holder = Any[inc]
         holder[1]()
         return x
     end"""),
]

# The front half, stopped where emit_method_region! would materialize: this
# is exactly the IR the late pipeline is defined on.
const JSZ = JuliaLowering.JuliaSyntax
function late_region_method(mod::Module, src::String)
    st0 = JSZ.parseall(JuliaLowering.SyntaxTree, src; filename = "late.jl")
    st = JSZ.kind(st0) == JSZ.K"toplevel" ? collect(JSZ.children(st0))[1] : st0
    ctx1, ex1 = JuliaLowering.expand_forms_1(mod, st, false, Base.get_world_counter())
    ctx2, ex2 = JuliaLowering.expand_forms_2(ctx1, ex1)
    ctx3, ex3 = JuliaLowering.resolve_scopes(ctx2, ex2)
    found = Ref{Any}(nothing)
    function walk(ex)
        found[] === nothing || return
        k = JSZ.kind(ex)
        if k == JSZ.K"method" && JSZ.numchildren(ex) == 3 && JSZ.kind(ex[3]) == JSZ.K"lambda"
            name = JuliaLowering.UnifiedBackend.method_name(ctx3, ex[1])
            ir, nargs, _, _ = JuliaLowering.UnifiedBackend.emit_lambda(
                ctx3, ex[3], name; region = true)
            UnifiedIR.promote_fixpoint!(ir; include_undef = false)
            found[] = (; name, nargs, ir)
            return
        end
        if k == JSZ.K"lambda"
            ex.is_toplevel_thunk && walk(ex[3])
            return
        end
        JSZ.is_leaf(ex) && return
        (k == JSZ.K"inert" || k == JSZ.K"inert_syntaxtree" || k == JSZ.K"quote") && return
        foreach(walk, JSZ.children(ex))
    end
    walk(ex3)
    return found[]
end

println("\n== the late pipeline: typed region IR (containers unchanged) ==")
println(rpad("case", 14), rpad("stock rt", 10), rpad("late rt", 10),
        rpad("cell reads / join", 26), "differential")
let lmod = Module()
    Base.eval(lmod, :(using Base))
    for (label, src) in ZOO_LATE
        Base.include_string(lmod, src, "$label.jl")
        m = late_region_method(lmod, src)
        res = ULATE.typed_region_ir!(m.ir, Any[Any])
        cellinfo = join(sort!([v.poisoned ? "Any (join $(v.content))" :
                               string(v.content) for v in values(res.cells)]), ", ")
        f = Base.invokelatest(getglobal, lmod, Symbol(m.name))
        stock_rt = Base.return_types(f, Tuple{})[1]
        a = outcome(() -> Base.invokelatest(f))
        b = outcome(() -> Base.invokelatest(UnifiedIR.interpret, m.ir, f))
        ok = isequal(a, b)
        ok || global ndiff += 1
        println(rpad(label, 14), rpad(string(stock_rt), 10),
                rpad(string(res.rettype), 10), rpad(cellinfo, 26),
                ok ? "MATCH  ($(repr(a[2])))" : "DIFF!! stock=$a late=$b")
    end
end

exit(ndiff == 0 ? 0 : 1)
