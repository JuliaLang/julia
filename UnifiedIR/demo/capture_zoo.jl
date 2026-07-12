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
# a microbenchmark of two representative cases.
pushfirst!(LOAD_PATH, joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia"))
using JuliaLowering
using InteractiveUtils

const ZOO = raw"""
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
# 3. mutation AFTER capture: must stay shared (typed: join of literals = Int)
function zoo3()
    x = 1
    f = () -> x
    x = 2
    return f()
end
# 4. closure created in a loop, var stored later in the body: multi-shot
#    backedge, must stay shared (typed variant via the declared type)
function zoo4(n)
    fs = Any[]
    local x::Int = 0
    for i in 1:n
        push!(fs, () -> x)
        x = i
    end
    return Any[f() for f in fs]
end
# 5. closure writing its capture: must stay shared (typed via declared type)
function zoo5()
    local x::Int = 0
    inc = () -> (x = x + 1)
    inc(); inc(); inc()
    return x
end
# 5b. same, without a declared type: the write's RHS is not a literal, so the
#     container type is not provable at lowering time -> stays Core.Box
function zoo5b()
    x = 0
    inc = () -> (x = x + 1)
    inc(); inc(); inc()
    return x
end
# 6. maybe-undef capture: must stay Core.Box (UndefVarError at USE, not at
#    capture -- a typed container could not represent the undef state)
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
        descr(n, T) = T isa TypeVar ? "$n::<value capture, type param>" : "$n::$T"
        push!(out, join((descr(n, T) for (n, T) in zip(fieldnames(base), ft)), ", "))
    end
    return isempty(out) ? "-" : join(out, "; ")
end

rt_of(f, tt) = Base.return_types(f, tt)[1]

println("== capture decisions and inferred return types ==")
trunc26(x) = (t = string(x); length(t) > 25 ? t[1:22] * "..." : t)
println(rpad("case", 8), rpad("stock boxes", 13), rpad("stock rt", 26),
        rpad("ours rt", 26), "our closure fields")
for (name, tt) in [(:zoo1, (Bool,)), (:zoo2, (Float64,)), (:zoo3, ()),
                   (:zoo4, (Int,)), (:zoo5, ()), (:zoo5b, ()),
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
    ("zoo1(true)",   M -> M.zoo1(true)),
    ("zoo1(false)",  M -> M.zoo1(false)),
    ("zoo2(4.0)",    M -> M.zoo2(4.0)),
    ("zoo2(-4.0)",   M -> M.zoo2(-4.0)),      # DomainError path
    ("zoo3()",       M -> M.zoo3()),          # mutation visible: 2
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
show_analysis_ir("""zoo3: mutation after capture -> stays shared: criterion (b) inside
     promote_capture_cells! sees the `cell_set` AFTER the closure op (a
     store the deferred reads can observe at call time) and refuses the
     value rewrite""", raw"""
function t3()
    x = 1
    f = () -> x
    x = 2
    return f()
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
function bench_counter(n)       # zoo5 shape (typed container vs Core.Box)
    local c::Int = 0
    acc = i -> (c = c ⊻ (c + i))   # not reducible to closed form
    call_n(acc, n)
    c
end
"""
Base.include_string(ZStock, BENCH, "bench.jl")
JuliaLowering.include_string(ZOurs, BENCH, "bench.jl")

function bench(f, n)
    f(n)                        # compile
    best = Inf
    for _ in 1:5
        t = @elapsed f(n)
        best = min(best, t)
    end
    return best
end

println("\n== microbenchmarks (n = 1_000_000, best of 5) ==")
for name in (:bench_join, :bench_counter)
    n = 1_000_000
    ts = bench(getglobal(ZStock, name), n)
    to = bench(getglobal(ZOurs, name), n)
    println(rpad(string(name), 15),
            "stock ", rpad(string(round(ts * 1000; digits = 2), " ms"), 12),
            "ours ", rpad(string(round(to * 1000; digits = 2), " ms"), 12),
            "speedup ", round(ts / to; digits = 1), "x")
end

exit(ndiff == 0 ? 0 : 1)
