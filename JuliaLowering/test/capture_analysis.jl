# Tests for the mem2reg-precise closure-capture analysis
# (src/unified/capture_analysis.jl): boxing decisions AND the semantics they
# must preserve. The must-stay-shared cases are soundness sentinels — a
# wrong-direction decision there is a silent miscompile of user code.

test_mod = Module()

# Count shared-container evidence (`Core.Box` core refs — the only shared
# container lowering emits, exactly as flisp) in the closure-converted tree
# for `src`.
function acp_count_shared(src::AbstractString)
    st0 = JuliaSyntax.parseall(JuliaSyntax.SyntaxTree, src; filename = "acp_test.jl")
    st = JuliaSyntax.kind(st0) == JuliaSyntax.K"toplevel" ? st0[1] : st0
    world = Base.get_world_counter()
    ctx1, ex1 = JuliaLowering.expand_forms_1(test_mod, st, false, world)
    ctx2, ex2 = JuliaLowering.expand_forms_2(ctx1, ex1)
    ctx3, ex3 = JuliaLowering.resolve_scopes(ctx2, ex2)
    ctx4, ex4 = JuliaLowering.convert_closures(ctx3, ex3)
    n = Ref(0)
    walk(e) = begin
        k = JuliaSyntax.kind(e)
        if k == JuliaSyntax.K"core" && e.name_val == "Box"
            n[] += 1
        elseif !JuliaSyntax.is_leaf(e)
            foreach(walk, JuliaSyntax.children(e))
        end
        nothing
    end
    walk(ex4)
    return n[]
end
acp_count_boxes(src::AbstractString) = acp_count_shared(src)

# NB. no enclosing top-level `try` block here: everything below must run as
# separate top-level expressions so `include_string`-created bindings are
# visible to later tests (a single wrapping expression freezes world age).
strict_was = JuliaLowering.ACP_STRICT[]
JuliaLowering.ACP_STRICT[] = true

# ---------------------------------------------------------------------------
# Newly-unboxed cases (the julia#15276 precision wins): boxes disappear AND
# the behavior is unchanged.
# ---------------------------------------------------------------------------

# assigned in both if-arms before the closure: the join is a value capture
@test acp_count_shared("""
function acp_zoo1(c)
    local x
    if c; x = 1; else; x = 2; end
    cl = () -> x
    return cl()
end
""") == 0
@test JuliaLowering.include_string(test_mod, """
begin
    function acp_zoo1(c)
        local x
        if c; x = 1; else; x = 2; end
        cl = () -> x
        return cl()
    end
    (acp_zoo1(true), acp_zoo1(false))
end
""") == (1, 2)

# try/catch definite assignment: value capture through the exception join
@test acp_count_shared("""
function acp_zoo2(a)
    local x
    try
        x = sqrt(a)
    catch
        x = -1.0
    end
    cl = () -> x
    return cl()
end
""") == 0
@test JuliaLowering.include_string(test_mod, """
begin
    function acp_zoo2(a)
        local x
        try
            x = sqrt(a)
        catch
            x = -1.0
        end
        cl = () -> x
        return cl()
    end
    (acp_zoo2(4.0), acp_zoo2(-4.0))
end
""") == (2.0, -1.0)

# conditional assignment feeding a comprehension (Generator closure)
@test acp_count_shared("""
function acp_zoo7(c)
    if c; x = 1; else; x = 2; end
    return [x + i for i in 1:3]
end
""") == 0
@test JuliaLowering.include_string(test_mod, """
begin
    function acp_zoo7(c)
        if c; x = 1; else; x = 2; end
        return [x + i for i in 1:3]
    end
    (acp_zoo7(true), acp_zoo7(false))
end
""") == ([2, 3, 4], [3, 4, 5])

# straight-line loop-local capture stays unboxed (stock parity)
@test acp_count_shared("""
function acp_loopfresh(n)
    fs = Any[]
    for i in 1:n
        x = i
        push!(fs, () -> x)
    end
    fs
end
""") == 0

# ---------------------------------------------------------------------------
# Closure-definition sinking (closure_conversion.jl
# sink_closure_definitions!): a creation statement moves down to just before
# its first use, so a store between creation and first use lands BEFORE the
# (sunk) creation and the capture criterion holds there. The differential is
# the point: the snapshot MUST see the post-store value — a value capture
# decided at the sunk position but emitted at the original one would
# silently see the stale value.
# ---------------------------------------------------------------------------

# store after creation, before first use: sinks -> VALUE capture of 2
@test acp_count_shared("""
function acp_zoo3()
    x = 1
    f = () -> x
    x = 2
    return f()
end
""") == 0
@test JuliaLowering.include_string(test_mod, """
begin
    function acp_zoo3()
        x = 1
        f = () -> x
        x = 2
        return f()
    end
    acp_zoo3()
end
""") == 2

# store inside an `if` between creation and use: the whole `if` statement
# never mentions f, so the creation sinks past it and captures the arm JOIN
@test acp_count_shared("""
function acp_ifstore(c)
    x = 1
    f = () -> x
    if c; x = 2; end
    return f()
end
""") == 0
@test JuliaLowering.include_string(test_mod, """
begin
    function acp_ifstore(c)
        x = 1
        f = () -> x
        if c; x = 2; end
        return f()
    end
    (acp_ifstore(true), acp_ifstore(false))
end
""") == (2, 1)

# sinking is legal past statements that may throw or exit early: on such a
# path control leaves the block and nothing can observe f afterwards
@test acp_count_shared("""
function acp_exitskip(c)
    x = 1
    f = () -> x
    if c; return 99; end
    x = 2
    return f()
end
""") == 0
@test JuliaLowering.include_string(test_mod, """
begin
    function acp_exitskip(c)
        x = 1
        f = () -> x
        if c; return 99; end
        x = 2
        return f()
    end
    (acp_exitskip(true), acp_exitskip(false))
end
""") == (99, 2)

# ---------------------------------------------------------------------------
# Must-stay-shared sentinels: these MUST keep the shared container, and the
# sharing semantics must be executable-identical to stock.
# ---------------------------------------------------------------------------

# USE before the store blocks the sink: the closure observes the later store
@test acp_count_shared("""
function acp_usestore()
    x = 1
    f = () -> x
    a = f()
    x = 2
    return (a, f())
end
""") > 0
@test JuliaLowering.include_string(test_mod, """
begin
    function acp_usestore()
        x = 1
        f = () -> x
        a = f()
        x = 2
        return (a, f())
    end
    acp_usestore()
end
""") == (1, 2)

# `@isdefined f` between creation and store is a use: blocks the sink
@test acp_count_shared("""
function acp_isdefblocks()
    x = 1
    f = () -> x
    d = @isdefined(f)
    x = 2
    return (d, f())
end
""") > 0
@test JuliaLowering.include_string(test_mod, """
begin
    function acp_isdefblocks()
        x = 1
        f = () -> x
        d = @isdefined(f)
        x = 2
        return (d, f())
    end
    acp_isdefblocks()
end
""") == (true, 2)

# two closures capturing each other's binding: neither creation may sink
# past the other's (the other's lambda MENTIONS it — a capture is a use);
# the second-created binding stays shared, and identity must round-trip
@test acp_count_shared("""
function acp_mutual()
    local f, g
    f = () -> g
    g = () -> f
    return (f()() === f, g()() === g)
end
""") > 0
@test JuliaLowering.include_string(test_mod, """
begin
    function acp_mutual()
        local f, g
        f = () -> g
        g = () -> f
        return (f()() === f, g()() === g)
    end
    acp_mutual()
end
""") == (true, true)

# split creation groups: kwargs closures desugar to a kw-body closure and a
# sorter whose decls/method_defs are SEPARATE statements of one block, and
# the sorter's DECL materializes its instance reading the captured kw-body
# binding — an implicit mention outside the decl's subtree. The sink scan
# must refuse to move the kw-body's bare decl past it (regression: sinking
# it made the sorter capture an undefined variable).
@test acp_count_shared("""
function acp_kwsplit()
    function inner(x; scale=2)
        return x * scale
    end
    (inner(3), inner(3; scale=5))
end
""") == 0
@test JuliaLowering.include_string(test_mod, """
begin
    function acp_kwsplit()
        function inner(x; scale=2)
            return x * scale
        end
        (inner(3), inner(3; scale=5))
    end
    acp_kwsplit()
end
""") == (6, 15)

# multi-shot loop backedge: a closure created in the loop observes stores
# from LATER iterations (one variable across iterations)
@test acp_count_shared("""
function acp_zoo4(n)
    fs = Any[]
    x = 0
    for i in 1:n
        push!(fs, () -> x)
        x = i
    end
    fs
end
""") > 0
@test JuliaLowering.include_string(test_mod, """
begin
    function acp_zoo4(n)
        fs = Any[]
        x = 0
        for i in 1:n
            push!(fs, () -> x)
            x = i
        end
        Any[f() for f in fs]
    end
    acp_zoo4(3)
end
""") == Any[3, 3, 3]

# closure writing its capture: stores propagate back to the frame
@test acp_count_shared("""
function acp_zoo5()
    x = 0
    inc = () -> (x = x + 1)
    inc(); inc(); inc()
    return x
end
""") > 0
@test JuliaLowering.include_string(test_mod, """
begin
    function acp_zoo5()
        x = 0
        inc = () -> (x = x + 1)
        inc(); inc(); inc()
        return x
    end
    acp_zoo5()
end
""") == 3

# maybe-undef capture: UndefVarError surfaces at USE inside the closure
@test acp_count_boxes("""
function acp_zoo6(c)
    local x
    if c; x = 1; end
    f = () -> x
    return f
end
""") > 0
@test JuliaLowering.include_string(test_mod, """
begin
    function acp_zoo6(c)
        local x
        if c; x = 1; end
        f = () -> x
        return f
    end
    acp_zoo6(true)()
end
""") == 1
@test_throws UndefVarError JuliaLowering.include_string(test_mod, """
acp_zoo6(false)()
""")

# self-recursive local function: self-capture needs the shared container
@test acp_count_shared("""
function acp_rec(n)
    fact(k) = k <= 1 ? 1 : k * fact(k - 1)
    fact(n)
end
""") > 0
@test JuliaLowering.include_string(test_mod, """
begin
    function acp_rec(n)
        fact(k) = k <= 1 ? 1 : k * fact(k - 1)
        fact(n)
    end
    acp_rec(5)
end
""") == 120

# store after capture reachable only around an outer loop backedge
@test acp_count_shared("""
function acp_backedge(n)
    fs = Any[]
    local x
    x = 0
    for i in 1:n
        push!(fs, () -> x)   # sees iteration i+1's store: must stay shared
        if i > 1
            x = i
        end
    end
    fs
end
""") > 0

# mutation observed through a nested (transitive) capture chain
@test JuliaLowering.include_string(test_mod, """
begin
    function acp_nested()
        x = 1
        g = () -> (() -> x)
        h = g()
        x = 2
        h()
    end
    acp_nested()
end
""") == 2

# ---------------------------------------------------------------------------
# Interaction with argument captures
# ---------------------------------------------------------------------------

# assigned argument, no store after the capture site: value capture
@test acp_count_shared("""
function acp_arg(x)
    x = x + 1
    g = () -> x
    g()
end
""") == 0

# assigned argument with a store between creation and first use: the
# creation sinks past it -> value capture of the post-increment value
@test acp_count_shared("""
function acp_arg2(x)
    g = () -> x
    x = x + 1
    g()
end
""") == 0
@test JuliaLowering.include_string(test_mod, """
begin
    function acp_arg2(x)
        g = () -> x
        x = x + 1
        g()
    end
    acp_arg2(41)
end
""") == 42

# ---------------------------------------------------------------------------
# Declared-type shares: the declared type constrains the VALUES (stores stay
# funneled through convert) but never the container — lowering may not type
# a shared container (that would need binding-table reads / inference, the
# compiler pipeline's job). Semantics must match stock exactly.
# ---------------------------------------------------------------------------

tc_mod = Module()
@test JuliaLowering.include_string(tc_mod, """
begin
    function tc_decl()          # declared type, closure writes its capture
        local x::Int = 0
        inc = () -> (x = x + 1)
        inc(); inc(); inc()
        x
    end
    function tc_loop(n)         # declared type, multi-shot loop sharing
        fs = Any[]
        local x::Int = 0
        for i in 1:n
            push!(fs, () -> x)
            x = i
        end
        Any[f() for f in fs]
    end
    function tc_conv()          # the convert funnel operates through the share
        local x::Int = 0
        setx = v -> (x = v)
        setx(2.0)
        x
    end
    (tc_decl(), tc_loop(3), tc_conv())
end
""") == (3, Any[3, 3, 3], 2)
@test JuliaLowering.include_string(tc_mod, """
begin
    function tc_conv2()
        local x::Int = 0
        setx = v -> (x = v)
        setx(1.5)               # inexact through the shared store: must throw
        x
    end
    try; tc_conv2(); "no throw"; catch e; typeof(e); end
end
""") === InexactError

# ---------------------------------------------------------------------------
# Seeded source-level differential fuzz: random capture shapes executed under
# stock lowering (Base.include_string) and ours must agree exactly — value or
# thrown error. This is the SINKING battery: the
# store-after-creation-before-first-use class is generated heavily, so a
# capture decision made at the sunk position with emission at the original
# one (the silent-miscompile hazard) surfaces as a differential, never as a
# green run. Fixed seed; the histogram is asserted so the shapes provably
# exercise the feature.
# ---------------------------------------------------------------------------

using Random

function sfz_gen(rng::AbstractRNG, name::String, stats::Dict{Symbol,Int})
    stmts = String[]
    push!(stmts, "r = 0")
    # x's declaration/initialization
    defined = true
    roll = rand(rng, 1:6)
    if roll == 1
        push!(stmts, "local x")
        defined = false
        stats[:maybe_undef] += 1
    elseif roll == 2
        push!(stmts, "local x::Int")
        push!(stmts, "x = $(rand(rng, 0:3))")
        stats[:decl_type] += 1
    elseif roll == 3
        push!(stmts, "if c; x = $(rand(rng, 0:3)); end")
        defined = false
        stats[:maybe_undef] += 1
    elseif roll == 4
        push!(stmts, "if c; x = $(rand(rng, 0:3)); else; x = $(rand(rng, 4:7)); end")
        stats[:if_join] += 1
    else
        push!(stmts, "x = $(rand(rng, 0:3))")
    end
    # the creation
    body = rand(rng, 1:4)
    if body == 1
        push!(stmts, "f = () -> (x = x + $(rand(rng, 1:3)); x)")   # write capture
        stats[:write_capture] += 1
    else
        push!(stmts, "f = () -> x")
    end
    creation_idx = length(stmts)
    # post-creation events; track whether a store precedes the first use
    used = false
    store_before_use = false
    for _ in 1:rand(rng, 1:4)
        ev = rand(rng, 1:10)
        if ev <= 3                       # plain store
            push!(stmts, defined && rand(rng, Bool) ? "x = x + $(rand(rng, 1:3))" :
                                                      "x = $(rand(rng, 10:13))")
            defined = true
            used || (store_before_use = true)
            stats[:post_store] += 1
        elseif ev == 4                   # conditional store
            push!(stmts, "if c; x = $(rand(rng, 20:23)); end")
            used || (store_before_use = true)
            stats[:post_ifstore] += 1
        elseif ev <= 7                   # call use
            push!(stmts, "r = r * 7 + (try; f(); catch e; e isa UndefVarError ? -9 : rethrow(); end)")
            used = true
            stats[:calls] += 1
        elseif ev == 8                   # alias escape (a use)
            push!(stmts, "g = f")
            push!(stmts, "r = r * 7 + (try; g(); catch e; e isa UndefVarError ? -8 : rethrow(); end)")
            used = true
            stats[:aliases] += 1
        elseif ev == 9                   # @isdefined observation (a use)
            push!(stmts, "r = r * 2 + ((@isdefined f) ? 1 : 0)")
            used = true
            stats[:isdef_uses] += 1
        else                             # loop-wrapped fresh-x creation+use
            k = rand(rng, 1:3)
            push!(stmts, "for i in 1:2; local y; y = i * $k; " *
                         "h = () -> y; y = y + i; r = r * 5 + h(); end")
            stats[:loop_fresh] += 1
        end
    end
    used || push!(stmts, "r = r * 7 + (try; f(); catch e; e isa UndefVarError ? -7 : rethrow(); end)")
    store_before_use && (stats[:store_before_first_use] += 1)
    _ = creation_idx
    src = """
    begin
        function $name(c)
            $(join(stmts, "\n        "))
            return (r, (@isdefined x) ? x : :undef)
        end
        ($name(true), $name(false))
    end
    """
    return src
end

sfz_run(includer, m, src) = try
    (:ok, includer(m, src, "sfz.jl"))
catch e
    (:err, e isa UndefVarError ? (:undef, e.var) : typeof(e))
end

let rng = Xoshiro(0x51EE7), iters = 200
    stats = Dict{Symbol,Int}(k => 0 for k in
        (:maybe_undef, :decl_type, :if_join, :write_capture, :post_store,
         :post_ifstore, :calls, :aliases, :isdef_uses, :loop_fresh,
         :store_before_first_use))
    stock_mod = Module()
    ours_mod = Module()
    ndiff = 0
    for i in 1:iters
        src = sfz_gen(rng, "sfz_$i", stats)
        stock = sfz_run(Base.include_string, stock_mod, src)
        ours = sfz_run(JuliaLowering.include_string, ours_mod, src)
        if !isequal(stock, ours)
            ndiff += 1
            ndiff <= 5 && @error "sinking fuzz differential" i stock ours src
        end
    end
    @info "capture/sinking source fuzz ($iters programs)" stats...
    @test ndiff == 0
    # the histogram must prove the sinking class is exercised
    @test stats[:store_before_first_use] >= 50
    @test stats[:write_capture] >= 30
    @test stats[:maybe_undef] >= 30
end

JuliaLowering.ACP_STRICT[] = strict_was
