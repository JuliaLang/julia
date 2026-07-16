# Tests for the mem2reg-precise closure-capture analysis
# (src/unified/capture_analysis.jl): boxing decisions AND the semantics they
# must preserve. The must-stay-shared cases are soundness sentinels — a
# wrong-direction decision there is a silent miscompile of user code.

test_mod = Module()

# Count the shared-container representations in the closure-converted tree
# for `src`: `(nbox, nmut)` — `Core.Box` core refs (the classical container,
# exactly as flisp) and merged mutable closure fields (Part 3: field-kind
# codes >= 2 in `eval_closure_type` calls — a mutably-captured variable
# owned solely by one closure becomes an untyped mutable field of it).
function acp_capture_repr(src::AbstractString)
    st0 = JuliaSyntax.parseall(JuliaSyntax.SyntaxTree, src; filename = "acp_test.jl")
    st = JuliaSyntax.kind(st0) == JuliaSyntax.K"toplevel" ? st0[1] : st0
    world = Base.get_world_counter()
    ctx1, ex1 = JuliaLowering.expand_forms_1(test_mod, st, false, world)
    ctx2, ex2 = JuliaLowering.expand_forms_2(ctx1, ex1)
    ctx3, ex3 = JuliaLowering.resolve_scopes(ctx2, ex2)
    ctx4, ex4 = JuliaLowering.convert_closures(ctx3, ex3)
    nbox = Ref(0)
    nmut = Ref(0)
    walk(e) = begin
        k = JuliaSyntax.kind(e)
        if k == JuliaSyntax.K"core" && e.name_val == "Box"
            nbox[] += 1
        elseif k == JuliaSyntax.K"call" && JuliaSyntax.numchildren(e) >= 5 &&
               JuliaSyntax.kind(e[1]) == JuliaSyntax.K"Value" &&
               e[1].value === JuliaLowering.eval_closure_type
            # [eval_closure_type mod name svec(fields...) svec(kinds...)]
            for kindex in JuliaSyntax.children(e[5])
                if JuliaSyntax.kind(kindex) == JuliaSyntax.K"Integer" &&
                   kindex.value >= 2
                    nmut[] += 1
                end
            end
        end
        if !JuliaSyntax.is_leaf(e)
            foreach(walk, JuliaSyntax.children(e))
        end
        nothing
    end
    walk(ex4)
    return (nbox[], nmut[])
end
acp_count_shared(src::AbstractString) = acp_capture_repr(src)[1]
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
# Must-stay-shared sentinels: these MUST keep a shared mutable location, and
# the sharing semantics must be executable-identical to stock. The
# REPRESENTATION of the share is two-fold since Part 3: a variable mutably
# captured by exactly ONE closure (creation dominating its later home
# accesses) merges into that closure as an untyped MUTABLE FIELD
# (`acp_capture_repr(...) == (0, nmut)`); everything else keeps `Core.Box`.
# ---------------------------------------------------------------------------

# USE before the store blocks the sink: the closure observes the later store.
# Single capturer => the share is a merged mutable field, box-free.
@test acp_capture_repr("""
function acp_usestore()
    x = 1
    f = () -> x
    a = f()
    x = 2
    return (a, f())
end
""") == (0, 1)
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
# (shared, as a merged mutable field)
@test acp_capture_repr("""
function acp_isdefblocks()
    x = 1
    f = () -> x
    d = @isdefined(f)
    x = 2
    return (d, f())
end
""") == (0, 1)
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
# past the other's (the other's lambda MENTIONS it — a capture is a use).
# f is assigned before every use — a plain value capture (unboxed). g is
# assigned AFTER the first closure captured it, so it must stay shared —
# and since exactly one closure captures it, the share is a merged mutable
# field (maybe-undef flavor: uninitialized at the first creation). Identity
# must round-trip through both representations.
@test acp_capture_repr("""
function acp_mutual()
    local f, g
    f = () -> g
    g = () -> f
    return (f()() === f, g()() === g)
end
""") == (0, 1)
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
# from LATER iterations (one variable across iterations). The variable's
# scope is OUTSIDE the loop while the creation repeats inside it, so the
# mutable-field merge is refused (each `new` would get its own field while
# one location is required): the classical Core.Box stays.
@test acp_capture_repr("""
function acp_zoo4(n)
    fs = Any[]
    x = 0
    for i in 1:n
        push!(fs, () -> x)
        x = i
    end
    fs
end
""") == (1, 0)
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

# closure writing its capture: stores propagate back to the frame. One
# capturer, dominating creation: the share is a merged mutable field.
@test acp_capture_repr("""
function acp_zoo5()
    x = 0
    inc = () -> (x = x + 1)
    inc(); inc(); inc()
    return x
end
""") == (0, 1)
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

# maybe-undef capture: UndefVarError surfaces at USE inside the closure.
# Merges as the maybe-undef flavor: an uninitialized trailing mutable field
# (`new` omits it; creation conditionally initializes it from the local) —
# the guarded read still throws with the right variable name.
@test acp_capture_repr("""
function acp_zoo6(c)
    local x
    if c; x = 1; end
    f = () -> x
    return f
end
""") == (0, 1)
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

# self-recursive local function: self-capture needs the shared container —
# and it keeps the Core.Box (the container must exist BEFORE the instance;
# a field of the instance cannot; `rec` binder is future work)
@test acp_capture_repr("""
function acp_rec(n)
    fact(k) = k <= 1 ? 1 : k * fact(k - 1)
    fact(n)
end
""") == (1, 0)
@test JuliaLowering.include_string(test_mod, """
begin
    function acp_rec(n)
        fact(k) = k <= 1 ? 1 : k * fact(k - 1)
        fact(n)
    end
    acp_rec(5)
end
""") == 120

# store after capture reachable only around an outer loop backedge:
# stays shared AND keeps the Box (variable scoped outside the repeating
# creation — same multi-instance refusal as acp_zoo4)
@test acp_capture_repr("""
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
""") == (1, 0)

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

# ---------------------------------------------------------------------------
# Part 3: merged captures (closures as mutable structs). Representation
# probes for the applicability rules, executable stock differentials for
# every affected semantic, and a second seeded fuzz battery generating the
# mutable-field shapes heavily.
# ---------------------------------------------------------------------------

# the per-iteration counter merges when the variable's scope repeats with
# the creation (both inside the loop)
@test acp_capture_repr("""
function p3_loopfresh(n)
    t = 0
    for i in 1:n
        x = 0
        g = () -> (x = x + i)
        g(); g()
        t += x
    end
    t
end
""") == (0, 1)

# TWO closures capturing one variable: cross-closure sharing keeps the Box
# (v1; a shared frame struct is future work) — one Box() creation at the
# declaration, no merged fields
@test acp_capture_repr("""
function p3_crossclosure()
    x = 0
    a = () -> (x = x + 1)
    b = () -> (x = x + 10)
    a(); b()
    x
end
""") == (1, 0)

# creation inside an if-arm with accesses outside it: the creation does not
# dominate them — Box
@test acp_capture_repr("""
function p3_ifarm(c)
    x = 1
    local g
    if c
        g = () -> (x = x + 1)
    else
        g = () -> 0
    end
    g()
    x
end
""") == (1, 0)

# assigned argument captured and written by the closure: merges (arguments
# are defined at creation: unconditional field initialization)
@test acp_capture_repr("""
function p3_argwrite(x)
    g = () -> (x = x + 1)
    g(); g()
    x
end
""") == (0, 1)

# labels anywhere in the frame refuse the merge (jumps break the
# structural order arguments): Box, exactly as before
@test acp_capture_repr("""
function p3_label(x)
    g() = x
    if false
        @goto done
        @label done
    end
    x = 1
    return (g, x)
end
""") == (1, 0)

# home mutation visibility BOTH directions through the merged field
@test JuliaLowering.include_string(test_mod, """
begin
    function p3_bothdirs()
        x = 1
        bump = () -> (x = x + 10)
        bump()                   # closure write, home read below
        a = x
        x = 100                  # home write, closure reads it next call
        b = bump()
        (a, b, x)
    end
    p3_bothdirs()
end
""") == (11, 110, 110)

# `@isdefined x` inside the closure over a maybe-undef merged capture
@test JuliaLowering.include_string(test_mod, """
begin
    function p3_isdef_inner(c)
        local x
        if c; x = 1; end
        f = () -> (@isdefined x)
        f()
    end
    (p3_isdef_inner(true), p3_isdef_inner(false))
end
""") == (true, false)

# maybe-undef merged capture becomes defined through a home write AFTER the
# creation (setfield! into the uninitialized trailing field)
@test JuliaLowering.include_string(test_mod, """
begin
    function p3_late_def(c)
        local x
        f = () -> x
        if c; x = 42; end
        try; f(); catch e; e isa UndefVarError ? :undef_of_x : rethrow(); end
    end
    (p3_late_def(true), p3_late_def(false))
end
""") == (42, :undef_of_x)

# executable stock differentials: every Part 3-affected semantic executed
# under stock lowering (Base.include_string) and ours — MATCH required,
# including thrown error type and UndefVarError variable name
let cases = [
        "mutation visible both directions" => """
            begin
                function d1()
                    x = 1
                    bump = () -> (x = x + 10)
                    bump(); a = x; x = 100; b = bump()
                    (a, b, x)
                end
                d1()
            end""",
        "multi-shot loop sees later iterations" => """
            begin
                function d2(n)
                    fs = Any[]
                    x = 0
                    for i in 1:n
                        push!(fs, () -> x)
                        x = i
                    end
                    Any[f() for f in fs]
                end
                d2(3)
            end""",
        "maybe-undef UndefVarError at use, right name" => """
            begin
                function d3(c)
                    local xvar
                    if c; xvar = 1; end
                    f = () -> xvar
                    f()
                end
                d3(false)
            end""",
        "@isdefined through the share" => """
            begin
                function d4(c)
                    local x
                    if c; x = 1; end
                    f = () -> x
                    pre = @isdefined x
                    (pre, f isa Function, (@isdefined x))
                end
                d4(true)
            end""",
        "cross-closure share" => """
            begin
                function d5()
                    x = 0
                    a = () -> (x = x + 1)
                    b = () -> (x = x + 10)
                    a(); b(); a()
                    x
                end
                d5()
            end""",
        "recursion (self-capture)" => """
            begin
                function d6(n)
                    fact(k) = k <= 1 ? 1 : k * fact(k - 1)
                    fact(n)
                end
                d6(6)
            end""",
        "declared-type convert funnel InexactError" => """
            begin
                function d7()
                    local x::Int = 0
                    setx = v -> (x = v)
                    setx(1.5)
                    x
                end
                d7()
            end""",
        "declared-type convert funnel converts" => """
            begin
                function d8()
                    local x::Int = 0
                    setx = v -> (x = v)
                    setx(2.0)
                    x
                end
                d8()
            end""",
        "per-iteration loop counter" => """
            begin
                function d9(n)
                    t = 0
                    for i in 1:n
                        x = 0
                        g = () -> (x = x + i)
                        g(); g()
                        t += x
                    end
                    t
                end
                d9(4)
            end""",
        "mutual pair identity" => """
            begin
                function d10()
                    local f, g
                    f = () -> g
                    g = () -> f
                    (f()() === f, g()() === g)
                end
                d10()
            end""",
    ]
    # like sfz_run, but unwraps Base.include_string's LoadError so thrown
    # errors compare by their real identity
    p3_run(includer, m, src) = try
        (:ok, includer(m, src, "p3.jl"))
    catch e
        e isa LoadError && (e = e.error)
        (:err, e isa UndefVarError ? (:undef, e.var) : typeof(e))
    end
    for (label, src) in cases
        stock = p3_run(Base.include_string, Module(), src)
        ours = p3_run(JuliaLowering.include_string, Module(), src)
        @test isequal(stock, ours)
        isequal(stock, ours) || @error "Part 3 differential" label stock ours
    end
end

# ---------------------------------------------------------------------------
# Seeded source-level differential fuzz, mutable-field battery: the merged
# shapes (write captures, maybe-undef writes, cross-closure sharing,
# per-iteration loop mutation, post-creation home traffic, declared types)
# are generated heavily and executed under stock lowering and ours — exact
# agreement required. Fixed seed; histogram asserted.
# ---------------------------------------------------------------------------

function mfz_gen(rng::AbstractRNG, name::String, stats::Dict{Symbol,Int})
    stmts = String[]
    push!(stmts, "r = 0")
    # x's declaration/initialization
    defined = true
    typed = false
    roll = rand(rng, 1:6)
    if roll == 1
        push!(stmts, "local x")
        defined = false
        stats[:maybe_undef] += 1
    elseif roll == 2
        push!(stmts, "local x::Int")
        push!(stmts, "x = $(rand(rng, 0:3))")
        typed = true
        stats[:typed_decl] += 1
    else
        push!(stmts, "x = $(rand(rng, 0:3))")
    end
    # the creation(s): the closure WRITES its capture (the merged class)
    two = rand(rng, 1:4) == 1
    wexpr = typed && rand(rng, Bool) ? "x = x + $(rand(rng, 1:3)).0" :
                                       "x = x + $(rand(rng, 1:3))"
    push!(stmts, "f = () -> ($wexpr; x)")
    stats[:write_capture] += 1
    defined || (stats[:maybe_undef_write] += 1)
    if two
        push!(stmts, "g = () -> (x = x + 100)")
        stats[:cross_closure] += 1
    end
    # post-creation events
    for _ in 1:rand(rng, 2:5)
        ev = rand(rng, 1:12)
        if ev <= 3                       # call the writer
            push!(stmts, "r = r * 7 + (try; f(); catch e; e isa UndefVarError ? -9 : rethrow(); end)")
            stats[:calls] += 1
        elseif ev <= 5                   # home read after creation
            push!(stmts, "r = r * 3 + ((@isdefined x) ? x : -5)")
            stats[:post_home_read] += 1
        elseif ev <= 7                   # home write after creation
            push!(stmts, defined && rand(rng, Bool) ? "x = x + $(rand(rng, 1:3))" :
                                                      "x = $(rand(rng, 10:13))")
            defined = true
            stats[:post_home_write] += 1
        elseif ev == 8                   # conditional home write
            push!(stmts, "if c; x = $(rand(rng, 20:23)); end")
            stats[:post_ifwrite] += 1
        elseif ev == 9 && two            # call the second closure
            push!(stmts, "r = r * 7 + (try; g(); catch e; e isa UndefVarError ? -8 : rethrow(); end)")
            stats[:calls] += 1
        elseif ev == 10                  # @isdefined observation
            push!(stmts, "r = r * 2 + ((@isdefined x) ? 1 : 0)")
            stats[:isdef_obs] += 1
        else                             # per-iteration loop-local writer
            k = rand(rng, 1:3)
            push!(stmts, "for i in 1:2; local y = i * $k; " *
                         "h = () -> (y = y + i); h(); r = r * 5 + y; end")
            stats[:loop_local_mut] += 1
        end
    end
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

let rng = Xoshiro(0xB0C5ED), iters = 200
    stats = Dict{Symbol,Int}(k => 0 for k in
        (:maybe_undef, :typed_decl, :write_capture, :maybe_undef_write,
         :cross_closure, :calls, :post_home_read, :post_home_write,
         :post_ifwrite, :isdef_obs, :loop_local_mut))
    stock_mod = Module()
    ours_mod = Module()
    ndiff = 0
    for i in 1:iters
        src = mfz_gen(rng, "mfz_$i", stats)
        stock = sfz_run(Base.include_string, stock_mod, src)
        ours = sfz_run(JuliaLowering.include_string, ours_mod, src)
        if !isequal(stock, ours)
            ndiff += 1
            ndiff <= 5 && @error "mutable-field fuzz differential" i stock ours src
        end
    end
    @info "merged-capture (mutable struct) source fuzz ($iters programs)" stats...
    @test ndiff == 0
    # the histogram must prove the mutable-field classes are exercised
    @test stats[:write_capture] >= 190
    @test stats[:post_home_write] >= 80
    @test stats[:post_home_read] >= 80
    @test stats[:cross_closure] >= 30
    @test stats[:maybe_undef_write] >= 25
    @test stats[:loop_local_mut] >= 40
    @test stats[:typed_decl] >= 25
end

JuliaLowering.ACP_STRICT[] = strict_was
