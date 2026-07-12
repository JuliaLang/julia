# Tests for the mem2reg-precise closure-capture analysis
# (src/unified/capture_analysis.jl): boxing decisions AND the semantics they
# must preserve. The must-stay-shared cases are soundness sentinels — a
# wrong-direction decision there is a silent miscompile of user code.

test_mod = Module()

# Count shared-container mentions in the closure-converted tree for `src`:
# `Core.Box` core refs plus typed containers (Base.RefValue{T} Value nodes).
# `acp_count_boxes` counts only the untyped Core.Box.
function acp_count_shared(src::AbstractString; boxes_only::Bool = false)
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
        elseif !boxes_only && k == JuliaSyntax.K"Value" &&
               (v = e.value; v isa Type && v <: Base.RefValue)
            n[] += 1
        elseif !JuliaSyntax.is_leaf(e)
            foreach(walk, JuliaSyntax.children(e))
        end
        nothing
    end
    walk(ex4)
    return n[]
end
acp_count_boxes(src::AbstractString) = acp_count_shared(src; boxes_only = true)

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
# Must-stay-shared sentinels: these MUST keep the shared container, and the
# sharing semantics must be executable-identical to stock.
# ---------------------------------------------------------------------------

# mutation after capture: the closure observes the later store
@test acp_count_shared("""
function acp_zoo3()
    x = 1
    f = () -> x
    x = 2
    return f()
end
""") > 0
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

# assigned argument with a store after the capture: stays shared
@test acp_count_shared("""
function acp_arg2(x)
    g = () -> x
    x = x + 1
    g()
end
""") > 0
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
# Typed shared containers: when sharing is unavoidable but the value type is
# provable at lowering time (and undef impossible), the container is
# Base.RefValue{T} instead of Core.Box.
# ---------------------------------------------------------------------------

closure_field_types(m::Module) = Dict(
    string(name) => collect(zip(fieldnames(getglobal(m, name)),
                                fieldtypes(getglobal(m, name))))
    for name in names(m; all = true)
    if isdefined(m, name) && getglobal(m, name) isa Type &&
       getglobal(m, name) <: Function && occursin("#", string(name)))

tc_mod = Module()
@test JuliaLowering.include_string(tc_mod, """
begin
    function tc_join()          # literal join -> RefValue{Int}
        x = 1
        f = () -> x
        x = 2
        f()
    end
    function tc_decl()          # declared type -> RefValue{Int}, closure writes
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
    function tc_undef(c)        # maybe-undef: MUST keep Core.Box
        local x
        if c; x = 1; end
        f = () -> x
        f
    end
    (tc_join(), tc_decl(), tc_loop(3))
end
""") == (2, 3, Any[3, 3, 3])
@test_throws UndefVarError JuliaLowering.include_string(tc_mod, "tc_undef(false)()")

let fts = closure_field_types(tc_mod)
    # match the closure TYPE (has capture fields), not the named function type
    fieldof(pat) = only([v for (k, v) in fts if occursin(pat, k) && !isempty(v)])
    @test fieldof("tc_join") == [(:x, Base.RefValue{Int})]
    @test fieldof("tc_decl") == [(:x, Base.RefValue{Int})]
    @test fieldof("tc_loop") == [(:x, Base.RefValue{Int})]
    @test fieldof("tc_undef") == [(:x, Core.Box)]
end

# the typed counter is now fully inferable (the #15276 payoff)
@test Base.return_types(getglobal(tc_mod, :tc_decl), ())[1] === Int

JuliaLowering.ACP_STRICT[] = strict_was
