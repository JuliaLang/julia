# UnifiedBackend corpus: lower real functions directly to UnifiedIR
# (lower_to_ir), verify L1, and differentially execute the reference
# interpreter against natively-defined behavior. Closure-containing bodies
# are first-class corpus members: the enclosing methods embed the
# closure-conversion decisions (value captures, Core.Box shares) and the
# closure methods themselves lower as ordinary methods.

using JuliaSyntax: UnifiedIR
const UB = JuliaLowering.UnifiedBackend

# native definitions live here; lower_to_ir reuses the same module so closure
# types resolve during interpretation
ub_mod = Module()
Base.eval(ub_mod, :(using Base))

# (name-to-execute, source, [args-tuples] or nothing)
UB_CORPUS = [
    (:c1, "c1(x) = x + 1", [(1,), (-2,)]),
    (:c2, """
        function c2(n)
            s = 0
            i = 1
            while i <= n
                s += i
                i += 1
            end
            s
        end""", [(10,), (0,)]),
    (:c3, "c3(x) = x < 0 ? -x : x", [(5,), (-7,)]),
    (:c4, """
        function c4(x, n)
            r = 1
            for _ in 1:n
                r *= x
            end
            r
        end""", [(2, 10), (3, 0)]),
    (:c5, "c5(a, b) = (a + b, a - b)", [(3, 4)]),
    (:c6, """
        function c6(c)
            local y
            if c
                y = 1
            end
            y
        end""", nothing),   # undef path checked separately below
    (:c7, """
        function c7(x)
            try
                x + 1
            finally
                x * 2
            end
        end""", [(5,)]),
    (:c8, "c8(t) = +(t...)", [((1, 2, 3),)]),
    (:c9, """
        function c9(x)
            s = "hi"
            x > 0 ? s : "lo"
        end""", [(1,), (-1,)]),
    (:c10, "c10(x)::Int = x + 1", [(5,)]),
    # ---- closures ---------------------------------------------------------
    (:cl1, """
        function cl1(c)
            local x
            if c; x = 1; else; x = 2; end
            f = () -> x            # value capture (join)
            f()
        end""", [(true,), (false,)]),
    (:cl2, """
        function cl2()
            x = 1
            f = () -> x            # Core.Box: mutated after capture
            x = 2
            f()
        end""", [()]),
    (:cl3, """
        function cl3()
            local x::Int = 0
            inc = () -> (x = x + 1)   # shared: closure writes its capture
            inc(); inc()
            x
        end""", [()]),
    (:cl4, "cl4(n) = [i * i for i in 1:n]", [(3,)]),
    (:cl5, """
        function cl5(v)
            s = 0
            foreach(v) do x
                s += x             # do-block writing its capture
            end
            s
        end""", [([1, 2, 3],)]),
]

# `lower_to_ir` re-runs closure conversion, which reserves FRESH closure type
# names (the native `include_string` run already claimed the first ones), but
# does not execute the toplevel thunk that would create those types. For the
# interpretation differential, alias each missing closure-type global to the
# natively-created one (same source, same field layout).
function alias_closure_types!(mod::Module, ms)
    for m in ms
        gl = m.ir.body.globals
        for g in gl
            g.mod === mod || continue
            isdefined(mod, g.name) && continue
            base = replace(String(g.name), r"[0-9]+$" => "")
            startswith(base, "#") || continue
            for cand in names(mod; all = true)
                sc = String(cand)
                (startswith(sc, base) && occursin(r"^[0-9]+$", sc[length(base)+1:end]) &&
                 isdefined(mod, cand)) || continue
                v = getglobal(mod, cand)
                v isa Type || continue
                Core.eval(mod, :(const $(g.name) = $cand))
                break
            end
        end
    end
    return ms
end

@testset "corpus: lower + verify + interpret differential" begin
    nlowered = 0
    nexecuted = 0
    UB.reset_path_counts!()
    for (name, src, exec) in UB_CORPUS
        @testset "$name" begin
            # native definition first: creates closure types/methods the
            # interpreter needs when running the enclosing bodies
            JuliaLowering.include_string(ub_mod, src)
            ms = alias_closure_types!(ub_mod, UB.lower_to_ir(ub_mod, src))
            i = findfirst(m -> m.name == name, ms)
            @test i !== nothing
            m = ms[i]
            @test UnifiedIR.verify_ir(m.ir; level = 1)
            @test length(m.slotnames) == m.nargs
            nlowered += length(ms)
            if exec !== nothing
                f = Base.invokelatest(getglobal, ub_mod, name)
                for args in exec
                    expected = Base.invokelatest(f, args...)
                    got = Base.invokelatest(UnifiedIR.interpret, m.ir, f, args...)
                    @test isequal(got, expected)
                    nexecuted += 1
                end
            end
        end
    end
    @info "UnifiedBackend corpus" nlowered nexecuted UB.REGION_STMTS[] UB.EAGER_STMTS[]
    @test nlowered >= 20     # closure bodies lower too (methods per source)
    # the whole corpus (closures included) lowers through the region path
    @test UB.EAGER_STMTS[] == 0
end

# ---------------------------------------------------------------------------
# The closure-REGION path: sources lower with real `closure` ops, the
# fixpoint decides captures structurally, and materialization produces
# self-contained closure types whose instances interpret the extracted
# method IR (a trampoline method), so the differential crosses the call
# boundary inside UnifiedIR on both frames.
# ---------------------------------------------------------------------------

count_globals(ir, mod, name) = count(g -> g.mod === mod && g.name === name,
                                     ir.body.globals)
count_boxes(ir) = count_globals(ir, Core, :Box)

RG_CORPUS = [
    # (name, src, execs, expect: :value = no shared container in the
    #  enclosing IR, :shared = at least one, nothing = don't check)
    (:rg_join, """
        function rg_join(c)
            local x
            if c; x = 1; else; x = 2; end
            g = () -> x + 1
            g()
        end""", [(true,), (false,)], :value),
    (:rg_try, """
        function rg_try(a)
            local x
            try
                x = sqrt(a)
            catch
                x = -1.0
            end
            cl = () -> x
            cl()
        end""", [(4.0,), (-4.0,)], :value),
    (:rg_after, """
        function rg_after()
            x = 1
            f = () -> x
            x = 2
            f()
        end""", [()], :shared),
    (:rg_counter, """
        function rg_counter()
            x = 0
            inc = () -> (x = x + 1)
            inc(); inc(); inc()
            x
        end""", [()], :shared),
    (:rg_typed, """
        function rg_typed()
            local x::Int = 0
            inc = () -> (x = x + 1)
            inc(); inc()
            x
        end""", [()], :shared),
    (:rg_loopfresh, """
        function rg_loopfresh(n)
            s = 0
            for i in 1:n
                x = i
                g = () -> x * 10
                s += g()
            end
            s
        end""", [(3,), (0,)], :value),
    (:rg_multishot, """
        function rg_multishot(n)
            fs = Any[]
            x = 0
            for i in 1:n
                push!(fs, () -> x)
                x = i
            end
            sum(Int[f() for f in fs]; init = 0)
        end""", [(3,)], :shared),
    (:rg_nested, """
        function rg_nested(a)
            g = () -> (() -> a + 1)
            h = g()
            h()
        end""", [(41,)], :value),
    (:rg_rec, """
        function rg_rec(n)
            fact(k) = k <= 1 ? 1 : k * fact(k - 1)
            fact(n)
        end""", [(5,), (1,)], :shared),
    (:rg_arg, """
        function rg_arg(x)
            x = x + 1
            g = () -> x
            g()
        end""", [(41,)], :value),
    (:rg_arg2, """
        function rg_arg2(x)
            g = () -> x
            x = x + 1
            g()
        end""", [(41,)], :shared),
    (:rg_manyfields, """
        function rg_manyfields(a, b)
            u = a + 1
            v = b * 2
            w = 0
            f = () -> (w = u + v; w + 1)
            r = f()
            r + w
        end""", [(1, 2)], nothing),
]

@testset "closure-region path corpus" begin
    UB.reset_path_counts!()
    for (name, src, execs, expect) in RG_CORPUS
        @testset "$name" begin
            JuliaLowering.include_string(ub_mod, src)
            ms = UB.lower_to_ir(ub_mod, src)
            i = findfirst(m -> m.name == name, ms)
            @test i !== nothing
            m = ms[i]
            @test length(ms) >= 2         # the closure method was extracted
            @test UnifiedIR.verify_ir(m.ir; level = 1)
            # no residual closure ops or cells of either class survive
            # materialization in the enclosing method
            for s in UnifiedIR.each_stmt(m.ir)
                @test UnifiedIR.stmt_kind(m.ir, s) !== UnifiedIR.K"closure"
                @test UnifiedIR.stmt_kind(m.ir, s) !== UnifiedIR.K"cell_shared"
            end
            if expect === :value
                @test count_boxes(m.ir) == 0
                @test count_globals(m.ir, Core, :setfield!) == 0
            elseif expect === :shared
                @test count_boxes(m.ir) > 0
            end
            f = Base.invokelatest(getglobal, ub_mod, name)
            for args in execs
                expected = Base.invokelatest(f, args...)
                got = Base.invokelatest(UnifiedIR.interpret, m.ir, f, args...)
                @test isequal(got, expected)
            end
        end
    end
    @test UB.EAGER_STMTS[] == 0           # every case took the region path
end

@testset "region-path undef semantics (zoo6)" begin
    src = """
        function rg_undef(c)
            local x
            if c; x = 1; end
            f = () -> x
            f
        end"""
    JuliaLowering.include_string(ub_mod, src)
    UB.reset_path_counts!()
    ms = UB.lower_to_ir(ub_mod, src)
    @test UB.EAGER_STMTS[] == 0
    m = ms[findfirst(m -> m.name == :rg_undef, ms)]
    @test count_boxes(m.ir) > 0           # maybe-undef MUST keep Core.Box
    f = Base.invokelatest(getglobal, ub_mod, :rg_undef)
    # creating the closure never throws; the UndefVarError is use-time, with
    # the right variable name
    cl = Base.invokelatest(UnifiedIR.interpret, m.ir, f, false)
    err = try
        Base.invokelatest(cl)
        nothing
    catch e
        e
    end
    @test err isa UndefVarError && err.var === :x
    cl2 = Base.invokelatest(UnifiedIR.interpret, m.ir, f, true)
    @test Base.invokelatest(cl2) == 1
end

@testset "v1 bails take the eager path (fidelity, not mis-lowering)" begin
    for (name, src, execs) in [
        # bl_kw: no execution differential — the kw BODY function name embeds
        # its own reservation counter, which the fresh-name aliasing cannot
        # match to the natively-created type (a pre-existing limitation of
        # the eager-path harness, not of the lowering)
        (:bl_kw, """
            function bl_kw()
                g(x; k = 2) = x + k
                g(1)
            end""", Tuple{}[]),
        (:bl_mm, """
            function bl_mm()
                g(x::Int) = 1
                g(x::String) = 2
                g(3) + g("s")
            end""", [()]),
        (:bl_sp, """
            function bl_sp()
                g(x::T) where {T} = T
                g(1)
            end""", [()]),
        (:bl_va, """
            function bl_va()
                g(xs...) = length(xs)
                g(1, 2, 3)
            end""", [()]),
        (:bl_rt, """
            function bl_rt()
                g()::Int = 41.0 + 1
                g()
            end""", [()]),
    ]
        @testset "$name" begin
            JuliaLowering.include_string(ub_mod, src)
            UB.reset_path_counts!()
            ms = alias_closure_types!(ub_mod, UB.lower_to_ir(ub_mod, src))
            @test UB.EAGER_STMTS[] == 1 && UB.REGION_STMTS[] == 0
            i = findfirst(m -> m.name == name, ms)
            @test i !== nothing
            m = ms[i]
            @test UnifiedIR.verify_ir(m.ir; level = 1)
            f = Base.invokelatest(getglobal, ub_mod, name)
            for args in execs
                expected = Base.invokelatest(f, args...)
                got = Base.invokelatest(UnifiedIR.interpret, m.ir, f, args...)
                @test isequal(got, expected)
            end
        end
    end
end

@testset "undef semantics through the backend" begin
    ms = UB.lower_to_ir(ub_mod, UB_CORPUS[6][2])
    m = ms[findfirst(m -> m.name == :c6, ms)]
    f = Base.invokelatest(getglobal, ub_mod, :c6)
    @test Base.invokelatest(UnifiedIR.interpret, m.ir, f, true) == 1
    @test_throws UndefVarError Base.invokelatest(UnifiedIR.interpret, m.ir, f, false)
end

@testset "closure bodies lower as ordinary methods" begin
    src = """
        function clbody(c)
            local x
            if c; x = 1.5; else; x = 2.5; end
            g = () -> x + 1
            g()
        end"""
    JuliaLowering.include_string(ub_mod, src)
    ms = alias_closure_types!(ub_mod, UB.lower_to_ir(ub_mod, src))
    @test length(ms) == 2                # the closure method and clbody
    outer = ms[findfirst(m -> m.name == :clbody, ms)]
    f = Base.invokelatest(getglobal, ub_mod, :clbody)
    @test Base.invokelatest(UnifiedIR.interpret, outer.ir, f, true) == 2.5
    @test Base.invokelatest(UnifiedIR.interpret, outer.ir, f, false) == 3.5
    # the value capture decision is visible in the enclosing IR: no Core.Box
    hasbox = any(1:UnifiedIR.nstmts(outer.ir)) do i
        s = UnifiedIR.StmtId(Int32(i))
        any(1:UnifiedIR.nops(outer.ir, s)) do j
            o = UnifiedIR.getop(outer.ir, s, j)
            UnifiedIR.optag(o) == UnifiedIR.TAG_GLOBAL &&
                (g = outer.ir.body.globals[UnifiedIR.payload(o)];
                 g.mod === Core && g.name === :Box)
        end
    end
    @test !hasbox
end
