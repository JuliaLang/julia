# UnifiedBackend corpus: lower real functions directly to UnifiedIR
# (lower_to_ir), verify L1, and differentially execute the reference
# interpreter against natively-defined behavior. Closure-containing bodies
# are first-class corpus members: the enclosing methods embed the
# closure-conversion decisions (value captures, Core.Box, typed RefValue
# containers) and the closure methods themselves lower as ordinary methods.

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
            inc = () -> (x = x + 1)   # typed container (RefValue{Int})
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
    @info "UnifiedBackend corpus" nlowered nexecuted
    @test nlowered >= 20     # closure bodies lower too (methods per source)
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
