const JL = JuliaLowering

@testset "hooks" begin
    test_mod = Module()

    @testset "`core_lowering_hook`" begin
        # Non-AST types are often sent through lowering
        stuff = Any[LineNumberNode(1), 123, 123.123, true, "foo", test_mod]
        for s in stuff
            @test JL.core_lowering_hook(s, test_mod) == Core.svec(s)
        end

        for ast_type in (Expr, JL.SyntaxTree)
            ex = parsestmt(ast_type, "[1,2,3] .+= 1")
            out = JL.core_lowering_hook(ex, test_mod)
            @test out isa Core.SimpleVector && out[1] isa Expr
            val = Core.eval(test_mod, out[1])
            @test val == [2,3,4]
        end

        # file argument mismatch with embedded linenumbernodes shouldn't crash
        ex = Expr(:block, LineNumberNode(111), :(x = 1), LineNumberNode(222), :(x + 1))
        lwr = JuliaLowering.core_lowering_hook(ex, test_mod, "foo.jl", 333)[1]
        @test Core.eval(test_mod, lwr) === 2
    end

    function jeval(str)
        prog = parseall(Expr, str)
        local out
        try
            JL.activate!()
            out = Core.eval(test_mod, prog)
        finally
            JL.activate!(false)
        end
    end
    @testset "integration: `JuliaLowering.activate!`" begin
        out = jeval("global asdf = 1")
        @test out === 1
        @test isdefined(test_mod, :asdf)

        out = jeval("module M; x = 1; end")
        @test out isa Module
        @test isdefined(test_mod, :M)
        @test isdefined(test_mod.M, :x)

        @test jeval("@ccall jl_value_ptr(nothing::Any)::Ptr{Cvoid}") isa Ptr{Cvoid}

        # Tricky cases with symbols
        out = jeval("""module M2
                Base.@constprop :aggressive function f(x); x; end
                const what = ccall(:jl_value_ptr, Ptr{Cvoid}, (Any,), Core.nothing)
            end""")
        @test out isa Module
        @test isdefined(test_mod, :M2)
        @test isdefined(test_mod.M2, :f)
        @test isdefined(test_mod.M2, :what)

        out = jeval(""" "docstring" module M3 end """)
        @test out isa Module
        @test isdefined(test_mod, :M3)

        # Macros may produce toplevel expressions.  Note that julia handles
        # this case badly (macro expansion replaces M5_inner with a
        # globalref) and we handle esc(:M5_inner) badly
        out = jeval("""module M5
            macro newmod()
                return quote
                    let a = 1
                        $(Expr(:toplevel,
                               Expr(:module, true, :M5_inner,
                                    Expr(:block, :(global asdf = 1)))))
                    end
                end
            end
            @newmod()
            end""")
        @test out isa Module
        @test isdefined(test_mod, :M5)
        @test isdefined(test_mod.M5, :M5_inner)
        @test isdefined(test_mod.M5.M5_inner, :asdf)

        @test jeval("Base.@propagate_inbounds @inline meta_double_quote_issue(x) = x") isa Function
    end

    @testset "CompilerFrontend" begin
        _Core = JuliaLowering._Core
        old_fe = _Core._set_compiler_frontend!(JuliaLowering.JuliaLoweringFrontend(false))
        try
            # Expr works with eval()
            _Core.eval(test_mod, :(xxx = 6))
            @test _Core.eval(test_mod, :(xxx / 2)) == 3

            # SyntaxTree works with eval()
            _Core.eval(test_mod, JuliaLowering.@SyntaxTree :(xxx = 8))
            @test _Core.eval(test_mod, JuliaLowering.@SyntaxTree :(xxx / 2)) == 4

        finally
            _Core._set_compiler_frontend!(old_fe)
        end
    end
end

@testset "Compiler frontend $frontend" for frontend in [JuliaLowering._Base.FlispCompilerFrontend(),
                                                        JuliaLowering._Base.DefaultCompilerFrontend()]
    test_mod = Module()

    _Core = JuliaLowering._Core
    _Base = JuliaLowering._Base
    old_fe = _Core._set_compiler_frontend!(frontend)
    try
        # Basic top level expressions
        @test _Base.include_string(test_mod,
        """
        x = 1
        y = 2

        (x,y)
        """) === (1,2)

        # Nested modules and module init order
        Amod = _Base.include_string(test_mod, """
        module A
            init_order = []
            __init__() = push!(init_order, "A")
            module B
                using ..A
                __init__() = push!(A.init_order, "B")
            end
            module C
                using ..A
                __init__() = push!(A.init_order, "C")
                module D
                    using ...A
                    __init__() = push!(A.init_order, "D")
                end
                module E
                    using ...A
                    __init__() = push!(A.init_order, "E")
                end
            end
        end
        """)
        Core.@latestworld
        @test nameof(Amod) == :A
        @test Amod.C.D isa Module
        @test Amod.init_order == ["B", "D", "E", "C", "A"]

        # Macro expansion world age
        @test _Base.include_string(test_mod,
        """
        module ModuleTopLevelEvalWorldTest
            macro mac2()
                :(101)
            end

            xx = @mac2
        end

        ModuleTopLevelEvalWorldTest.xx
        """) === 101

        # Macros expanding to `Expr(:toplevel)`
        @test _Base.include_string(test_mod,
        """
        macro expand_to_toplevel()
            esc(Expr(:toplevel,
                :(a = 1),
                :(b = 2),
                :((a, b)),
            ))
        end

        @expand_to_toplevel
        """) == (1,2)

        # Test that `mapexpr` argument to `include()` is applied
        function test_mapexpr(ex)
            if ex isa Expr && ex.head != :module
                10101
            else
                ex
            end
        end
        @test JuliaLowering.include_string(test_mapexpr, test_mod, """
        module ContentNotMapped
            x = 1  # This line won't have mapexpr applied even though it's at
                   # top level in the module.
        end

        this + expression + will + be + replaced
        """) === 10101
        Core.@latestworld
        @test test_mod.ContentNotMapped.x == 1

    finally
        _Core._set_compiler_frontend!(old_fe)
    end
end
