import Base.CompilerFrontend

@testset "CompilerFrontend" for frontend in [Base.FlispCompilerFrontend(),
                                             Base.DefaultCompilerFrontend(VERSION)]
    test_mod = Module()

    fe_eval(x; opts...) = CompilerFrontend.eval(frontend, test_mod, x; opts...)
    fe_include_string(x; opts...) = CompilerFrontend.include_string(frontend, test_mod, x; opts...)

    @testset "eval of basic construct" begin
        @test fe_eval(1) === 1
        @test fe_eval(Expr(:call, :+, 1, 2)) === 3
        @test fe_eval(nothing) === nothing
        @test fe_eval(LineNumberNode(1)) === nothing
    end

    @testset "eval of Expr(:toplevel)" begin
        @test fe_eval(Expr(:toplevel, 1, :(1+2))) === 3
        # Expressions evaluated sequentially in latest world
        @test fe_eval(Expr(:toplevel, 
            :(macro world_test()
                  return "blah"
            end),
            :(@world_test)
        )) == "blah"

        # Iteration continues inside macros which expand to toplevel expressions
        fe_eval(:(macro expands_to_toplevel()
                      return Expr(:toplevel, 1, 2)
                  end))
        iter = CompilerFrontend.lower_init(frontend, test_mod,
                       Expr(:macrocall, :var"@expands_to_toplevel", LineNumberNode(10,:foo)))
        @test CompilerFrontend.lower_step(iter, test_mod, Base.get_world_counter) ==
            CompilerFrontend.ToplevelExpression(1, LineNumberNode(10, :foo))
        @test CompilerFrontend.lower_step(iter, test_mod, Base.get_world_counter) ==
            CompilerFrontend.ToplevelExpression(2, LineNumberNode(10, :foo))
        @test CompilerFrontend.lower_step(iter, test_mod, Base.get_world_counter) ==
            nothing

        # edge cases for unusual top level expressions and line number nodes
        @test fe_eval(Expr(:toplevel)) === nothing
        @test fe_eval(Expr(:toplevel, LineNumberNode(1), 1)) === 1
        @test fe_eval(Expr(:toplevel, LineNumberNode(1))) === nothing
        @test fe_eval(Expr(:toplevel, LineNumberNode(1), LineNumberNode(1))) === nothing

        # The following cases are compatible with the previous eval()
        # implementation. But perhaps they are bugs - should a trailing line number
        # really evaluate the whole expression to `nothing`?
        @test fe_eval(Expr(:toplevel, 1, LineNumberNode(1))) === nothing
        @test fe_eval(Expr(:toplevel, 1, LineNumberNode(1))) === nothing
        @test fe_eval(Expr(:toplevel, 1, Expr(:toplevel))) === nothing
    end

    @testset "eval of Expr(:module)" begin
        # Module evaluation
        @test fe_eval(Expr(:module, true, :A, Expr(:block, LineNumberNode(10, "foo.jl"), :(x="x in A")))) isa Module
        @test test_mod.A isa Module
        @test isdefined(test_mod.A, :Base)
        @test test_mod.A.x == "x in A"
        @test Base.moduleloc(test_mod.A) == LineNumberNode(10, "foo.jl")

        @test fe_eval(Expr(:module, v"1.12.0", true, :ModWithVer, Expr(:block))) isa Module
        @test isdefined(test_mod.ModWithVer, :var"#compiler-frontend#")

        # baremodule
        @test fe_eval(Expr(:module, false, :B, Expr(:block))) isa Module
        @test test_mod.B isa Module
        @test !isdefined(test_mod.B, :Base)

        # malformed modules
        @test_throws TypeError fe_eval(Expr(:module, true, "not-a-symbol", Expr(:block)))
        @test_throws ErrorException fe_eval(Expr(:module, true, :NoBlockForBody, nothing))

        # Documented modules
        @test fe_include_string("""
                                "Some docs"
                                module DocumentedModule
                                end
                                """) isa Module

        @test fe_eval(Expr(:toplevel, LineNumberNode(100, "bar.jl"),
                           Expr(:module, true, :OkNestedInTopLevel, Expr(:block)))) isa Module
        # Line number inherited if necessary
        @test Base.moduleloc(test_mod.OkNestedInTopLevel) == LineNumberNode(100, "bar.jl")

        # Nested modules and module init order
        fe_eval(:(
        module InitTest
            init_order = []
            __init__() = push!(init_order, "InitTest")
            module B
                using ..InitTest
                __init__() = push!(InitTest.init_order, "B")
            end
            module C
                using ..InitTest
                __init__() = push!(InitTest.init_order, "C")
                module D
                    using ...InitTest
                    __init__() = push!(InitTest.init_order, "D")
                end
                module E
                    using ...InitTest
                    __init__() = push!(InitTest.init_order, "E")
                end
            end
        end
        ))
        InitTest = test_mod.InitTest
        @test nameof(InitTest) == :InitTest
        # @test InitTest.C.D isa Module
        @test InitTest.init_order == ["B", "D", "E", "C", "InitTest"]

        # Iteration continues inside macros which expand to module expressions
        # NB: here we don't actually construct the module, but we can still
        # iterate over its lowered content using test_mod as a dummy context
        # for lowering
        fe_eval(:(macro expands_to_module()
                      return esc(Expr(:module, true, :ExpandedFromMacro, Expr(:block, LineNumberNode(20,:moduleloc), 1)))
                  end))
        iter = CompilerFrontend.lower_init(frontend, test_mod,
                                           Expr(:macrocall, :var"@expands_to_module", LineNumberNode(10,:foo)))
        @test CompilerFrontend.lower_step(iter, test_mod, Base.get_world_counter) ==
            CompilerFrontend.BeginModule(:ExpandedFromMacro, nothing, true,
                                         LineNumberNode(20,:moduleloc))
        @test CompilerFrontend.lower_step(iter, test_mod, Base.get_world_counter) ==
            CompilerFrontend.ToplevelExpression(1, LineNumberNode(20,:moduleloc))
        @test CompilerFrontend.lower_step(iter, test_mod, Base.get_world_counter) ==
            CompilerFrontend.EndModule()
        @test CompilerFrontend.lower_step(iter, test_mod, Base.get_world_counter) ==
            nothing
    end

    @testset "mapexpr handling" begin
        function mapexpr_1(ex)
            if ex isa Expr && ex.head == :call
                "call_is_replaced"
            else
                ex
            end
        end
        @test fe_eval(:(x = 10101); mapexpr=mapexpr_1) == 10101
        @test test_mod.x == 10101
        @test fe_eval(:(f()); mapexpr=mapexpr_1) == "call_is_replaced"
        # mapexpr is applied inside toplevel expressions
        @test fe_eval(Expr(:toplevel, :(f())); mapexpr=mapexpr_1) == "call_is_replaced"
        # mapexpr not applied at top level inside modules (should it be?)
        fe_eval(:(module C
                      function f()
                          global x = 10
                      end
                      f()
                  end); mapexpr=mapexpr_1)
        @test isdefined(test_mod.C, :x)
    end

    @testset "include_string" begin
        @test fe_include_string("1 + 2") === 3
        err_type = frontend isa Base.FlispCompilerFrontend ? Meta.ParseError : Base.JuliaSyntax.ParseError
        @test_throws err_type fe_include_string("error + ")
        # If there's any error, bail out immediately
        @test_throws err_type fe_include_string("var_before_error = 10\n error + ")
        @test !isdefined(test_mod, :var_before_error)
    end
end
