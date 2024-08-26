@testset "macros" begin

test_mod = Module()

JuliaLowering.include_string(test_mod, """
module M
    using JuliaLowering: JuliaLowering, @ast, @chk, adopt_scope
    using JuliaSyntax

    # Introspection
    macro __MODULE__()
        __context__.scope_layer.mod
    end

    macro __FILE__()
        JuliaLowering.filename(__context__.macroname)
    end

    macro __LINE__()
        JuliaLowering.source_location(__context__.macroname)[1]
    end

    someglobal = "global in module M"

    # Macro with local variables
    macro foo(ex)
        :(begin
            x = "`x` from @foo"
            (x, someglobal, \$ex)
        end)
    end

    # Set `a_global` in M
    macro set_a_global(val)
        :(begin
            global a_global = \$val
        end)
    end

    macro set_other_global(ex, val)
        :(begin
            global \$ex = \$val
        end)
    end

    macro set_global_in_parent(ex)
        e1 = adopt_scope(:(sym_introduced_from_M), __context__)
        quote
            \$e1 = \$ex
            nothing
        end
    end

    macro inner()
        :(2)
    end

    macro outer()
        :((1, @inner))
    end

    # # Recursive macro call
    # # TODO: Need branching!
    # macro recursive(N)
    #     Nval = N.value #::Int
    #     if Nval < 1
    #         return N
    #     end
    #     quote
    #         x = \$N
    #         (@recursive \$(Nval-1), x)
    #     end
    # end
end
""")

@test JuliaLowering.include_string(test_mod, """
let 
    x = "`x` from outer scope"
    M.@foo x
end
""") == ("`x` from @foo", "global in module M", "`x` from outer scope")
@test !isdefined(test_mod.M, :x)

@test JuliaLowering.include_string(test_mod, """
#line1
(M.@__MODULE__(), M.@__FILE__(), M.@__LINE__())
""", "foo.jl") == (test_mod, "foo.jl", 2)

@test !isdefined(test_mod.M, :a_global)
@test JuliaLowering.include_string(test_mod, """
begin 
    M.@set_a_global 42
    M.a_global
end
""") == 42

JuliaLowering.include_string(test_mod, """
M.@set_global_in_parent "bent hygiene!"
""")
@test test_mod.sym_introduced_from_M == "bent hygiene!"

JuliaLowering.include_string(test_mod, "M.@set_other_global global_in_test_mod 100")
@test !isdefined(test_mod.M, :global_in_test_mod)
@test test_mod.global_in_test_mod == 100

Base.eval(test_mod.M, :(
# Recursive macro call
function var"@recursive"(mctx, N)
    @chk kind(N) == K"Integer"
    Nval = N.value::Int
    if Nval < 1
        return N
    end
    @ast mctx (@HERE) [K"block"
        [K"="(@HERE)
            "x"::K"Identifier"(@HERE)
            N
        ]
        [K"tuple"(@HERE)
            "x"::K"Identifier"(@HERE)
            [K"macrocall"(@HERE)
                "@recursive"::K"Identifier"
                (Nval-1)::K"Integer"
            ]
        ]
    ]
end
))

@test JuliaLowering.include_string(test_mod, """
M.@recursive 3
""") == (3, (2, (1, 0)))

@test let
    ex = parsestmt(SyntaxTree, "M.@outer()", filename="foo.jl")
    expanded = JuliaLowering.macroexpand(test_mod, ex)
    sourcetext.(flattened_provenance(expanded[2]))
end == [
    "M.@outer()"
    "@inner"
    "2"
]


@test_throws LoweringError JuliaLowering.include_string(test_mod, """
macro mmm(a; b=2)
end
""")

@test_throws LoweringError JuliaLowering.include_string(test_mod, """
macro A.b(ex)
end
""")

# Macros not allowed in local scope
@test_throws LoweringError JuliaLowering.include_string(test_mod, """
let
    macro foo(ex)
    end
end
""")
@test_throws LoweringError JuliaLowering.include_string(test_mod, """
function f()
    macro foo()
    end
end
""")

end
