@testset "Syntax quoting & interpolation" begin

test_mod = Module()

ex = JuliaLowering.include_string(test_mod, """
begin
    x = 10
    y = :(g(z))
    quote
        f(\$(x+1), \$y)
    end
end
""")
@test ex ≈ @ast_ [K"block"
    [K"call"
        "f"::K"Identifier"
        11::K"Value"
        [K"call"
            "g"::K"Identifier"
            "z"::K"Identifier"
        ]
    ]
]
@test sourcetext(ex[1]) == "f(\$(x+1), \$y)"
@test sourcetext(ex[1][2]) == "\$(x+1)"
@test sourcetext.(flattened_provenance(ex[1][3])) == ["\$y", "g(z)"]
@test sprint(io->showprov(io, ex[1][3], tree=true)) == raw"""
    (call g z)
    ├─ (call g z)
    │  └─ @ string:3
    └─ ($ y)
       └─ @ string:5
    """
@test sprint(io->showprov(io, ex[1][3])) == raw"""
    begin
        x = 10
        y = :(g(z))
    #         └──┘ ── in source
        quote
            f($(x+1), $y)
    # @ string:3

        y = :(g(z))
        quote
            f($(x+1), $y)
    #                 └┘ ── interpolated here
        end
    end
    # @ string:5"""
@test sprint(io->showprov(io, ex[1][3]; note="foo")) == raw"""
    begin
        x = 10
        y = :(g(z))
    #         └──┘ ── foo
        quote
            f($(x+1), $y)
    # @ string:3

        y = :(g(z))
        quote
            f($(x+1), $y)
    #                 └┘ ── foo
        end
    end
    # @ string:5"""


# Test expression flags are preserved during interpolation
@test JuliaSyntax.is_infix_op_call(JuliaLowering.include_string(test_mod, """
let
    x = 1
    :(\$x + \$x)
end
"""))

# Test that trivial interpolation without any nesting works.
ex = JuliaLowering.include_string(test_mod, """
let
    x = 123
    :(\$x)
end
""")
@test kind(ex) == K"Value"
@test ex.value == 123

# Test that interpolation with field access works
# (the field name can be interpolated into
ex = JuliaLowering.include_string(test_mod, """
let
    field_name = :(a)
    :(x.\$field_name)
end
""")
@test kind(ex[2]) == K"Identifier"
@test ex[2].name_val == "a"

# Test quoted property access syntax like `Core.:(foo)` and `Core.:(!==)`
@test JuliaLowering.include_string(test_mod, """
    x = (a=1, b=2)
    x.:(a)
""") == 1
@test JuliaLowering.include_string(test_mod, """
    Core.:(!==)
""") === (!==)

# Test quoted operator function definitions (issue #20)
@test JuliaLowering.include_string(test_mod, """
begin
    struct Issue20
        x::Int
    end
    Base.:(==)(a::Issue20, b::Issue20) = a.x == b.x
    Issue20(1) == Issue20(1)
end
""") === true

@test JuliaLowering.include_string(test_mod, """
begin
    Base.:(<)(a::Issue20, b::Issue20) = a.x < b.x
    Issue20(1) < Issue20(2)
end
""") === true

# interpolations at multiple depths
ex = JuliaLowering.include_string(test_mod, raw"""
let
    args = (:(x),:(y))
    quote
        x = 1
        y = 2
        quote
            f($$(args...))
        end
    end
end
""")
@test ex ≈ @ast_ [K"block"
    [K"="
        "x"::K"Identifier"
        1::K"Integer"
    ]
    [K"="
        "y"::K"Identifier"
        2::K"Integer"
    ]
    [K"quote"
        [K"block"
            [K"call"
                "f"::K"Identifier"
                [K"$"
                    "x"::K"Identifier"
                    "y"::K"Identifier"
                ]
            ]
        ]
    ]
]
@test sourcetext(ex[3][1][1][2]) == "\$\$(args...)"
@test sourcetext(ex[3][1][1][2][1]) == "x"
@test sourcetext(ex[3][1][1][2][2]) == "y"

ex2 = JuliaLowering.eval(test_mod, ex)
@test sourcetext(ex2[1][2]) == "x"
@test sourcetext(ex2[1][3]) == "y"

@test JuliaLowering.include_string(test_mod, ":x") isa Symbol
@test JuliaLowering.include_string(test_mod, ":(x)") isa SyntaxTree

# Double interpolation
double_interp_ex = JuliaLowering.include_string(test_mod, raw"""
let
    args = (:(xxx),)
    :(:($$(args...)))
end
""")
Base.eval(test_mod, :(xxx = 111))
dinterp_eval = JuliaLowering.eval(test_mod, double_interp_ex)
@test kind(dinterp_eval) == K"Value"
@test dinterp_eval.value == 111

multi_interp_ex = JuliaLowering.include_string(test_mod, raw"""
let
    args = (:(x), :(y))
    :(:($$(args...)))
end
""")
@test_throws LoweringError JuliaLowering.eval(test_mod, multi_interp_ex)

# Interpolation of SyntaxTree Identifier vs plain Symbol
symbol_interp = JuliaLowering.include_string(test_mod, raw"""
let
    x = :xx    # Plain Symbol
    y = :(yy)  # SyntaxTree K"Identifier"
    :(f($x, $y, z))
end
""")
@test symbol_interp ≈ @ast_ [K"call"
    "f"::K"Identifier"
    "xx"::K"Identifier"
    "yy"::K"Identifier"
    "z"::K"Identifier"
]
@test sourcetext(symbol_interp[2]) == "\$x" # No provenance for plain Symbol
@test sourcetext(symbol_interp[3]) == "yy"

# Mixing Expr into a SyntaxTree doesn't graft it onto the SyntaxTree AST but
# treats it as a plain old value. (This is the conservative API choice and also
# encourages ASTs to be written in the new form. However we may choose to
# change this if necessary for compatibility.)
expr_interp_is_value = JuliaLowering.include_string(test_mod, raw"""
let
    x = Expr(:call, :f, :x)
    :(g($x))
end
""")
@test expr_interp_is_value ≈ @ast_ [K"call"
    "g"::K"Identifier"
    Expr(:call, :f, :x)::K"Value"
    # ^^ NB not [K"call" "f"::K"Identifier" "x"::K"Identifier"]
]
@test Expr(expr_interp_is_value) == Expr(:call, :g, QuoteNode(Expr(:call, :f, :x)))

@testset "Interpolation in Expr compat mode" begin
    expr_interp = JuliaLowering.include_string(test_mod, raw"""
    let
        x = :xx
        :(f($x, z))
    end
    """, expr_compat_mode=true)
    @test expr_interp == Expr(:call, :f, :xx, :z)

    double_interp_expr = JuliaLowering.include_string(test_mod, raw"""
    let
        x = :xx
        :(:(f($$x, $y)))
    end
    """, expr_compat_mode=true)
    @test double_interp_expr == Expr(:quote, Expr(:call, :f, Expr(:$, :xx), Expr(:$, :y)))

    # Test that ASTs are copied before they're seen by the user
    @test JuliaLowering.include_string(test_mod, raw"""
    exs = []
    for i = 1:2
        push!(exs, :(f(x,y)))
        push!(exs[end].args, :z)
    end
    exs
    """, expr_compat_mode=true) == Any[Expr(:call, :f, :x, :y, :z), Expr(:call, :f, :x, :y, :z)]
end

end
