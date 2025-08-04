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
@test ex ~ @ast_ [K"block"
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
    :(a.\$field_name)
end
""")
@test kind(ex[2]) == K"Identifier"
@test ex[2].name_val == "a"

# interpolations at multiple depths
ex = JuliaLowering.include_string(test_mod, """
let
    args = (:(x),:(y))
    quote
        x = 1
        y = 2
        quote
            f(\$\$(args...))
        end
    end
end
""")
@test ex ~ @ast_ [K"block"
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
ex = JuliaLowering.include_string(test_mod, """
let
    args = (:(xxx),)
    :(:(\$\$(args...)))
end
""")
Base.eval(test_mod, :(xxx = 111))
ex2 = JuliaLowering.eval(test_mod, ex)
@test kind(ex2) == K"Value"
@test ex2.value == 111

double_interp_ex = JuliaLowering.include_string(test_mod, """
let
    args = (:(x), :(y))
    :(:(\$\$(args...)))
end
""")
@test_throws LoweringError JuliaLowering.eval(test_mod, double_interp_ex)

end
