test_mod = Module()

@eval test_mod import JuliaLowering.@legacy_quote_to_syntax

@testset "basic quoting and dollar-interpolation" begin
    @eval test_mod global interpolated_var

    @testset for run in [
        (x::String)->fl_eval(test_mod, Expr(:block, JuliaSyntax.parsestmt(Expr, x))),
        (x::String)->jl_eval(test_mod, JuliaSyntax.parsestmt(SyntaxTree, x); expr_compat_mode=true),
        (x::String)->jl_eval(test_mod, JuliaSyntax.parsestmt(SyntaxTree, x); expr_compat_mode=false),
        ]
        @test run(raw":x") == :x
        @test run(raw":(:x)") == QuoteNode(:x)
        @test run(raw":(:(:x))") == Expr(:quote, (QuoteNode(:x)))
        @test run(raw":(:($x))") == Expr(:quote, Expr(:$, :x))
        @test run(raw":($(:($(:x))))") == :x
        @test run(raw":($(:(:($x))))") == Expr(:quote, Expr(:$, :x))
        @test run(raw":(:($(:($x))))") == Expr(:quote, Expr(:$, Expr(:quote, Expr(:$, :x))))

        @testset for ivar_val in [:y, Symbol(""), GlobalRef(Base, :push!), Expr(:call, :identity, 2), 1, nothing],
            ivar in [ivar_val, Expr(:quote, ivar_val), Expr(:inert, ivar_val), QuoteNode(ivar_val)]

            Base.setglobal!(test_mod, :interpolated_var, ivar)

            @test run(raw"interpolated_var") == ivar
            @test run(raw":($interpolated_var)") == ivar
            @test run(raw":($(:($interpolated_var)))") == ivar
            @test run(raw":(:($$interpolated_var))") == Expr(:quote, Expr(:$, ivar))
            @test run(raw":(:($($interpolated_var)))") == Expr(:quote, Expr(:$, ivar))
            @test run(raw":(identity($interpolated_var))") == Expr(:call, :identity, ivar)
        end
    end
end

@testset "self-quoting forms" for
    form in [1, true, "string", [], nothing,],
    quoted in [Expr(:quote, form), Expr(:inert, form), QuoteNode(form)]

    @test fl_eval(test_mod, Expr(:block, quoted)) == form
    @test jl_eval(test_mod, Expr(:block, quoted); expr_compat_mode=true) == form
    @test jl_eval(test_mod, Expr(:block, quoted); expr_compat_mode=false) == form
end
@testset "self-quoting forms, interpolated into quote" for
    form in [1, true, "string", [], nothing,],
    quoted in [form, Expr(:quote, form), Expr(:inert, form), QuoteNode(form)]

    @test fl_eval(test_mod, Expr(:quote, Expr(:$, quoted))) == form
    @test jl_eval(test_mod, Expr(:quote, Expr(:$, quoted)); expr_compat_mode=true) == form
    @test jl_eval(test_mod, Expr(:quote, Expr(:$, quoted)); expr_compat_mode=false) == form
end

@eval test_mod global quotesplatvar = [1,[2,[3,[4]]]]
@testset "unquote-splicing `...`" for run in [
    (x)->fl_eval(test_mod, x),
    (x)->jl_eval(test_mod, x; expr_compat_mode=true),
    (x)->jl_eval(test_mod, x; expr_compat_mode=false),
    ]
    @test expr_structure_eq(
        run(
            Expr(:quote,
                 Expr(:call, Base.vect,
                      Expr(:$,
                           Expr(:..., :quotesplatvar))))),
        Expr(:call, Base.vect, 1, [2, [3, [4]]]))
    @test expr_structure_eq(
        run(
            Expr(:quote,
                 Expr(:quote,
                      Expr(:$,
                           Expr(:...,  # A quoted `...` is left unchanged
                                Expr(:$,
                                     Expr(:..., :quotesplatvar))))))),
        Expr(:quote, Expr(:$, Expr(:..., 1, [2, [3, [4]]]))))
    @test expr_structure_eq(
        run(
            Expr(:quote,
                 Expr(:quote,
                      Expr(:$,
                           Expr(:$,
                                Expr(:...,
                                     Expr(:..., :quotesplatvar))))))),
        Expr(:quote, Expr(:$, 1, 2, [3, [4]])))
    @test expr_structure_eq(
        run(
            Expr(:quote,
                 Expr(:quote,
                      Expr(:$,
                           Expr(:$,
                                Expr(:...,
                                     Expr(:...,
                                          Expr(:..., :quotesplatvar)))))))),
        Expr(:quote, Expr(:$, 1, 2, 3, [4])))
end

@testset "@legacy_quote_to_syntax" begin
    @test JuliaLowering.include_string(
        test_mod, raw"@legacy_quote_to_syntax :x") isa SyntaxTree
    @test JuliaLowering.include_string(
        test_mod, raw"@legacy_quote_to_syntax :x") |> kind === K"Identifier"
    @test JuliaLowering.include_string(
        test_mod, raw"@legacy_quote_to_syntax :($1)") isa SyntaxTree
    @test JuliaLowering.include_string(
        test_mod, raw"@legacy_quote_to_syntax :($1)") |> kind === K"Value"

    @test JuliaLowering.include_string(
        test_mod, raw"@legacy_quote_to_syntax :(x+1)") isa SyntaxTree
    @test JuliaLowering.include_string(
        test_mod, raw"@legacy_quote_to_syntax :(x+1)") |> kind === K"call"

    # compat mode makes standard quote
    @test JuliaLowering.include_string(
        test_mod, raw"@legacy_quote_to_syntax :x"; expr_compat_mode=true) == :x
    @test JuliaLowering.include_string(
        test_mod, raw"@legacy_quote_to_syntax :(x+1)"; expr_compat_mode=true) ==
            Expr(:call, :+, :x, 1)

    # syntaxunquote does not support the equivalent of Expr(:$, :a, :b), but
    # legacy_quote_to_syntax can convert it
    @test expr_structure_eq(
        jl_eval(
            test_mod,
            Expr(:let, Expr(:block, Expr(:(=), :a, 1), Expr(:(=), :b, [2, [3, [4]]])),
                 Expr(:macrocall,
                      Symbol("@legacy_quote_to_syntax"),
                      LineNumberNode(1),
                      Expr(:quote,
                           Expr(:call, Base.vect,
                                Expr(:$, :a, :b)))))
            ; expr_compat_mode=true),
        Expr(:call, Base.vect, 1, [2, [3, [4]]]))
    # splat :b
    @test expr_structure_eq(
        jl_eval(
            test_mod,
            Expr(:let, Expr(:block, Expr(:(=), :a, 1), Expr(:(=), :b, [2, [3, [4]]])),
                 Expr(:macrocall,
                      Symbol("@legacy_quote_to_syntax"),
                      LineNumberNode(1),
                      Expr(:quote,
                           Expr(:call, Base.vect,
                                Expr(:$, :a, Expr(:..., :b))))))
            ; expr_compat_mode=true),
        Expr(:call, Base.vect, 1, 2, [3, [4]]))
    # double-splat :b
    @test expr_structure_eq(
        jl_eval(
            test_mod,
            Expr(:let, Expr(:block, Expr(:(=), :a, 1), Expr(:(=), :b, [2, [3, [4]]])),
                 Expr(:macrocall,
                      Symbol("@legacy_quote_to_syntax"),
                      LineNumberNode(1),
                      Expr(:quote,
                           Expr(:call, Base.vect,
                                Expr(:$, :a, Expr(:..., Expr(:..., :b)))))))
            ; expr_compat_mode=true),
        Expr(:call, Base.vect, 1, 2, 3, [4]))

    # with compat=false
    st = jl_eval(
        test_mod,
        Expr(:let, Expr(:block, Expr(:(=), :a, 1), Expr(:(=), :b, [2, [3, [4]]])),
             Expr(:macrocall,
                  Symbol("@legacy_quote_to_syntax"),
                  LineNumberNode(1),
                  Expr(:quote,
                       Expr(:call, Base.vect,
                            Expr(:$, :a, :b)))))
        ; expr_compat_mode=false)
    @test st isa SyntaxTree
    @test JuliaSyntax.numchildren(st) == 3
    st = jl_eval(
        test_mod,
        Expr(:let, Expr(:block, Expr(:(=), :a, 1), Expr(:(=), :b, [2, [3, [4]]])),
             Expr(:macrocall,
                  Symbol("@legacy_quote_to_syntax"),
                  LineNumberNode(1),
                  Expr(:quote,
                       Expr(:call, Base.vect,
                            Expr(:$, :a, Expr(:..., :b))))))
        ; expr_compat_mode=false)
    @test st isa SyntaxTree
    @test JuliaSyntax.numchildren(st) == 4
    st = jl_eval(
        test_mod,
        Expr(:let, Expr(:block, Expr(:(=), :a, 1), Expr(:(=), :b, [2, [3, [4]]])),
             Expr(:macrocall,
                  Symbol("@legacy_quote_to_syntax"),
                  LineNumberNode(1),
                  Expr(:quote,
                       Expr(:call, Base.vect,
                            Expr(:$, :a, Expr(:..., Expr(:..., :b)))))))
        ; expr_compat_mode=false)
    @test st isa SyntaxTree
    @test JuliaSyntax.numchildren(st) == 5
end

ex = JuliaLowering.include_string(test_mod, """
begin
    x = 10
    y = @legacy_quote_to_syntax :(g(z))
    @legacy_quote_to_syntax quote
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

# Test that interpolation with field access works
# (the field name can be interpolated after the dot).
@test JuliaLowering.include_string(test_mod, """
let
    field_name = @legacy_quote_to_syntax :(a)
    @legacy_quote_to_syntax :(x.\$field_name)
end
""") ≈ @ast_ [K"." "x"::K"Identifier" [K"inert" "a"::K"Identifier"]]
@test JuliaLowering.include_string(test_mod, """
let
    field_name = @legacy_quote_to_syntax :(a)
    @legacy_quote_to_syntax :(x.\$field_name)
end
"""; expr_compat_mode=true) == Expr(:., :x, QuoteNode(:a))

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
    args = (:(x,x),:(y,y))
    quote
        x = 1
        y = 2
        quote
            f($$(args...))
        end
    end
end
""")
@test Base.remove_linenums!(ex) ==
    Expr(:block,
         Expr(:(=), :x, 1),
         Expr(:(=), :y, 2),
         Expr(:quote,
              Expr(:block,
                   Expr(:call, :f, Expr(:$, Expr(:tuple, :x, :x),
                                        Expr(:tuple, :y, :y))))))

# Double interpolation
double_interp_ex = JuliaLowering.include_string(test_mod, raw"""
let
    args = (:(xxx),)
    :(:($$(args...)))
end
""")
Base.eval(test_mod, :(xxx = 111))
dinterp_eval = JuliaLowering.eval(test_mod, double_interp_ex)
@test dinterp_eval == 111

multi_interp_ex = JuliaLowering.include_string(test_mod, raw"""
let
    args = (:(x), :(y))
    :(:($$(args...)))
end
""")

err = try
    JuliaLowering.eval(test_mod, multi_interp_ex)
    nothing
catch exc
    @test exc isa LoweringError
    sprint(io->Base.showerror(io, exc, show_detail=false))
end
@test contains(err, raw"More than one value in bare `$` expression")

err = try
    JuliaLowering.eval(test_mod, multi_interp_ex, expr_compat_mode=true)
    nothing
catch exc
    @test exc isa LoweringError
    sprint(io->Base.showerror(io, exc, show_detail=false))
end
@test contains(err, raw"More than one value in bare `$` expression")

# Symbol should be interpolated (converted from expr)
@eval test_mod using JuliaLowering
symbol_interp = JuliaLowering.include_string(test_mod, """
let
    x = :xx
    y = @legacy_quote_to_syntax :yy
    @legacy_quote_to_syntax :(f(\$x, \$y, z))
end
""")
@test symbol_interp ≈ @ast_ [K"call"
    "f"::K"Identifier"
    "xx"::K"Identifier"
    "yy"::K"Identifier"
    "z"::K"Identifier"
]
@test sourcetext(symbol_interp[2]) == raw"$x"
@test sourcetext(symbol_interp[3]) == "yy"

# (may change) Expr interpolated into SyntaxTree
@test_throws LoweringError JuliaLowering.include_string(test_mod, raw"""
let
    x = Expr(:call, :f, :x)
    @legacy_quote_to_syntax :(g($x))
end
""") broken=true

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

    # Test interpolation into QuoteNode
    @test JuliaLowering.include_string(test_mod, raw"""
    let x = :push!
        @eval Base.$x
    end
    """; expr_compat_mode=true) == Base.push!
end

# (. l r) should pass lowering only when r is one of:
# - simple identifier (resolved variable)
# - any simple atom, bare, inert, or in quote
# - anything else if inert (not evaluated)
# - any valid `r` wrapped in unquote, then quote
#
# note Expr(:block) is to avoid the special top-level evaluation of Expr(:.) in
# flisp, which skips handling :quote
@eval test_mod begin
    struct GetProperty; gs_field; end
    Base.getproperty(::GetProperty, x) = ("got", x)
    Base.getproperty(::GetProperty, x::Symbol) = ("got", x) # avoid ambiguity

    global gs = GetProperty([])
    global outer_field = :gs_field
end
@testset "getproperty quoting" for wrap_quote in [
    identity,
    x->Expr(:quote, Expr(:$, x)),
    x->Expr(:quote, Expr(:$, Expr(:quote, Expr(:$, x))))]

    @testset "arg2 unquoted identifier" for expr_compat_mode in [true, false]
        local field = :outer_field

        @test fl_eval(test_mod, Expr(:block, Expr(:., test_mod.gs, field))) ==
            ("got", :gs_field)
        @test jl_eval(test_mod, Expr(:block, Expr(:., test_mod.gs, field));
                      expr_compat_mode) ==
            ("got", :gs_field)
    end
    @testset "arg2 quoted identifier" for expr_compat_mode in [true, false],
        field in [Expr(:quote, :gs_field),
                  Expr(:inert, :gs_field),
                  QuoteNode(:gs_field)]

        @test fl_eval(test_mod, Expr(:block, Expr(:., test_mod.gs, field))) ==
            ("got", :gs_field)
        @test jl_eval(test_mod, Expr(:block, Expr(:., test_mod.gs, field));
                      expr_compat_mode) ==
            ("got", :gs_field)
    end
    @testset "arg2 maybe-quoted non-identifier atom" for expr_compat_mode in [true, false],
        field_atom in ["str", 1],
        field in [field_atom,
                  Expr(:quote, field_atom),
                  Expr(:inert, field_atom),
                  QuoteNode(field_atom)]

        @test fl_eval(test_mod, Expr(:block, Expr(:., test_mod.gs, field))) ==
            ("got", field_atom)
        @test jl_eval(test_mod, Expr(:block, Expr(:., test_mod.gs, field));
                      expr_compat_mode) ==
            ("got", field_atom)
    end

    @testset "arg2 inert AST" for expr_compat_mode in [true, false],
        # oddly, bool and nothing don't work unquoted in flisp
        field_inner in [true,
                        nothing,
                        GlobalRef(Core, :Type),
                        Expr(:string, "s", "tr"),
                        Expr(:string, "s", Expr(:call, string, :tr))],
        field in [Expr(:inert, field_inner),
                  QuoteNode(field_inner)]

        @test fl_eval(test_mod, Expr(:block, Expr(:., test_mod.gs, field))) ==
            ("got", field_inner)
        @test jl_eval(test_mod, Expr(:block, Expr(:., test_mod.gs, field));
                      expr_compat_mode) ==
            ("got", field_inner)
    end

    @testset "arg2 non-inert non-atom should throw" for expr_compat_mode in [true, false],
        field in [Expr(:string, "s", "tr"),
                  Expr(:string, "s", Expr(:call, string, :tr)),
                  Expr(:quote, Expr(:string, "s", "tr")),
                  Expr(:quote, Expr(:string, "s", Expr(:$, :outer_field)))]

        @test_throws "invalid syntax" fl_eval(
            test_mod, Expr(:block, Expr(:., test_mod.gs, field)))
        @test_throws LoweringError jl_eval(
            test_mod, Expr(:block, Expr(:., test_mod.gs, field)))
    end
end

@testset "syntax context in macro body should be discarded" begin
    # Both `x`s should have argument context in the macrocall, not new-syntax
    # context (resulting in (99, 2))
    @test JuliaLowering.include_string(test_mod, raw"""
        macro set_value(name, body)
            @legacy_quote_to_syntax(quote
                $name = 1
                $body
            end)
        end
        (function ()
            x = 99
            r = @set_value x x + 1
            (x, r)
        end)()
   """) == (1,2)

    @test JuliaLowering.include_string(test_mod, raw"""
    macro addone_value(name, body)
        @legacy_quote_to_syntax(quote
            x = 99 # hygienic
            $name += 1
            $body
        end)
    end
    (function ()
         x = 0
         out = []
         push!(out, (x, @addone_value x x + 1))
         push!(out, (x, @addone_value x x + 1))
         push!(out, (x, @addone_value x x + 1))
         out
     end)()
    """) == [(0, 2), (1, 3), (2, 4)]

    @test JuliaLowering.include_string(test_mod, raw"""
    macro addone_value_quote2(name, body)
        @legacy_quote_to_syntax(quote
            x = 99 # hygienic
            $(:($name)) += 1
            $(:($body))
        end)
    end
    (function ()
         x = 0
         out = []
         push!(out, (x, @addone_value_quote2 x x + 1))
         push!(out, (x, @addone_value_quote2 x x + 1))
         push!(out, (x, @addone_value_quote2 x x + 1))
         out
     end)()
    """) == [(0, 2), (1, 3), (2, 4)]
end
