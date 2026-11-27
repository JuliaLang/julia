@testset "validationresult" begin
    local (; validation_errors, validation_ok, is_known) = JuliaLowering
    local pass = JuliaLowering.pass()
    local unkn = JuliaLowering.unknown()
    local fail = JuliaLowering.valid_st1(JuliaLowering.expr_to_est(Expr(:invalid_expr)))

    @test pass | unkn == pass
    @test unkn | pass == pass
    @test pass | fail == pass
    @test fail | pass == pass
    @test unkn | fail == fail
    @test fail | unkn == fail

    @test pass | pass == pass
    @test unkn | unkn == unkn
    @test fail | fail == fail

    @test pass & unkn == unkn
    @test unkn & pass == unkn
    @test pass & fail == fail
    @test fail & pass == fail
    @test unkn & fail == fail
    @test fail & unkn == fail

    @test pass & pass == pass
    @test unkn & unkn == unkn
    let f2 = fail & fail
        @test is_known(f2)
        @test !validation_ok(f2)
    end
end

# Basic tests that the validator accepts or rejects a given form.  For testing
# error messages, write an IR test instead.
function vst1_ok(x::Expr)
    est = JuliaLowering.expr_to_est(x)
    JuliaLowering.validation_ok(JuliaLowering.valid_st1(est))
end

let
    nonempty_heads = String[
        "let",
        "if",
        "try",
        "function",
        "call",
        "'",
        ".",
        "do",
        "=",
        "return",
        "for",
        "while",
        "curly",
        "where",
        "->",
        "flatten",
        "generator",
        "comprehension",
        "typed_comprehension",
        "comparison",
        "::",
        ".&&",
        ".||",
        "const",
        "global",
        "local",
        "macrocall",
        "quote",
        "inert",
        "syntaxinert",
        "top",
        "opaque_closure",
        "symboliclabel",
        "symbolicgoto",
        "symbolicblock",
        "gc_preserve",
        "isdefined",
        "lambda",
        "foreigncall",
        "cfunction",
        "cconv",
        "tryfinally",
        "inline",
        "noinline",
        "inbounds",
        "islocal",
        "isglobal",
        "new",
        "splatnew",
        "thisfunction",
        "copyast",
        ":",
        "...",
        ".+=",
        "|=",
        ".=",
        "braces",
        "\$",
        "parameters",
        "kw",
        "outer",
        "macro",
        "struct",
        "abstract",
        "primitive",
        "module",
        "local-def",
        "::",
        "where",
        "curly",
        "ref",
        "ncat",
        "nrow",
        "typed_hcat",
        "typed_vcat",
        "typed_ncat",
        "import",
        "using",
    ]
    standalone_heads = String[
        "block",
        "tuple",
        "public",
        "export",
        "string",
        "&&",
        "<:",
        ">:",
        "-->",
        "&&",
        "||",
        "toplevel",
        "locals",
        "vect",
        "hcat",
        "vcat",
        "meta",
        "boundscheck",
        "loopinfo",
    ]
    @testset for h in nonempty_heads
        @test !vst1_ok(Expr(Symbol(h)))
    end
    @testset for h in standalone_heads
        ex = Expr(Symbol(h))
        @test !Meta.isexpr(fl_lower(@__MODULE__, ex), :error)
        @test vst1_ok(ex)
    end
end

if JL.DEBUG
    @test_throws ErrorException vst1_ok(Expr(:nothing))
else
    @test !vst1_ok(Expr(:nothing))
end
@test vst1_ok(Expr(:block, nothing))
@test vst1_ok(Expr(:block, GlobalRef(Core, :nothing)))

@test vst1_ok(Expr(:-->, 1))
@test vst1_ok(Expr(:-->, 1, 2))
@test vst1_ok(Expr(:-->, 1, 2, 3))
@test vst1_ok(Expr(:-->, Expr(:..., Expr(:tuple, 1, 2, 3))))
@test vst1_ok(Expr(:-->, Expr(:kw, :foo, 1)))

@test vst1_ok(Expr(:const, :a, 1))

# vst1_dot_getproperty_rhs allows usually-invalid forms
@testset "dot rhs forms" for rhs in [:_, :__, Symbol("#unused#"), :ccall, :cglobal, 1]
    @test vst1_ok(Expr(:., :Mod, rhs))
    @test vst1_ok(Expr(:., :Mod, Expr(:inert, rhs)))
    @test vst1_ok(Expr(:., :Mod, QuoteNode(rhs)))

    @test vst1_ok(Expr(:., :Mod, string(rhs)))
    @test vst1_ok(Expr(:., :Mod, Expr(:inert, string(rhs))))
    @test vst1_ok(Expr(:., :Mod, QuoteNode(string(rhs))))
end

@test vst1_ok(:(using Mod: cglobal))
@test vst1_ok(:(Mod.cglobal))
@test vst1_ok(:(Mod._ = 1))

@test !vst1_ok(:(local cglobal::Int))
@test !vst1_ok(:(let ccall; end))
@test !vst1_ok(:(let ccall::Int; end))
@test !vst1_ok(:(try; catch ccall; end))

@test !vst1_ok(Expr(:const, Expr(:global, :x)))

# calldecl name should be syntax
@test !vst1_ok(Expr(:function, Expr(:call, Base.view, :A), Expr(:block)))
@test !vst1_ok(Expr(:(=), Expr(:call, Base.view, :A, :idx), 2))

@testset "underscores that should probably not be valid" begin
    @test vst1_ok(:(Mod._))
    @test vst1_ok(:(function f(x::_); x; end))
    @test vst1_ok(:(global _))
    @test vst1_ok(:(global _::Int))
    @test vst1_ok(:(local _))
    @test vst1_ok(:(local _::Int))
end

@testset "empty symbol is valid" for e in [
    Expr(:block, Symbol(""))
    Expr(:inert, Symbol(""))
    Expr(:(::), Symbol(""), :Int)
    Expr(:const, Expr(:(=), Symbol(""), 1))
    Expr(:global, Expr(:(=), Symbol(""), 1))
    Expr(:local, Expr(:(=), Symbol(""), 1))
    Expr(:let, Expr(:block, Expr(:(=), Symbol(""), 1)), Expr(:block))
    Expr(:function, Expr(:call, Symbol("")), Expr(:block))
    ]
    @test vst1_ok(e)
end

@testset "import/using path" begin
    # `.` after identifier
    @test !vst1_ok(Expr(:import, Expr(:., :A, :., :B)))
    # leading `.` on a name
    @test !vst1_ok(Expr(:import, Expr(:(:), Expr(:., :M), Expr(:., :., :a))))
    # non-identifier rename
    @test !vst1_ok(Expr(:import, Expr(:(:), Expr(:., :M),
                                      Expr(:as, Expr(:., :a), Expr(:call, :f)))))
    # empty path
    @test !vst1_ok(Expr(:import, Expr(:.)))
    # not an import path
    @test !vst1_ok(Expr(:import, Expr(:call, :f)))
    @test !vst1_ok(Expr(:import, 42))
end

@testset "toplevel" begin
    @test !vst1_ok(Expr(:function, Expr(:tuple), Expr(:block, Expr(:toplevel))))
    @test !vst1_ok(Expr(:->, Expr(:tuple), Expr(:block, Expr(:toplevel))))
    @test !vst1_ok(Expr(:(=), Expr(:call, :f), Expr(:block, Expr(:toplevel))))
    @test vst1_ok(Expr(:let, Expr(:block), Expr(:block, Expr(:toplevel))))
end

@testset "rare valid forms" for ex in [
        Expr(:softscope), Expr(:generated), Expr(:boundscheck, true),
        Expr(:the_exception), Expr(:top, :x),
        Expr(:function, Expr(:call, Expr(:., :A, "f")), Expr(:block)),
        Expr(:function, Expr(:call, Expr(:., Base, QuoteNode(:f))), Expr(:block)),
    ]
    @test vst1_ok(ex)
end

# Malformed forms that macros could produce, with the error each should give
@testset "malformed forms" begin
    blk = Expr(:block)
    @testset "$msg" for (ex, msg) in [
            Expr(:break, :lab) => "labeled `break` outside of loop or symbolic block"
            Expr(:try, blk) => "try without catch or finally"
            Expr(:function, Expr(:call, Expr(:., :A, Expr(:tuple))), blk) =>
                "dotcall syntax not valid here"
            Expr(:function, Expr(:call, Expr(:., :A, Expr(:call, :g))), blk) =>
                "invalid `.` syntax"
            Expr(:while, true, Expr(:break, 1)) => "expected identifier"
            Expr(:->, Expr(:block, :a, :b, :c), blk) => "more than one semicolon in signature"
            Expr(:function, Expr(:call, :f, Expr(:kw, :x)), blk) =>
                "malformed optional positional parameter; expected `=`"
            Expr(:struct, false, :S, Expr(:block, Expr(:(=), :x, 1))) =>
                "assignment syntax in structure fields is reserved"
            Expr(:call, :f, Expr(:..., 1, 2)) => "expected one argument to `...`"
            Expr(:for, Expr(:call, :f), blk) => "expected one of `=`, `in`, `∈`"
            Expr(:flatten, 1) => "expected `generator`"
            Expr(:lambda, 1) => "malformed `lambda`"
            Expr(:var"with-static-parameters", 1) => "expected `lambda`"
            Expr(:generated_lambda, 1) => "malformed `generated_lambda`"
            Expr(:meta, Expr(:call, :f)) => "unsupported `meta` argument"
        ]
        errs = JuliaLowering.validation_errors(
            JuliaLowering.valid_st1(JuliaLowering.expr_to_est(ex)))
        @test !isnothing(errs) && msg in [m for e in errs for m in e.msgs]
    end
end

@testset "unexpanded syntax (valid_st0)" begin
    vst0_ok(x) = JuliaLowering.valid_st0(JuliaLowering.expr_to_est(x))
    lnn = LineNumberNode(1)
    @test vst0_ok(Expr(:macrocall, Symbol("@m"), lnn, :x))
    # Errors inside macrocalls are left to the macro
    @test vst0_ok(Expr(:macrocall, Symbol("@m"), 1, :x))
    @test vst0_ok(Expr(:call, :f, Expr(:macrocall, Symbol("@m"), lnn, Expr(:..., 1, 2))))
    @test !vst0_ok(Expr(:call, :f, Expr(:..., 1, 2)))
    @test vst0_ok(Expr(:quote, Expr(:call, :f, Expr(:$, :x))))
    @test !vst0_ok(Expr(:quote, Expr(:call, :f, Expr(:$, Expr(:..., 1, 2)))))
    # Only an interpolation at the outermost quote level is checked
    @test vst0_ok(Expr(:quote, Expr(:quote, Expr(:$, Expr(:..., 1, 2)))))
    @test !vst0_ok(Expr(:quote, Expr(:quote, Expr(:$, Expr(:$, Expr(:..., 1, 2))))))
end
