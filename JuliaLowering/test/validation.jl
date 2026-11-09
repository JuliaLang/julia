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
