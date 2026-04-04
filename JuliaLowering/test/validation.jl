# Basic tests that the validator accepts or rejects a given form.  For testing
# error messages, write an IR test instead.

function vst1_ok(x::Expr)
    est = JuliaLowering.expr_to_est(x)
    JuliaLowering.valid_st1(est).ok
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
        "<:",
        ">:",
        "::",
        ".&&",
        ".||",
        "const",
        "global",
        "local",
        "macrocall",
        "quote",
        "inert",
        "inert_syntaxtree",
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
        "<:",
        ">:",
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
        @test !Meta.isexpr(:error, Meta.lower(@__MODULE__, ex))
        @test vst1_ok(ex)
    end
end

@test vst1_ok(Expr(:-->, 1))
@test vst1_ok(Expr(:-->, 1, 2))
@test vst1_ok(Expr(:-->, 1, 2, 3))

@test vst1_ok(Expr(:const, :a, 1))

# vst1_dot_rhs allows usually-invalid forms
@testset "dot rhs forms" for rhs in [:_, :__, Symbol("#unused#"), :ccall, :cglobal]
    @test vst1_ok(Expr(:., :Mod, rhs))
    @test vst1_ok(Expr(:., :Mod, Expr(:inert, rhs)))
    @test vst1_ok(Expr(:., :Mod, QuoteNode(rhs)))
    @test vst1_ok(Expr(:., :Mod, string(rhs)))
end

@test vst1_ok(:(using Mod: cglobal))
@test vst1_ok(:(Mod.cglobal))
@test vst1_ok(:(Mod._ = 1))

@testset "underscores that should probably not be valid" begin
    @test vst1_ok(:(Mod._))
    @test vst1_ok(:(function f(x::_); x; end))
    @test vst1_ok(:(global _))
    @test vst1_ok(:(global _::Int))
    @test vst1_ok(:(local _))
    @test vst1_ok(:(local _::Int))
end
