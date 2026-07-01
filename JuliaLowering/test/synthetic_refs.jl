# Regression coverage for `synthetic_ref` metadata (see `synthetic_ref` in `src/ast.jl`).
# The lowering pipeline emits many internal references to user-visible bindings.
# Each such reference must be tagged with `synthetic_ref(...)` at its emit site so consumers
# walking the resolved tree can distinguish machinery refs from real user-written uses.
# These tests exercise a minimal "pretend we are an LSP-style occurrence analyzer" walker
# and assert that each of the known machinery constructs classifies correctly.
module synthetic_refs

using Test, JuliaSyntax, JuliaLowering
using .JuliaLowering: VariableAnalysisContext, BindingInfo,
    binding_ex, expand_forms_1, expand_forms_2, get_binding, getmeta, resolve_scopes
using .JuliaSyntax: @K_str, Kind, SyntaxTree, byte_range, children, kind, numchildren, parsestmt

function lower_to_st3(code::AbstractString; version::VersionNumber=VERSION)
    mod = Module(:SyntheticRefTestMod)
    st0 = parsestmt(SyntaxTree, code; version)
    ctx1, st1 = expand_forms_1(mod, st0, false, Base.get_world_counter())
    ctx2, st2 = expand_forms_2(ctx1, st1)
    ctx3, st3 = resolve_scopes(ctx2, st2)
    return (ctx3, st3)
end

# Return every `K"BindingId"` ref for `name` as
# `(:decl | :def | :use, synthetic_ref::Bool, byte_range)`, following
# the classification downstream LSP-style occurrence analyzers use:
# - `K"local"` / `K"global"` / `K"function_decl"` child → :decl
# - `K"="` LHS / `K"method_defs"` / `K"constdecl"` first child → :def
# - `K"lambda"`'s arg / sparam block children → :def
# - any other `K"BindingId"` → :use
function classify_binding_id_refs(ctx3::VariableAnalysisContext, st3::SyntaxTree, name::String)
    out = Tuple{Symbol,Bool,UnitRange{Int}}[]
    function record_def!(c::SyntaxTree)
        kind(c) === K"BindingId" || return
        binfo = get_binding(ctx3, c)
        binfo.name == name || return
        sr = getmeta(c, :synthetic_ref, false)::Bool
        push!(out, (:def, sr, byte_range(c)))
    end
    function walk(st::SyntaxTree)
        k = kind(st)
        nc = numchildren(st)
        if (k === K"local" || k === K"global" || k === K"function_decl") && nc ≥ 1
            c = st[1]
            if kind(c) === K"BindingId"
                binfo = get_binding(ctx3, c)
                if binfo.name == name
                    sr = getmeta(c, :synthetic_ref, false)::Bool
                    push!(out, (:decl, sr, byte_range(c)))
                end
            end
            return
        end
        if (k === K"=" || k === K"method_defs" || k === K"constdecl") && nc ≥ 1
            record_def!(st[1])
            for i = 2:nc
                walk(st[i])
            end
            return
        end
        if k === K"lambda" && nc ≥ 2
            # child 1 = arg block; child 2 = sparam block (direct children are decls)
            for i in 1:numchildren(st[1]); record_def!(st[1][i]); end
            for i in 1:numchildren(st[2]); record_def!(st[2][i]); end
            for i = 3:nc; walk(st[i]); end
            return
        end
        if k === K"BindingId"
            binfo = get_binding(ctx3, st)
            if binfo.name == name
                sr = getmeta(st, :synthetic_ref, false)::Bool
                push!(out, (:use, sr, byte_range(st)))
            end
            return
        end
        for c in children(st)
            walk(c)
        end
    end
    walk(st3)
    return out
end

# Number of `:use` refs a downstream consumer would record (machinery
# refs tagged `synthetic_ref` are skipped).
function real_use_count(ctx3, st3, name::String)
    refs = classify_binding_id_refs(ctx3, st3, name)
    return count(ref -> ref[1] === :use && !ref[2], refs)
end

@testset "function definitions" begin
    for code in ("function foo end",
                 "foo(x) = x",
                 "function foo(x)\n    return x\nend",
                 "foo(x, y=1) = x + y",                                # optional positional
                 "foo(x; k=1) = x + k",                                # keyword
                 "foo(x, y=1; k=1) = x + y + k",                       # combined
                 "foo(x::T) where T = x",                              # where clause
                 "foo(x)::Int = x",                                    # return-type annotation
                 "foo(x, y...) = (x, y)",                              # varargs
                 "@generated function foo(x)\n    return :(x + 1)\nend")
        ctx3, st3 = lower_to_st3(code)
        @test real_use_count(ctx3, st3, "foo") == 0
    end
end

@testset "macro definitions" begin
    for code in ("macro foo end",
                 "macro foo(x); x; end",
                 "macro foo(x, y); x; end",
                 "macro foo(args...); args; end")
        ctx3, st3 = lower_to_st3(code)
        @test real_use_count(ctx3, st3, "@foo") == 0
    end
end

@testset "type definitions" begin
    for code in ("struct Foo end",
                 "mutable struct Foo; x::Int; end",
                 "struct Foo{T}; x::T; end",                           # parametric
                 "struct Foo; x::Int; Foo(x) = new(x); end",           # inner constructor
                 "struct Foo{T}; x::T; Foo{T}(x) where T = new{T}(x); end",
                 "abstract type Foo end",
                 "abstract type Foo <: Integer end",                   # supertype
                 "primitive type Foo 8 end")
        ctx3, st3 = lower_to_st3(code)
        @test real_use_count(ctx3, st3, "Foo") == 0
    end
    @testset "typegroup" begin
        let (ctx3, st3) = lower_to_st3("""
                typegroup
                    struct A
                        x::Int
                    end
                    struct B
                        y::Int
                    end
                end
                """; version=v"1.14")
            @test real_use_count(ctx3, st3, "A") == 0
            @test real_use_count(ctx3, st3, "B") == 0
        end
    end
end

# User-written refs must survive the machinery wrap: self-recursive
# calls and identifiers inside default expressions stay `:use`.
@testset "user-written refs preserved alongside machinery wraps" begin
    # self-recursive call
    let (ctx3, st3) = lower_to_st3("foo(x) = foo(x - 1)")
        @test real_use_count(ctx3, st3, "foo") == 1
    end
    # self-recursive kwfunc call
    let (ctx3, st3) = lower_to_st3("""
            function foo(x; kw=1)
                return foo(x - 1; kw=kw + 1)
            end
            """)
        @test real_use_count(ctx3, st3, "foo") == 1
        @test real_use_count(ctx3, st3, "x") == 1
        @test real_use_count(ctx3, st3, "kw") == 2
    end
    # positional dependent default
    let (ctx3, st3) = lower_to_st3("f(x, y=x) = x + y")
        @test real_use_count(ctx3, st3, "x") == 2
        @test real_use_count(ctx3, st3, "y") == 1
    end
    # keyword dependent default
    let (ctx3, st3) = lower_to_st3("function f(a; b=a); b; end")
        @test real_use_count(ctx3, st3, "a") == 2
        @test real_use_count(ctx3, st3, "b") == 2
    end
    # inner constructor with dependent default
    let (ctx3, st3) = lower_to_st3(
            "struct B; v::Int; B(y, z=y) = new(y + z); end")
        @test real_use_count(ctx3, st3, "y") == 2
        @test real_use_count(ctx3, st3, "z") == 1
    end
    # positional + keyword dependent defaults combined
    let (ctx3, st3) = lower_to_st3("f(x, y=x; kw=y) = x + y + kw")
        @test real_use_count(ctx3, st3, "x") == 3
        @test real_use_count(ctx3, st3, "y") == 3
        @test real_use_count(ctx3, st3, "kw") == 2
    end
end

end # module synthetic_refs
