# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using Base.Meta
using Core: ReturnNode

include(normpath(@__DIR__, "irutils.jl"))

"""
Helper to walk the AST and call a function on every node.
"""
function walk(func, expr)
    func(expr)
    if isa(expr, Expr)
        func(expr.head)
        for o in expr.args
            walk(func, o)
        end
    end
end

"""
Helper to test that every slot is in range after inlining.
"""
function test_inlined_symbols(func, argtypes)
    src, rettype = code_typed(func, argtypes)[1]
    nl = length(src.slotnames)
    ast = Expr(:block)
    ast.args = src.code
    walk(ast) do e
        if isa(e, Core.Slot)
            @test 1 <= e.id <= nl
        end
        if isa(e, Core.NewvarNode)
            @test 1 <= e.slot.id <= nl
        end
    end
end

# Test case 1:
# Make sure that all symbols are properly escaped after inlining
# https://github.com/JuliaLang/julia/issues/12620
@inline function test_inner(count)
    x = 1
    i = 0
    while i <= count
        y = x
        x = x + y
        i += 1
    end
end
function test_outer(a)
    test_inner(a)
end
test_inlined_symbols(test_outer, Tuple{Int64})

# Test case 2:
# Make sure that an error is thrown for the undeclared
# y in the else branch.
# https://github.com/JuliaLang/julia/issues/12620
@inline function foo_inl(x)
    if x
        y = 2
    else
        return y
    end
end
function bar12620()
    for i = 1:3
        foo_inl(i==1)
    end
end
@test_throws UndefVarError(:y) bar12620()

# issue #16165
@inline f16165(x) = (x = UInt(x) + 1)
g16165(x) = f16165(x)
@test g16165(1) === (UInt(1) + 1)

# issue #18948
f18948() = (local bar::Int64; bar=1.5)
g18948() = (local bar::Int32; bar=0x80000000)
@test_throws InexactError f18948()
@test_throws InexactError g18948()

# issue #21074
struct s21074
    x::Tuple{Int, Int}
end
@inline Base.getindex(v::s21074, i::Integer) = v.x[i]
@eval f21074() = $(s21074((1,2))).x[1]
let (src, _) = code_typed(f21074, ())[1]
    @test src.code[end] == ReturnNode(1)
end
@eval g21074() = $(s21074((1,2)))[1]
let (src, _) = code_typed(g21074, ())[1]
    @test src.code[end] == ReturnNode(1)
end

# issue #21311
counter21311 = Ref(0)
@noinline function update21311!(x)
    counter21311[] += 1
    x[] = counter21311[]
    return x
end
@noinline map21311(t::Tuple{Any}) = (update21311!(t[1]),)
@inline map21311(t::Tuple) = (update21311!(t[1]), map21311(Base.tail(t))...)
function read21311()
    xs = Ref(1), Ref(1)
    map21311(xs)
    return xs[1]
end
let a = read21311()
    @test a[] == 1
end

# issue #29083
f29083(;μ,σ) = μ + σ*randn()
g29083() = f29083(μ=2.0,σ=0.1)
let c = code_typed(g29083, ())[1][1].code
    # make sure no call to kwfunc remains
    @test !any(e->(isa(e,Expr) && ((e.head === :invoke && e.args[1].def.name === :kwfunc) ||
                                   (e.head === :foreigncall && e.args[1] === QuoteNode(:jl_get_keyword_sorter)))),
               c)
end

@testset "issue #19122: [no]inline of short func. def. with return type annotation" begin
    exf19122 = @macroexpand(@inline f19122()::Bool = true)
    exg19122 = @macroexpand(@noinline g19122()::Bool = true)
    @test exf19122.args[2].args[1].args[1] === :inline
    @test exg19122.args[2].args[1].args[1] === :noinline

    @inline f19122()::Bool = true
    @noinline g19122()::Bool = true
    @test f19122()
    @test g19122()
end

@testset "issue #27403: getindex is inlined with Union{Int,Missing}" begin
    function sum27403(X::AbstractArray)
        s = zero(eltype(X)) + zero(eltype(X))
        for x in X
            if !ismissing(x)
                s += x
            end
        end
        s
    end

    (src, _) = code_typed(sum27403, Tuple{Vector{Int}})[1]
    @test !any(x -> x isa Expr && x.head === :invoke, src.code)
end

# check that ismutabletype(type) can be fully eliminated
f_mutable_nothrow(s::String) = Val{typeof(s).name.flags}
@test fully_eliminated(f_mutable_nothrow, (String,))

# check that ifelse can be fully eliminated
function f_ifelse(x)
    a = ifelse(true, false, true)
    b = ifelse(a, true, false)
    return b ? x + 1 : x
end
@test length(code_typed(f_ifelse, (String,))[1][1].code) <= 2

# Test that inlining of _apply_iterate properly hits the inference cache
@noinline cprop_inline_foo1() = (1, 1)
@noinline cprop_inline_foo2() = (2, 2)
function cprop_inline_bar(x...)
    if x === (1, 1, 1, 1)
        return x
    else
        # What you put here doesn't really matter,
        # the point is to prevent inlining when
        # x is not known to be (1, 1, 1, 1)
        println(stdout, "Hello")
        println(stdout, "World")
        println(stdout, "Hello")
        println(stdout, "World")
        println(stdout, "Hello")
        println(stdout, "World")
        println(stdout, "Hello")
        println(stdout, "World")
        println(stdout, "Hello")
        println(stdout, "World")
        println(stdout, "Hello")
        println(stdout, "World")
        return x
    end
    x
end

function cprop_inline_baz1()
    return cprop_inline_bar(cprop_inline_foo1()..., cprop_inline_foo1()...)
end
@test fully_eliminated(cprop_inline_baz1, ())

function cprop_inline_baz2()
    return cprop_inline_bar(cprop_inline_foo2()..., cprop_inline_foo2()...)
end
@test length(code_typed(cprop_inline_baz2, ())[1][1].code) == 2

# Check that apply_type/TypeVar can be fully eliminated
function f_apply_typevar(T)
    NTuple{N, T} where N
    return T
end
@test fully_eliminated(f_apply_typevar, (Type{Any},))

# check that div can be fully eliminated
function f_div(x)
    div(x, 1)
    return x
end
@test fully_eliminated(f_div, (Int,); retval=Core.Argument(2))
# ...unless we div by an unknown amount
function f_div(x, y)
    div(x, y)
    return x
end
@test length(code_typed(f_div, (Int, Int))[1][1].code) > 1

f_identity_splat(t) = (t...,)
@test fully_eliminated(f_identity_splat, (Tuple{Int,Int},))

# splatting one tuple into (,) plus zero or more empties should reduce
# this pattern appears for example in `fill_to_length`
f_splat_with_empties(t) = (()..., t..., ()..., ()...)
@test fully_eliminated(f_splat_with_empties, (NTuple{200,UInt8},))

# check that <: can be fully eliminated
struct SomeArbitraryStruct; end
function f_subtype()
    T = SomeArbitraryStruct
    T <: Bool
end
@test fully_eliminated(f_subtype, Tuple{}; retval=false)

# check that pointerref gets deleted if unused
f_pointerref(T::Type{S}) where S = Val(length(T.parameters))
let code = code_typed(f_pointerref, Tuple{Type{Int}})[1][1].code
    any_ptrref = false
    for i = 1:length(code)
        stmt = code[i]
        isa(stmt, Expr) || continue
        stmt.head === :call || continue
        arg1 = stmt.args[1]
        if arg1 === Base.pointerref || (isa(arg1, GlobalRef) && arg1.name === :pointerref)
            any_ptrref = true
        end
    end
    @test !any_ptrref
end

# Test that inlining can inline _applys of builtins/_applys on SimpleVectors
function foo_apply_apply_type_svec()
    A = (Tuple, Float32)
    B = Tuple{Float32, Float32}
    Core.apply_type(A..., B.types...)
end
@test fully_eliminated(foo_apply_apply_type_svec, Tuple{}; retval=NTuple{3, Float32})

# The that inlining doesn't drop ambiguity errors (#30118)
c30118(::Tuple{Ref{<:Type}, Vararg}) = nothing
c30118(::Tuple{Ref, Ref}) = nothing
b30118(x...) = c30118(x)

@test_throws MethodError c30118((Base.RefValue{Type{Int64}}(), Ref(Int64)))
@test_throws MethodError b30118(Base.RefValue{Type{Int64}}(), Ref(Int64))

# Issue #34900
f34900(x::Int, y) = x
f34900(x, y::Int) = y
f34900(x::Int, y::Int) = invoke(f34900, Tuple{Int, Any}, x, y)
@test fully_eliminated(f34900, Tuple{Int, Int}; retval=Core.Argument(2))

@testset "check jl_ir_flag_inlineable for inline macro" begin
    @test ccall(:jl_ir_flag_inlineable, Bool, (Any,), only(methods(@inline x -> x)).source)
    @test ccall(:jl_ir_flag_inlineable, Bool, (Any,), only(methods(x -> (@inline; x))).source)
    @test !ccall(:jl_ir_flag_inlineable, Bool, (Any,), only(methods(x -> x)).source)
    @test ccall(:jl_ir_flag_inlineable, Bool, (Any,), only(methods(@inline function f(x) x end)).source)
    @test ccall(:jl_ir_flag_inlineable, Bool, (Any,), only(methods(function f(x) @inline; x end)).source)
    @test !ccall(:jl_ir_flag_inlineable, Bool, (Any,), only(methods(function f(x) x end)).source)
    @test ccall(:jl_ir_flag_inlineable, Bool, (Any,), only(methods() do x @inline; x end).source)
    @test !ccall(:jl_ir_flag_inlineable, Bool, (Any,), only(methods() do x x end).source)
end

const _a_global_array = [1]
f_inline_global_getindex() = _a_global_array[1]
let ci = code_typed(f_inline_global_getindex, Tuple{})[1].first
    @test any(x->(isexpr(x, :call) && x.args[1] === GlobalRef(Base, :arrayref)), ci.code)
end

# Issue #29114 & #36087 - Inlining of non-tuple splats
f_29115(x) = (x...,)
@test @allocated(f_29115(1)) == 0
@test @allocated(f_29115(1=>2)) == 0
let ci = code_typed(f_29115, Tuple{Int64})[1].first
    @test length(ci.code) == 2 && isexpr(ci.code[1], :call) &&
        ci.code[1].args[1] === GlobalRef(Core, :tuple)
end
let ci = code_typed(f_29115, Tuple{Pair{Int64, Int64}})[1].first
    @test length(ci.code) == 4 && isexpr(ci.code[1], :call) &&
        ci.code[end-1].args[1] === GlobalRef(Core, :tuple)
end

# Issue #37182 & #37555 - Inlining of pending nodes
function f37555(x::Int; kwargs...)
    @assert x < 10
    +(x, kwargs...)
end
@test f37555(1) == 1

# Test that we can inline small constants even if they are not isbits
struct NonIsBitsDims
    dims::NTuple{N, Int} where N
end
NonIsBitsDims() = NonIsBitsDims(())
@test fully_eliminated(NonIsBitsDims, (); retval=QuoteNode(NonIsBitsDims()))

struct NonIsBitsDimsUndef
    dims::NTuple{N, Int} where N
    NonIsBitsDimsUndef() = new()
end
@test Core.Compiler.is_inlineable_constant(NonIsBitsDimsUndef())
@test !Core.Compiler.is_inlineable_constant((("a"^1000, "b"^1000), nothing))

# More nothrow modeling for apply_type
f_apply_type_typeof(x) = (Ref{typeof(x)}; nothing)
@test fully_eliminated(f_apply_type_typeof, Tuple{Any})
@test fully_eliminated(f_apply_type_typeof, Tuple{Vector})
@test fully_eliminated(x->(Val{x}; nothing), Tuple{Int})
@test fully_eliminated(x->(Val{x}; nothing), Tuple{Symbol})
@test fully_eliminated(x->(Val{x}; nothing), Tuple{Tuple{Int, Int}})
@test !fully_eliminated(x->(Val{x}; nothing), Tuple{String})
@test !fully_eliminated(x->(Val{x}; nothing), Tuple{Any})
@test !fully_eliminated(x->(Val{x}; nothing), Tuple{Tuple{Int, String}})

struct RealConstrained{T <: Real}; end
@test !fully_eliminated(x->(RealConstrained{x}; nothing), Tuple{Int})
@test !fully_eliminated(x->(RealConstrained{x}; nothing), Tuple{Type{Vector{T}} where T})

# Check that pure functions with non-inlineable results still get deleted
struct Big
    x::NTuple{1024, Int}
end
@Base.pure Big() = Big(ntuple(identity, 1024))
function pure_elim_full()
    Big()
    nothing
end

@test fully_eliminated(pure_elim_full, Tuple{})

# Union splitting of convert
f_convert_missing(x) = convert(Int64, x)
let ci = code_typed(f_convert_missing, Tuple{Union{Int64, Missing}})[1][1],
    ci_unopt = code_typed(f_convert_missing, Tuple{Union{Int64, Missing}}; optimize=false)[1][1]
    # We want to check that inlining was able to union split this, but we don't
    # want to make the test too specific to the exact structure that inlining
    # generates, so instead, we just check that the compiler made it bigger.
    # There are performance tests that are also sensitive to union splitting
    # here, so a non-obvious regression
    @test length(ci.code) >
        length(ci_unopt.code)
end

# OC getfield elim
using Base.Experimental: @opaque
f_oc_getfield(x) = (@opaque ()->x)()
@test fully_eliminated(f_oc_getfield, Tuple{Int})

import Core.Compiler: argextype, singleton_type
const EMPTY_SPTYPES = Any[]

code_typed1(args...; kwargs...) = first(only(code_typed(args...; kwargs...)))::Core.CodeInfo
get_code(args...; kwargs...) = code_typed1(args...; kwargs...).code

# check if `x` is a dynamic call of a given function
iscall(y) = @nospecialize(x) -> iscall(y, x)
function iscall((src, f)::Tuple{Core.CodeInfo,Base.Callable}, @nospecialize(x))
    return iscall(x) do @nospecialize x
        singleton_type(argextype(x, src, EMPTY_SPTYPES)) === f
    end
end
iscall(pred::Base.Callable, @nospecialize(x)) = Meta.isexpr(x, :call) && pred(x.args[1])

# check if `x` is a statically-resolved call of a function whose name is `sym`
isinvoke(y) = @nospecialize(x) -> isinvoke(y, x)
isinvoke(sym::Symbol, @nospecialize(x)) = isinvoke(mi->mi.def.name===sym, x)
isinvoke(pred::Function, @nospecialize(x)) = Meta.isexpr(x, :invoke) && pred(x.args[1]::Core.MethodInstance)

@testset "@inline/@noinline annotation before definition" begin
    M = Module()
    @eval M begin
        @inline function _def_inline(x)
            # this call won't be resolved and thus will prevent inlining to happen if we don't
            # annotate `@inline` at the top of this function body
            return unresolved_call(x)
        end
        def_inline(x) = _def_inline(x)
        @noinline _def_noinline(x) = x # obviously will be inlined otherwise
        def_noinline(x) = _def_noinline(x)

        # test that they don't conflict with other "before-definition" macros
        @inline Base.@constprop :aggressive function _def_inline_noconflict(x)
            # this call won't be resolved and thus will prevent inlining to happen if we don't
            # annotate `@inline` at the top of this function body
            return unresolved_call(x)
        end
        def_inline_noconflict(x) = _def_inline_noconflict(x)
        @noinline Base.@constprop :aggressive _def_noinline_noconflict(x) = x # obviously will be inlined otherwise
        def_noinline_noconflict(x) = _def_noinline_noconflict(x)
    end

    let code = get_code(M.def_inline, (Int,))
        @test all(!isinvoke(:_def_inline), code)
    end
    let code = get_code(M.def_noinline, (Int,))
        @test any(isinvoke(:_def_noinline), code)
    end
    # test that they don't conflict with other "before-definition" macros
    let code = get_code(M.def_inline_noconflict, (Int,))
        @test all(!isinvoke(:_def_inline_noconflict), code)
    end
    let code = get_code(M.def_noinline_noconflict, (Int,))
        @test any(isinvoke(:_def_noinline_noconflict), code)
    end
end

@testset "@inline/@noinline annotation within a function body" begin
    M = Module()
    @eval M begin
        function _body_inline(x)
            @inline
            # this call won't be resolved and thus will prevent inlining to happen if we don't
            # annotate `@inline` at the top of this function body
            return unresolved_call(x)
        end
        body_inline(x) = _body_inline(x)
        function _body_noinline(x)
            @noinline
            return x # obviously will be inlined otherwise
        end
        body_noinline(x) = _body_noinline(x)

        # test annotations for `do` blocks
        @inline simple_caller(a) = a()
        function do_inline(x)
            simple_caller() do
                @inline
                # this call won't be resolved and thus will prevent inlining to happen if we don't
                # annotate `@inline` at the top of this anonymous function body
                return unresolved_call(x)
            end
        end
        function do_noinline(x)
            simple_caller() do
                @noinline
                return x # obviously will be inlined otherwise
            end
        end
    end

    let code = get_code(M.body_inline, (Int,))
        @test all(!isinvoke(:_body_inline), code)
    end
    let code = get_code(M.body_noinline, (Int,))
        @test any(isinvoke(:_body_noinline), code)
    end
    # test annotations for `do` blocks
    let code = get_code(M.do_inline, (Int,))
        # what we test here is that both `simple_caller` and the anonymous function that the
        # `do` block creates should inlined away, and as a result there is only the unresolved call
        @test all(code) do @nospecialize x
            !isinvoke(:simple_caller, x) &&
            !isinvoke(x) do mi
                startswith(string(mi.def.name), '#')
            end
        end
    end
    let code = get_code(M.do_noinline, (Int,))
        # the anonymous function that the `do` block created shouldn't be inlined here
        @test any(code) do @nospecialize x
            isinvoke(x) do mi
                startswith(string(mi.def.name), '#')
            end
        end
    end
end

@testset "callsite @inline/@noinline annotations" begin
    M = Module()
    @eval M begin
        # this global variable prevents inference to fold everything as constant, and/or the optimizer to inline the call accessing to this
        g = 0

        @noinline noinlined_explicit(x) = x
        force_inline_explicit(x)        = @inline noinlined_explicit(x)
        force_inline_block_explicit(x)  = @inline noinlined_explicit(x) + noinlined_explicit(x)
        noinlined_implicit(x)          = g
        force_inline_implicit(x)       = @inline noinlined_implicit(x)
        force_inline_block_implicit(x) = @inline noinlined_implicit(x) + noinlined_implicit(x)

        @inline inlined_explicit(x)      = x
        force_noinline_explicit(x)       = @noinline inlined_explicit(x)
        force_noinline_block_explicit(x) = @noinline inlined_explicit(x) + inlined_explicit(x)
        inlined_implicit(x)              = x
        force_noinline_implicit(x)       = @noinline inlined_implicit(x)
        force_noinline_block_implicit(x) = @noinline inlined_implicit(x) + inlined_implicit(x)

        # test callsite annotations for constant-prop'ed calls

        @noinline Base.@constprop :aggressive noinlined_constprop_explicit(a) = a+g
        force_inline_constprop_explicit() = @inline noinlined_constprop_explicit(0)
        Base.@constprop :aggressive noinlined_constprop_implicit(a) = a+g
        force_inline_constprop_implicit() = @inline noinlined_constprop_implicit(0)

        @inline Base.@constprop :aggressive inlined_constprop_explicit(a) = a+g
        force_noinline_constprop_explicit() = @noinline inlined_constprop_explicit(0)
        @inline Base.@constprop :aggressive inlined_constprop_implicit(a) = a+g
        force_noinline_constprop_implicit() = @noinline inlined_constprop_implicit(0)

        @noinline notinlined(a) = a
        function nested(a0, b0)
            @noinline begin
                a = @inline notinlined(a0) # this call should be inlined
                b = notinlined(b0) # this call should NOT be inlined
                return a, b
            end
        end
    end

    let code = get_code(M.force_inline_explicit, (Int,))
        @test all(!isinvoke(:noinlined_explicit), code)
    end
    let code = get_code(M.force_inline_block_explicit, (Int,))
        @test all(code) do @nospecialize x
            !isinvoke(:noinlined_explicit, x) &&
            !isinvoke(:(+), x)
        end
    end
    let code = get_code(M.force_inline_implicit, (Int,))
        @test all(!isinvoke(:noinlined_implicit), code)
    end
    let code = get_code(M.force_inline_block_implicit, (Int,))
        @test all(!isinvoke(:noinlined_explicit), code)
    end

    let code = get_code(M.force_noinline_explicit, (Int,))
        @test any(isinvoke(:inlined_explicit), code)
    end
    let code = get_code(M.force_noinline_block_explicit, (Int,))
        @test count(isinvoke(:inlined_explicit), code) == 2
    end
    let code = get_code(M.force_noinline_implicit, (Int,))
        @test any(isinvoke(:inlined_implicit), code)
    end
    let code = get_code(M.force_noinline_block_implicit, (Int,))
        @test count(isinvoke(:inlined_implicit), code) == 2
    end

    let code = get_code(M.force_inline_constprop_explicit)
        @test all(!isinvoke(:noinlined_constprop_explicit), code)
    end
    let code = get_code(M.force_inline_constprop_implicit)
        @test all(!isinvoke(:noinlined_constprop_implicit), code)
    end

    let code = get_code(M.force_noinline_constprop_explicit)
        @test any(isinvoke(:inlined_constprop_explicit), code)
    end
    let code = get_code(M.force_noinline_constprop_implicit)
        @test any(isinvoke(:inlined_constprop_implicit), code)
    end

    let code = get_code(M.nested, (Int,Int))
        @test count(isinvoke(:notinlined), code) == 1
    end
end

# force constant-prop' for `setproperty!`
# https://github.com/JuliaLang/julia/pull/41882
let code = @eval Module() begin
        # if we don't force constant-prop', `T = fieldtype(Foo, ::Symbol)` will be union-split to
        # `Union{Type{Any},Type{Int}` and it will make `convert(T, nothing)` too costly
        # and it leads to inlining failure
        mutable struct Foo
            val
            _::Int
        end

        function setter(xs)
            for x in xs
                x.val = nothing
            end
        end

        $get_code(setter, (Vector{Foo},))
    end

    @test !any(isinvoke(:setproperty!), code)
end

# Issue #41299 - inlining deletes error check in :>
g41299(f::Tf, args::Vararg{Any,N}) where {Tf,N} = f(args...)
@test_throws TypeError g41299(>:, 1, 2)

# https://github.com/JuliaLang/julia/issues/42078
# idempotency of callsite inling
function getcache(mi::Core.MethodInstance)
    cache = Core.Compiler.code_cache(Core.Compiler.NativeInterpreter())
    codeinf = Core.Compiler.get(cache, mi, nothing)
    return isnothing(codeinf) ? nothing : codeinf
end
@noinline f42078(a) = sum(sincos(a))
let
    ninlined = let
        code = get_code((Int,)) do a
            @inline f42078(a)
        end
        @test all(!isinvoke(:f42078), code)
        length(code)
    end

    let # codegen will discard the source because it's not supposed to be inlined in general context
        a = 42
        f42078(a)
    end
    let # make sure to discard the inferred source
        specs = collect(only(methods(f42078)).specializations)
        mi = specs[findfirst(!isnothing, specs)]::Core.MethodInstance
        codeinf = getcache(mi)::Core.CodeInstance
        codeinf.inferred = nothing
    end

    let # inference should re-infer `f42078(::Int)` and we should get the same code
        code = get_code((Int,)) do a
            @inline f42078(a)
        end
        @test all(!isinvoke(:f42078), code)
        @test ninlined == length(code)
    end
end

begin
    # more idempotency of callsite inling
    # -----------------------------------
    # this test case requires forced constant propagation for callsite inlined function call,
    # particularly, in the following example, the inlinear will look up `+ₚ(::Point, ::Const(Point(2.25, 4.75)))`
    # and the callsite inlining needs the corresponding constant result to exist in the local cache

    struct Point
        x::Float64
        y::Float64
    end
    @noinline a::Point +ₚ b::Point = Point(a.x + b.x, a.y + b.y)

    function compute_idem_n(n)
        a = Point(1.5, 2.5)
        b = Point(2.25, 4.75)
        for i in 0:(n-1)
            a = @inline (a +ₚ b) +ₚ b
        end
        return a.x, a.y
    end
    let src = code_typed1(compute_idem_n, (Int,))
        @test count(isinvoke(:+ₚ), src.code) == 0 # successful inlining
    end

    function compute_idem_n(n)
        a = Point(1.5, 2.5)
        b = Point(2.25, 4.75)
        for i in 0:(n-1)
            a = (a +ₚ b) +ₚ b
        end
        return a.x, a.y
    end
    let src = code_typed1(compute_idem_n, (Int,))
        @test count(isinvoke(:+ₚ), src.code) == 2 # no inlining
    end

    compute_idem_n(42) # this execution should discard the cache of `+ₚ` since it's declared as `@noinline`

    function compute_idem_n(n)
        a = Point(1.5, 2.5)
        b = Point(2.25, 4.75)
        for i in 0:(n-1)
            @inline a = (a +ₚ b) +ₚ b
        end
        return a.x, a.y
    end
    let src = code_typed1(compute_idem_n, (Int,))
        @test count(isinvoke(:+ₚ), src.code) == 0 # no inlining !?
    end
end

# https://github.com/JuliaLang/julia/issues/42246
@test mktempdir() do dir
    cd(dir) do
        code = quote
            issue42246() = @noinline IOBuffer("a")
            let
                ci, rt = only(code_typed(issue42246))
                if any(ci.code) do stmt
                       Meta.isexpr(stmt, :invoke) &&
                       stmt.args[1].def.name === nameof(IOBuffer)
                   end
                    exit(0)
                else
                    exit(1)
               end
            end
        end |> string
        cmd = `$(Base.julia_cmd()) --code-coverage=tmp.info -e $code`
        success(pipeline(Cmd(cmd); stdout=stdout, stderr=stderr))
    end
end

# Issue #42264 - crash on certain union splits
let f(x) = (x...,)
    # Test splatting with a Union of non-{Tuple, SimpleVector} types that require creating new `iterate` calls
    # in inlining. For this particular case, we're relying on `iterate(::CaretesianIndex)` throwing an error, such
    # the the original apply call is not union-split, but the inserted `iterate` call is.
    @test code_typed(f, Tuple{Union{Int64, CartesianIndex{1}, CartesianIndex{3}}})[1][2] == Tuple{Int64}
end

# https://github.com/JuliaLang/julia/issues/42754
# inline union-split constant-prop'ed results
mutable struct X42754
    # NOTE in order to confuse `fieldtype_tfunc`, we need to have at least two fields with different types
    a::Union{Nothing, Int}
    b::Symbol
end
let src = code_typed1((X42754, Union{Nothing,Int})) do x, a
        # this `setproperty` call would be union-split and constant-prop will happen for
        # each signature: inlining would fail if we don't use constant-prop'ed source
        # since the approximate inlining cost of `convert(fieldtype(X, sym), a)` would
        # end up very high if we don't propagate `sym::Const(:a)`
        x.a = a
        x
    end
    @test all(src.code) do @nospecialize x
        !(isinvoke(:setproperty!, x) || iscall((src, setproperty!), x))
    end
end

import Base: @constprop

# test union-split callsite with successful and unsuccessful constant-prop' results
# (also for https://github.com/JuliaLang/julia/issues/43287)
@constprop :aggressive @inline f42840(cond::Bool, xs::Tuple, a::Int) =  # should be successful, and inlined with constant prop' result
    cond ? xs[a] : @noinline(length(xs))
@constprop :none @noinline f42840(::Bool, xs::AbstractVector, a::Int) = # should be unsuccessful, but still statically resolved
    xs[a]
let src = code_typed((Union{Tuple{Int,Int,Int}, Vector{Int}},)) do xs
             f42840(true, xs, 2)
         end |> only |> first
    # `f43287(true, xs::Tuple{Int,Int,Int}, 2)` => `getfield(xs, 2)`
    # `f43287(true, xs::Vector{Int}, 2)` => `:invoke f43287(true, xs, 2)`
    @test count(iscall((src, getfield)), src.code) == 1
    @test count(isinvoke(:length), src.code) == 0
    @test count(isinvoke(:f42840), src.code) == 1
end
# a bit weird, but should handle this kind of case as well
@constprop :aggressive @noinline g42840(xs, a::Int) = xs[a]         # should be successful, but only statically resolved
@constprop :none @inline g42840(xs::AbstractVector, a::Int) = xs[a] # should be unsuccessful, still inlined
let src = code_typed((Union{Tuple{Int,Int,Int}, Vector{Int}},)) do xs
        g42840(xs, 2)
    end |> only |> first
    # `(xs::Vector{Int})[a::Const(2)]` => `Base.arrayref(true, xs, 2)`
    @test count(iscall((src, Base.arrayref)), src.code) == 1
    @test count(isinvoke(:g42840), src.code) == 1
end

# test single, non-dispatchtuple callsite inlining

@constprop :none @inline test_single_nondispatchtuple(@nospecialize(t)) =
    isa(t, DataType) && t.name === Type.body.name
let
    src = code_typed1((Any,)) do x
        test_single_nondispatchtuple(x)
    end
    @test all(src.code) do @nospecialize x
        !(isinvoke(:test_single_nondispatchtuple, x) || iscall((src, test_single_nondispatchtuple), x))
    end
end

@constprop :aggressive @inline test_single_nondispatchtuple(c, @nospecialize(t)) =
    c && isa(t, DataType) && t.name === Type.body.name
let
    src = code_typed1((Any,)) do x
        test_single_nondispatchtuple(true, x)
    end
    @test all(src.code) do @nospecialize(x)
        !(isinvoke(:test_single_nondispatchtuple, x) || iscall((src, test_single_nondispatchtuple), x))
    end
end

# validate inlining processing

@constprop :none @inline validate_unionsplit_inlining(@nospecialize(t)) = throw("invalid inlining processing detected")
@constprop :none @noinline validate_unionsplit_inlining(i::Integer) = (println(IOBuffer(), "prevent inlining"); false)
let
    invoke(xs) = validate_unionsplit_inlining(xs[1])
    @test invoke(Any[10]) === false
end

@constprop :aggressive @inline validate_unionsplit_inlining(c, @nospecialize(t)) = c && throw("invalid inlining processing detected")
@constprop :aggressive @noinline validate_unionsplit_inlining(c, i::Integer) = c && (println(IOBuffer(), "prevent inlining"); false)
let
    invoke(xs) = validate_unionsplit_inlining(true, xs[1])
    @test invoke(Any[10]) === false
end

# test union-split, non-dispatchtuple callsite inlining

@constprop :none @noinline abstract_unionsplit(@nospecialize x::Any) = Base.inferencebarrier(:Any)
@constprop :none @noinline abstract_unionsplit(@nospecialize x::Number) = Base.inferencebarrier(:Number)
let src = code_typed1((Any,)) do x
        abstract_unionsplit(x)
    end
    @test count(isinvoke(:abstract_unionsplit), src.code) == 2
    @test count(iscall((src, abstract_unionsplit)), src.code) == 0 # no fallback dispatch
end
let src = code_typed1((Union{Type,Number},)) do x
        abstract_unionsplit(x)
    end
    @test count(isinvoke(:abstract_unionsplit), src.code) == 2
    @test count(iscall((src, abstract_unionsplit)), src.code) == 0 # no fallback dispatch
end

@constprop :none @noinline abstract_unionsplit_fallback(@nospecialize x::Type) = Base.inferencebarrier(:Any)
@constprop :none @noinline abstract_unionsplit_fallback(@nospecialize x::Number) = Base.inferencebarrier(:Number)
let src = code_typed1((Any,)) do x
        abstract_unionsplit_fallback(x)
    end
    @test count(isinvoke(:abstract_unionsplit_fallback), src.code) == 2
    @test count(iscall((src, abstract_unionsplit_fallback)), src.code) == 1 # fallback dispatch
end
let src = code_typed1((Union{Type,Number},)) do x
        abstract_unionsplit_fallback(x)
    end
    @test count(isinvoke(:abstract_unionsplit_fallback), src.code) == 2
    @test count(iscall((src, abstract_unionsplit)), src.code) == 0 # no fallback dispatch
end

@constprop :aggressive @inline abstract_unionsplit(c, @nospecialize x::Any) = (c && println("erase me"); typeof(x))
@constprop :aggressive @inline abstract_unionsplit(c, @nospecialize x::Number) = (c && println("erase me"); typeof(x))
let src = code_typed1((Any,)) do x
        abstract_unionsplit(false, x)
    end
    @test count(iscall((src, typeof)), src.code) == 2
    @test count(isinvoke(:println), src.code) == 0
    @test count(iscall((src, println)), src.code) == 0
    @test count(iscall((src, abstract_unionsplit)), src.code) == 0 # no fallback dispatch
end
let src = code_typed1((Union{Type,Number},)) do x
        abstract_unionsplit(false, x)
    end
    @test count(iscall((src, typeof)), src.code) == 2
    @test count(isinvoke(:println), src.code) == 0
    @test count(iscall((src, println)), src.code) == 0
    @test count(iscall((src, abstract_unionsplit)), src.code) == 0 # no fallback dispatch
end

@constprop :aggressive @inline abstract_unionsplit_fallback(c, @nospecialize x::Type) = (c && println("erase me"); typeof(x))
@constprop :aggressive @inline abstract_unionsplit_fallback(c, @nospecialize x::Number) = (c && println("erase me"); typeof(x))
let src = code_typed1((Any,)) do x
        abstract_unionsplit_fallback(false, x)
    end
    @test count(iscall((src, typeof)), src.code) == 2
    @test count(isinvoke(:println), src.code) == 0
    @test count(iscall((src, println)), src.code) == 0
    @test count(iscall((src, abstract_unionsplit_fallback)), src.code) == 1 # fallback dispatch
end
let src = code_typed1((Union{Type,Number},)) do x
        abstract_unionsplit_fallback(false, x)
    end
    @test count(iscall((src, typeof)), src.code) == 2
    @test count(isinvoke(:println), src.code) == 0
    @test count(iscall((src, println)), src.code) == 0
    @test count(iscall((src, abstract_unionsplit)), src.code) == 0 # no fallback dispatch
end

abstract_diagonal_dispatch(x::Int, y::Int) = 1
abstract_diagonal_dispatch(x::Real, y::Int) = 2
abstract_diagonal_dispatch(x::Int, y::Real) = 3
function test_abstract_diagonal_dispatch(xs)
    @test abstract_diagonal_dispatch(xs[1], xs[2]) == 1
    @test abstract_diagonal_dispatch(xs[3], xs[4]) == 3
    @test abstract_diagonal_dispatch(xs[5], xs[6]) == 2
    @test_throws MethodError abstract_diagonal_dispatch(xs[7], xs[8])
end
test_abstract_diagonal_dispatch(Any[
    1, 1,    # => 1
    1, 1.0,  # => 3
    1.0, 1,  # => 2
    1.0, 1.0 # => MethodError
])

constrained_dispatch(x::T, y::T) where T<:Real = 0
let src = code_typed1((Real,Real,)) do x, y
        constrained_dispatch(x, y)
    end
    @test any(iscall((src, constrained_dispatch)), src.code) # should account for MethodError
end
@test_throws MethodError let
    x, y = 1.0, 1
    constrained_dispatch(x, y)
end

# issue 43104

@inline isGoodType(@nospecialize x::Type) =
    x !== Any && !(@noinline Base.has_free_typevars(x))
let # aggressive inlining of single, abstract method match
    src = code_typed((Type, Any,)) do x, y
        isGoodType(x), isGoodType(y)
    end |> only |> first
    # both callsites should be inlined
    @test count(isinvoke(:has_free_typevars), src.code) == 2
    # `isGoodType(y::Any)` isn't fully covered, thus a runtime type check and fallback dynamic dispatch should be inserted
    @test count(iscall((src,isGoodType)), src.code) == 1
end

@inline isGoodType2(cnd, @nospecialize x::Type) =
    x !== Any && !(@noinline (cnd ? Core.Compiler.isType : Base.has_free_typevars)(x))
let # aggressive inlining of single, abstract method match (with constant-prop'ed)
    src = code_typed((Type, Any,)) do x, y
        isGoodType2(true, x), isGoodType2(true, y)
    end |> only |> first
    # both callsite should be inlined with constant-prop'ed result
    @test count(isinvoke(:isType), src.code) == 2
    @test count(isinvoke(:has_free_typevars), src.code) == 0
    # `isGoodType(y::Any)` isn't fully convered, thus a runtime type check and fallback dynamic dispatch should be inserted
    @test count(iscall((src,isGoodType2)), src.code) == 1
end

@noinline function checkBadType!(@nospecialize x::Type)
    if x === Any || Base.has_free_typevars(x)
        println(x)
    end
    return nothing
end
let # aggressive static dispatch of single, abstract method match
    src = code_typed((Type, Any,)) do x, y
        checkBadType!(x), checkBadType!(y)
    end |> only |> first
    # both callsites should be resolved statically
    @test count(isinvoke(:checkBadType!), src.code) == 2
    # `checkBadType!(y::Any)` isn't fully covered, thus a runtime type check and fallback dynamic dispatch should be inserted
    @test count(iscall((src,checkBadType!)), src.code) == 1
end

@testset "late_inline_special_case!" begin
    let src = code_typed((Symbol,Any,Any)) do a, b, c
            TypeVar(a, b, c)
        end |> only |> first
        @test count(iscall((src,TypeVar)), src.code) == 0
        @test count(iscall((src,Core._typevar)), src.code) == 1
    end
    let src = code_typed((TypeVar,Any)) do a, b
            UnionAll(a, b)
        end |> only |> first
        @test count(iscall((src,UnionAll)), src.code) == 0
    end
end

# have_fma elimination inside ^
f_pow() = ^(2.0, -1.0)
@test fully_eliminated(f_pow, Tuple{})

# bug where Conditional wasn't being properly marked as ConstAPI
let
    @noinline fcond(a, b) = a === b
    ftest(a) = (fcond(a, nothing); a)
    @test fully_eliminated(ftest, Tuple{Bool})
end

# sqrt not considered volatile
f_sqrt() = sqrt(2)
@test fully_eliminated(f_sqrt, Tuple{})

# use constant prop' result even when the return type doesn't get refined
const Gx = Ref{Any}()
Base.@constprop :aggressive function conditional_escape!(cnd, x)
    if cnd
        Gx[] = x
    end
    return nothing
end
@test fully_eliminated((String,)) do x
    @invoke conditional_escape!(false::Any, x::Any)
end

@testset "strides for ReshapedArray (PR#44027)" begin
    # Type-based contiguous check
    a = vec(reinterpret(reshape,Int16,reshape(view(reinterpret(Int32,randn(10)),2:11),5,:)))
    f(a) = only(strides(a));
    @test fully_eliminated(f, Tuple{typeof(a)}) && f(a) == 1
end

@testset "elimination of `get_binding_type`" begin
    m = Module()
    @eval m begin
        global x::Int
        f() = Core.get_binding_type($m, :x)
        g() = Core.get_binding_type($m, :y)
    end

    @test fully_eliminated(m.f, Tuple{}; retval=Int)
    src = code_typed(m.g, ())[][1]
    @test count(iscall((src, Core.get_binding_type)), src.code) == 1
    @test m.g() === Any
end

# have_fma elimination inside ^
f_pow() = ^(2.0, -1.0)
@test fully_eliminated(f_pow, Tuple{})

# unused total, noinline function
@noinline function f_total_noinline(x)
    return x + 1.0
end
@noinline function f_voltatile_escape(ptr)
    unsafe_store!(ptr, 0)
end
function f_call_total_noinline_unused(x)
    f_total_noinline(x)
    return x
end
function f_call_volatile_escape(ptr)
    f_voltatile_escape(ptr)
    return ptr
end

@test fully_eliminated(f_call_total_noinline_unused, Tuple{Float64})
@test !fully_eliminated(f_call_volatile_escape, Tuple{Ptr{Int}})

let b = Expr(:block, (:(y += sin($x)) for x in randn(1000))...)
    @eval function f_sin_perf()
        y = 0.0
        $b
        y
    end
end
@test fully_eliminated(f_sin_perf, Tuple{})

# Test that we inline the constructor of something that is not const-inlineable
const THE_REF_NULL = Ref{Int}()
const THE_REF = Ref{Int}(0)
struct FooTheRef
    x::Ref
    FooTheRef(v) = new(v === nothing ? THE_REF_NULL : THE_REF)
end
let src = code_typed1() do
        FooTheRef(nothing)
    end
    @test count(isnew, src.code) == 1
end
let src = code_typed1() do
        FooTheRef(0)
    end
    @test count(isnew, src.code) == 1
end
let src = code_typed1() do
        @invoke FooTheRef(nothing::Any)
    end
    @test count(isnew, src.code) == 1
end
let src = code_typed1() do
        @invoke FooTheRef(0::Any)
    end
    @test count(isnew, src.code) == 1
end
@test fully_eliminated() do
    FooTheRef(nothing)
    nothing
end
@test fully_eliminated() do
    FooTheRef(0)
    nothing
end
@test fully_eliminated() do
    @invoke FooTheRef(nothing::Any)
    nothing
end
@test fully_eliminated() do
    @invoke FooTheRef(0::Any)
    nothing
end

# Test that the Core._apply_iterate bail path taints effects
function f_apply_bail(f)
    f(()...)
    return nothing
end
f_call_apply_bail(f) = f_apply_bail(f)
@test !fully_eliminated(f_call_apply_bail, Tuple{Function})

# Test that arraysize has proper effect modeling
@test fully_eliminated(M->(size(M, 2); nothing), Tuple{Matrix{Float64}})

# DCE of non-inlined callees
@noinline noninlined_dce_simple(a) = identity(a)
@test fully_eliminated((String,)) do s
    noninlined_dce_simple(s)
    nothing
end
@noinline noninlined_dce_new(a::String) = Some(a)
@test fully_eliminated((String,)) do s
    noninlined_dce_new(s)
    nothing
end
mutable struct SafeRef{T}
    x::T
end
Base.getindex(s::SafeRef) = getfield(s, 1)
Base.setindex!(s::SafeRef, x) = setfield!(s, 1, x)
@noinline noninlined_dce_new(a::Symbol) = SafeRef(a)
@test fully_eliminated((Symbol,)) do s
    noninlined_dce_new(s)
    nothing
end
# should be resolved once we merge https://github.com/JuliaLang/julia/pull/43923
@test_broken fully_eliminated((Union{Symbol,String},)) do s
    noninlined_dce_new(s)
    nothing
end

# Test that ambigous calls don't accidentally get nothrow effect
ambig_effect_test(a::Int, b) = 1
ambig_effect_test(a, b::Int) = 1
ambig_effect_test(a, b) = 1
global ambig_unknown_type_global=1
@noinline function conditionally_call_ambig(b::Bool, a)
    if b
        ambig_effect_test(a, ambig_unknown_type_global)
    end
    return 0
end
function call_call_ambig(b::Bool)
    conditionally_call_ambig(b, 1)
    return 1
end
@test !fully_eliminated(call_call_ambig, Tuple{Bool})

# Test that a missing methtable identification gets tainted
# appropriately
struct FCallback; f::Union{Nothing, Function}; end
f_invoke_callback(fc) = let f=fc.f; (f !== nothing && f(); nothing); end
function f_call_invoke_callback(f::FCallback)
    f_invoke_callback(f)
    return nothing
end
@test !fully_eliminated(f_call_invoke_callback, Tuple{FCallback})

# https://github.com/JuliaLang/julia/issues/41694
Base.@assume_effects :terminates_globally function issue41694(x)
    res = 1
    1 < x < 20 || throw("bad")
    while x > 1
        res *= x
        x -= 1
    end
    return res
end
@test fully_eliminated() do
    issue41694(2)
end

Base.@assume_effects :terminates_globally function recur_termination1(x)
    x == 1 && return 1
    1 < x < 20 || throw("bad")
    return x * recur_termination1(x-1)
end
@test fully_eliminated() do
    recur_termination1(12)
end
Base.@assume_effects :terminates_globally function recur_termination21(x)
    x == 1 && return 1
    1 < x < 20 || throw("bad")
    return recur_termination22(x)
end
recur_termination22(x) = x * recur_termination21(x-1)
@test fully_eliminated() do
    recur_termination21(12) + recur_termination22(12)
end

const ___CONST_DICT___ = Dict{Any,Any}(Symbol(c) => i for (i, c) in enumerate('a':'z'))
Base.@assume_effects :foldable concrete_eval(
    f, args...; kwargs...) = f(args...; kwargs...)
@test fully_eliminated() do
    concrete_eval(getindex, ___CONST_DICT___, :a)
end

# https://github.com/JuliaLang/julia/issues/44732
struct Component44732
    v
end
struct Container44732
    x::Union{Nothing,Component44732}
end

# NOTE make sure to prevent inference bail out
validate44732(::Component44732) = nothing
validate44732(::Any) = error("don't erase this error!")

function issue44732(c::Container44732)
    validate44732(c.x)
    return nothing
end

let src = code_typed1(issue44732, (Container44732,))
    @test any(isinvoke(:validate44732), src.code)
end
@test_throws ErrorException("don't erase this error!") issue44732(Container44732(nothing))

global x44200::Int = 0
function f44200()
    global x44200 = 0
    while x44200 < 10
        x44200 += 1
    end
    x44200
end
let src = code_typed1(f44200)
    @test count(x -> isa(x, Core.PiNode), src.code) == 0
end

# Test that peeling off one case from (::Any) doesn't introduce
# a dynamic dispatch.
@noinline f_peel(x::Int) = Base.inferencebarrier(1)
@noinline f_peel(@nospecialize(x::Any)) = Base.inferencebarrier(2)
g_call_peel(x) = f_peel(x)
let src = code_typed1(g_call_peel, Tuple{Any})
    @test count(isinvoke(:f_peel), src.code) == 2
end

const my_defined_var = 42
@test fully_eliminated((); retval=42) do
    getglobal(@__MODULE__, :my_defined_var, :monotonic)
end
@test !fully_eliminated() do
    getglobal(@__MODULE__, :my_defined_var, :foo)
end

# Test for deletion of value-dependent control flow that is apparent
# at inference time, but hard to delete later.
function maybe_error_int(x::Int)
    if x > 2
        Base.donotdelete(Base.inferencebarrier(x))
        error()
    end
    return 1
end
@test fully_eliminated() do
    return maybe_error_int(1)
end

# Test that effect modeling for return_type doesn't incorrectly pick
# up the effects of the function being analyzed
function f_throws()
    error()
end

@noinline function return_type_unused(x)
    Core.Compiler.return_type(f_throws, Tuple{})
    return x+1
end

@test fully_eliminated(Tuple{Int}) do x
    return_type_unused(x)
    return nothing
end

# Test that inlining doesn't accidentally delete a bad return_type call
f_bad_return_type() = Core.Compiler.return_type(+, 1, 2)
@test_throws MethodError f_bad_return_type()

# Test that inlining doesn't leave useless globalrefs around
f_ret_nothing(x) = (Base.donotdelete(x); return nothing)
let src = code_typed1(Tuple{Int}) do x
        f_ret_nothing(x)
        return 1
    end
    @test count(x -> isa(x, Core.GlobalRef) && x.name === :nothing, src.code) == 0
end

# Test that we can inline a finalizer for a struct that does not otherwise escape
@noinline nothrow_side_effect(x) =
    @Base.assume_effects :total !:effect_free @ccall jl_(x::Any)::Cvoid

mutable struct DoAllocNoEscape
    function DoAllocNoEscape()
        finalizer(new()) do this
            nothrow_side_effect(nothing)
        end
    end
end
let src = code_typed1() do
        for i = 1:1000
            DoAllocNoEscape()
        end
    end
    @test count(isnew, src.code) == 0
end

# Test that a case when `Core.finalizer` is registered interprocedurally,
# but still eligible for SROA after inlining
mutable struct DoAllocNoEscapeInter end

let src = code_typed1() do
        for i = 1:1000
            obj = DoAllocNoEscapeInter()
            finalizer(obj) do this
                nothrow_side_effect(nothing)
            end
        end
    end
    @test count(isnew, src.code) == 0
end

function register_finalizer!(obj)
    finalizer(obj) do this
        nothrow_side_effect(nothing)
    end
end
let src = code_typed1() do
        for i = 1:1000
            obj = DoAllocNoEscapeInter()
            register_finalizer!(obj)
        end
    end
    @test count(isnew, src.code) == 0
end

function genfinalizer(val)
    return function (this)
        nothrow_side_effect(val)
    end
end
let src = code_typed1() do
        for i = 1:1000
            obj = DoAllocNoEscapeInter()
            finalizer(genfinalizer(nothing), obj)
        end
    end
    @test count(isnew, src.code) == 0
end

# Test that we can inline a finalizer that just returns a constant value
mutable struct DoAllocConst
    function DoAllocConst()
        finalizer(new()) do this
            return nothing
        end
    end
end
let src = code_typed1() do
        for i = 1:1000
            DoAllocConst()
        end
    end
    @test count(isnew, src.code) == 0
end

# Test that finalizer elision doesn't cause a throw to be inlined into a function
# that shouldn't have it
const finalizer_should_throw = Ref{Bool}(true)
mutable struct DoAllocFinalizerThrows
    function DoAllocFinalizerThrows()
        finalizer(new()) do this
            finalizer_should_throw[] && error("Unexpected finalizer throw")
        end
    end
end

function f_finalizer_throws()
    prev = GC.enable(false)
    for i = 1:100
        DoAllocFinalizerThrows()
    end
    finalizer_should_throw[] = false
    GC.enable(prev)
    GC.gc()
    return true
end

@test f_finalizer_throws()

# Test finalizers with static parameters
mutable struct DoAllocNoEscapeSparam{T}
    x::T
    function finalizer_sparam(d::DoAllocNoEscapeSparam{T}) where {T}
        nothrow_side_effect(nothing)
        nothrow_side_effect(T)
    end
    function DoAllocNoEscapeSparam{T}(x::T) where {T}
        finalizer(finalizer_sparam, new{T}(x))
    end
end
DoAllocNoEscapeSparam(x::T) where {T} = DoAllocNoEscapeSparam{T}(x)
let src = code_typed1(Tuple{Any}) do x
        for i = 1:1000
            DoAllocNoEscapeSparam(x)
        end
    end
    # This requires more inlining enhancments. For now just make sure this
    # doesn't error.
    @test count(isnew, src.code) in (0, 1) # == 0
end

# Test noinline finalizer
@noinline function noinline_finalizer(d)
    nothrow_side_effect(nothing)
end
mutable struct DoAllocNoEscapeNoInline
    function DoAllocNoEscapeNoInline()
        finalizer(noinline_finalizer, new())
    end
end
let src = code_typed1() do
        for i = 1:1000
            DoAllocNoEscapeNoInline()
        end
    end
    @test count(isnew, src.code) == 1
    @test count(isinvoke(:noinline_finalizer), src.code) == 1
end

# Test that we resolve a `finalizer` call that we don't handle currently
mutable struct DoAllocNoEscapeBranch
    val::Int
    function DoAllocNoEscapeBranch(val::Int)
        finalizer(new(val)) do this
            if this.val > 500
                nothrow_side_effect(this.val)
            else
                nothrow_side_effect(nothing)
            end
        end
    end
end
let src = code_typed1() do
        for i = 1:1000
            DoAllocNoEscapeBranch(i)
        end
    end
    @test !any(iscall((src, Core.finalizer)), src.code)
    @test !any(isinvoke(:finalizer), src.code)
end

# optimize `[push!|pushfirst!](::Vector{Any}, x...)`
@testset "optimize `$f(::Vector{Any}, x...)`" for f = Any[push!, pushfirst!]
    @eval begin
        let src = code_typed1((Vector{Any}, Any)) do xs, x
                $f(xs, x)
            end
            @test count(iscall((src, $f)), src.code) == 0
            @test count(src.code) do @nospecialize x
                isa(x, Core.GotoNode) ||
                isa(x, Core.GotoIfNot) ||
                iscall((src, getfield))(x)
            end == 0 # no loop should be involved for the common single arg case
        end
        let src = code_typed1((Vector{Any}, Any, Any)) do xs, x, y
                $f(xs, x, y)
            end
            @test count(iscall((src, $f)), src.code) == 0
        end
        let xs = Any[]
            $f(xs, :x, "y", 'z')
            @test xs[1] === :x
            @test xs[2] == "y"
            @test xs[3] === 'z'
        end
    end
end

# https://github.com/JuliaLang/julia/issues/45050
@testset "propagate :meta annotations to keyword sorter methods" begin
    # @inline, @noinline, @constprop
    let @inline f(::Any; x::Int=1) = 2x
        @test ccall(:jl_ir_flag_inlineable, Bool, (Any,), only(methods(f)).source)
        @test ccall(:jl_ir_flag_inlineable, Bool, (Any,), only(methods(Core.kwfunc(f))).source)
    end
    let @noinline f(::Any; x::Int=1) = 2x
        @test !ccall(:jl_ir_flag_inlineable, Bool, (Any,), only(methods(f)).source)
        @test !ccall(:jl_ir_flag_inlineable, Bool, (Any,), only(methods(Core.kwfunc(f))).source)
    end
    let Base.@constprop :aggressive f(::Any; x::Int=1) = 2x
        @test Core.Compiler.is_aggressive_constprop(only(methods(f)))
        @test Core.Compiler.is_aggressive_constprop(only(methods(Core.kwfunc(f))))
    end
    let Base.@constprop :none f(::Any; x::Int=1) = 2x
        @test Core.Compiler.is_no_constprop(only(methods(f)))
        @test Core.Compiler.is_no_constprop(only(methods(Core.kwfunc(f))))
    end
    # @nospecialize
    let f(@nospecialize(A::Any); x::Int=1) = 2x
        @test only(methods(f)).nospecialize == 1
        @test only(methods(Core.kwfunc(f))).nospecialize == 4
    end
    let f(::Any; x::Int=1) = (@nospecialize; 2x)
        @test only(methods(f)).nospecialize == -1
        @test only(methods(Core.kwfunc(f))).nospecialize == -1
    end
    # Base.@assume_effects
    let Base.@assume_effects :notaskstate f(::Any; x::Int=1) = 2x
        @test Core.Compiler.decode_effects_override(only(methods(f)).purity).notaskstate
        @test Core.Compiler.decode_effects_override(only(methods(Core.kwfunc(f))).purity).notaskstate
    end
    # propagate multiple metadata also
    let @inline Base.@assume_effects :notaskstate Base.@constprop :aggressive f(::Any; x::Int=1) = (@nospecialize; 2x)
        @test ccall(:jl_ir_flag_inlineable, Bool, (Any,), only(methods(f)).source)
        @test Core.Compiler.is_aggressive_constprop(only(methods(f)))
        @test ccall(:jl_ir_flag_inlineable, Bool, (Any,), only(methods(Core.kwfunc(f))).source)
        @test Core.Compiler.is_aggressive_constprop(only(methods(Core.kwfunc(f))))
        @test only(methods(f)).nospecialize == -1
        @test only(methods(Core.kwfunc(f))).nospecialize == -1
        @test Core.Compiler.decode_effects_override(only(methods(f)).purity).notaskstate
        @test Core.Compiler.decode_effects_override(only(methods(Core.kwfunc(f))).purity).notaskstate
    end
end
