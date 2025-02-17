# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using Base.Meta
using Core: ReturnNode

include("irutils.jl")
include("newinterp.jl")

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
        if isa(e, Core.SlotNumber)
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
@test_throws UndefVarError(:y, :local) bar12620()

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
    @test !any(e->(isa(e,Expr) && (e.head === :invoke && e.args[1].def.def.name === :kwfunc)), c)
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

    (src, _) = only(code_typed(sum27403, Tuple{Vector{Int}}))
    @test !any(src.code) do x
        x isa Expr && x.head === :invoke && !(x.args[2] in (Core.GlobalRef(Base, :throw_boundserror), Base.throw_boundserror))
    end
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

# Test that inlining can inline _apply_iterate of builtins/_apply_iterate on SimpleVectors
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

using .Compiler: is_declared_inline, is_declared_noinline

@testset "is_declared_[no]inline" begin
    @test is_declared_inline(only(methods(@inline x -> x)))
    @test is_declared_inline(only(methods(x -> (@inline; x))))
    @test is_declared_inline(only(methods(@inline function f(x) x end)))
    @test is_declared_inline(only(methods(function f(x) @inline; x end)))
    @test is_declared_inline(only(methods() do x @inline; x end))
    @test is_declared_noinline(only(methods(@noinline x -> x)))
    @test is_declared_noinline(only(methods(x -> (@noinline; x))))
    @test is_declared_noinline(only(methods(@noinline function f(x) x end)))
    @test is_declared_noinline(only(methods(function f(x) @noinline; x end)))
    @test is_declared_noinline(only(methods() do x @noinline; x end))
    @test !is_declared_inline(only(methods(x -> x)))
    @test !is_declared_noinline(only(methods(x -> x)))
    @test !is_declared_inline(only(methods(function f(x) x end)))
    @test !is_declared_noinline(only(methods(function f(x) x end)))
    @test !is_declared_inline(only(methods() do x x end))
    @test !is_declared_noinline(only(methods() do x x end))
end

using .Compiler: is_inlineable, set_inlineable!

@testset "basic set_inlineable! functionality" begin
    ci = code_typed1() do
        x -> x
    end
    set_inlineable!(ci, true)
    @test is_inlineable(ci)
    set_inlineable!(ci, false)
    @test !is_inlineable(ci)
    @test_throws MethodError set_inlineable!(ci, 5)
end

const _a_global_array = [1]
f_inline_global_getindex() = _a_global_array[1]
let ci = code_typed(f_inline_global_getindex, Tuple{})[1].first
    @test any(x->(isexpr(x, :call) && x.args[1] in (GlobalRef(Base, :memoryrefget), Base.memoryrefget)), ci.code)
end

# Issue #29114 & #36087 - Inlining of non-tuple splats
f_29115(x) = (x...,)
@test @allocated(f_29115(1)) == 0
@test @allocated(f_29115(1=>2)) == 0
let src = code_typed(f_29115, Tuple{Int64}) |> only |> first
    @test iscall((src, tuple), src.code[end-1])
end
let src = code_typed(f_29115, Tuple{Pair{Int64, Int64}}) |> only |> first
    @test iscall((src, tuple), src.code[end-1])
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
@test fully_eliminated(NonIsBitsDims, (); retval=NonIsBitsDims())

struct NonIsBitsDimsUndef
    dims::NTuple{N, Int} where N
    NonIsBitsDimsUndef() = new()
end
@test Compiler.is_inlineable_constant(NonIsBitsDimsUndef())
@test !Compiler.is_inlineable_constant((("a"^1000, "b"^1000), nothing))

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

        function force_inline_constprop_cached1()
            r1 =         noinlined_constprop_implicit(0)
            r2 = @inline noinlined_constprop_implicit(0)
            return (r1, r2)
        end
        function force_inline_constprop_cached2()
            r1 = @inline noinlined_constprop_implicit(0)
            r2 =         noinlined_constprop_implicit(0)
            return (r1, r2)
        end

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
    let code = get_code(M.force_inline_constprop_cached1)
        @test count(isinvoke(:noinlined_constprop_implicit), code) == 1
    end
    let code = get_code(M.force_inline_constprop_cached2)
        @test count(isinvoke(:noinlined_constprop_implicit), code) == 1
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

@noinline fresh_edge_noinlined(a::Integer) = unresolvable(a)
let src = code_typed1((Integer,)) do x
        @inline fresh_edge_noinlined(x)
    end
    @test count(iscall((src, fresh_edge_noinlined)), src.code) == 0
end
let src = code_typed1((Integer,)) do x
        @inline fresh_edge_noinlined(x)
    end
    @test count(iscall((src, fresh_edge_noinlined)), src.code) == 0 # should be idempotent
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
# idempotency of callsite inlining
function getcache(mi::Core.MethodInstance)
    cache = Compiler.code_cache(Compiler.NativeInterpreter())
    codeinst = Compiler.get(cache, mi, nothing)
    return isnothing(codeinst) ? nothing : codeinst
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
        mi = only(methods(f42078)).specializations::Core.MethodInstance
        codeinst = getcache(mi)::Core.CodeInstance
        @atomic codeinst.inferred = nothing
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
    # more idempotency of callsite inlining
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
mktempdir() do dir
    cd(dir) do
        code = """
            issue42246() = @noinline IOBuffer("a")
            let
                ci, rt = only(code_typed(issue42246))
                if any(ci.code) do stmt
                       Meta.isexpr(stmt, :invoke) &&
                       stmt.args[1].def.def.name === nameof(IOBuffer)
                   end
                    exit(0)
                else
                    exit(1)
               end
            end
            """
        cmd = `$(Base.julia_cmd()) --code-coverage=tmp.info -e $code`
        @test success(pipeline(cmd; stdout, stderr))
    end
end

# callsite inlining with cached frames
issue49823_events = @NamedTuple{evid::Int8, base_time::Float64}[
    (evid = 1, base_time = 0.0), (evid = -1, base_time = 0.0)]
issue49823_fl1(t, events) = @inline findlast(x -> x.evid ∈ (1, 4) && x.base_time <= t, events)
issue49823_fl3(t, events) = @inline findlast(x -> any(==(x.evid), (1,4)) && x.base_time <= t, events)
issue49823_fl5(t, events) = begin
    f = let t=t
        x -> x.evid ∈ (1, 4) && x.base_time <= t
    end
    @inline findlast(f, events)
end
let src = @code_typed1 issue49823_fl1(0.0, issue49823_events)
    @test count(isinvoke(:findlast), src.code) == 0 # successful inlining
end
let src = @code_typed1 issue49823_fl3(0.0, issue49823_events)
    @test count(isinvoke(:findlast), src.code) == 0 # successful inlining
end
let src = @code_typed1 issue49823_fl5(0.0, issue49823_events)
    @test count(isinvoke(:findlast), src.code) == 0 # successful inlining
end

# Issue #42264 - crash on certain union splits
let f(x) = (x...,)
    # Test splatting with a Union of non-{Tuple, SimpleVector} types that require creating new `iterate` calls
    # in inlining. For this particular case, we're relying on `iterate(::CaretesianIndex)` throwing an error, such
    # that the original apply call is not union-split, but the inserted `iterate` call is.
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
    # `(xs::Vector{Int})[a::Const(2)]`
    @test count(iscall((src, Base.memoryrefget)), src.code) == 1
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
    @test count(iscall((src, Core.throw_methoderror)), src.code) == 1 # fallback method error
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
    @test count(iscall((src, Core.throw_methoderror)), src.code) == 1 # fallback method error
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
_has_free_typevars(t) = ccall(:jl_has_free_typevars, Cint, (Any,), t) != 0
@inline isGoodType(@nospecialize x::Type) =
    x !== Any && !(@noinline _has_free_typevars(x))
let # aggressive inlining of single, abstract method match
    src = code_typed((Type, Any,)) do x, y
        isGoodType(x), isGoodType(y)
    end |> only |> first
    # both callsites should be inlined
    @test count(isinvoke(:_has_free_typevars), src.code) == 2
    # `isGoodType(y::Any)` isn't fully covered, so the fallback is a method error
    @test count(iscall((src, Core.throw_methoderror)), src.code) == 1 # fallback method error
end

@inline isGoodType2(cnd, @nospecialize x::Type) =
    x !== Any && !(@noinline (cnd ? Compiler.isType : _has_free_typevars)(x))
let # aggressive inlining of single, abstract method match (with constant-prop'ed)
    src = code_typed((Type, Any,)) do x, y
        isGoodType2(true, x), isGoodType2(true, y)
    end |> only |> first
    # both callsite should be inlined with constant-prop'ed result
    @test count(isinvoke(:isType), src.code) == 2
    @test count(isinvoke(:_has_free_typevars), src.code) == 0
    # `isGoodType(y::Any)` isn't fully covered, thus a MethodError gets inserted
    @test count(iscall((src, Core.throw_methoderror)), src.code) == 1 # fallback method error
end

@noinline function checkBadType!(@nospecialize x::Type)
    if x === Any || _has_free_typevars(x)
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
    # `checkBadType!(y::Any)` isn't fully covered, thus a MethodError gets inserted
    @test count(iscall((src, Core.throw_methoderror)), src.code) == 1 # fallback method error
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
    # test >:
    let src = code_typed((Any,Any)) do x, y
            x >: y
        end |> only |> first
        idx = findfirst(iscall((src,<:)), src.code)
        @test idx !== nothing
        @test src.code[idx].args[2:3] == Any[#=y=#Argument(3), #=x=#Argument(2)]
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
@test fully_eliminated((Union{Symbol,String},)) do s
    noninlined_dce_new(s)
    nothing
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
@test fully_eliminated(; retval=42) do
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

# Test that inlining doesn't accidentally delete a bad return_type call
f_bad_return_type() = Compiler.return_type(+, 1, 2)
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
    Base.@assume_effects :total !:effect_free @ccall jl_(x::Any)::Cvoid
@test Compiler.is_finalizer_inlineable(Base.infer_effects(nothrow_side_effect, (Nothing,)))

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
    x
    @inline function finalizer_sparam(d::DoAllocNoEscapeSparam{T}) where {T}
        nothrow_side_effect(nothing)
        nothrow_side_effect(T)
    end
    @inline function DoAllocNoEscapeSparam(x::T) where {T}
        finalizer(finalizer_sparam, new{T}(x))
    end
end
let src = code_typed1(Tuple{Any}) do x
        for i = 1:1000
            DoAllocNoEscapeSparam(x)
        end
    end
    @test count(x->isexpr(x, :static_parameter), src.code) == 0 # A bad inline might leave left-over :static_parameter
    nnothrow_invokes = count(isinvoke(:nothrow_side_effect), src.code)
    @test count(iscall(f->!isa(singleton_type(argextype(f, src)), Core.Builtin)), src.code) ==
          count(iscall((src, nothrow_side_effect)), src.code) == 2 - nnothrow_invokes
    # TODO: Our effect modeling is not yet strong enough to fully eliminate this
    @test_broken count(isnew, src.code) == 0
end

# Test finalizer varargs
function varargs_finalizer(args...)
    nothrow_side_effect(args[1])
end
mutable struct DoAllocNoEscapeNoVarargs
    function DoAllocNoEscapeNoInline()
        finalizer(noinline_finalizer, new())
    end
end
let src = code_typed1() do
        for i = 1:1000
            DoAllocNoEscapeNoInline()
        end
    end
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

const FINALIZATION_COUNT = Ref(0)
init_finalization_count!() = FINALIZATION_COUNT[] = 0
get_finalization_count() = FINALIZATION_COUNT[]
@noinline add_finalization_count!(x) = FINALIZATION_COUNT[] += x
@noinline Base.@assume_effects :nothrow safeprint(io::IO, x...) = (@nospecialize; print(io, x...))
@test Compiler.is_finalizer_inlineable(Base.infer_effects(add_finalization_count!, (Int,)))

mutable struct DoAllocWithField
    x::Int
    function DoAllocWithField(x::Int)
        finalizer(new(x)) do this
            add_finalization_count!(this.x)
        end
    end
end
mutable struct DoAllocWithFieldInter
    x::Int
end
function register_finalizer!(obj::DoAllocWithFieldInter)
    finalizer(obj) do this
        add_finalization_count!(this.x)
    end
end

function const_finalization(io)
    for i = 1:1000
        o = DoAllocWithField(1)
        safeprint(io, o.x)
    end
end
let src = code_typed1(const_finalization, (IO,))
    @test count(isinvoke(:add_finalization_count!), src.code) == 1
end
let
    init_finalization_count!()
    const_finalization(IOBuffer())
    @test get_finalization_count() == 1000
end

# Test that finalizers that don't do anything are just erased from the IR
function useless_finalizer()
    x = Ref(1)
    finalizer(x) do x
        nothing
    end
    return x
end
let src = code_typed1(useless_finalizer, ())
    @test count(iscall((src, Core.finalizer)), src.code) == 0
    @test length(src.code) == 2
end

# tests finalizer inlining when def/uses involve control flow
function cfg_finalization1(io)
    for i = -999:1000
        o = DoAllocWithField(i)
        if i == 1000
            safeprint(io, o.x, '\n')
        elseif i > 0
            safeprint(io, o.x)
        end
    end
end
let src = code_typed1(cfg_finalization1, (IO,))
    @test count(isinvoke(:add_finalization_count!), src.code) == 1
end
let
    init_finalization_count!()
    cfg_finalization1(IOBuffer())
    @test get_finalization_count() == 1000
end

function cfg_finalization2(io)
    for i = -999:1000
        o = DoAllocWithField(1)
        o.x = i # with `setfield!`
        if i == 1000
            safeprint(io, o.x, '\n')
        elseif i > 0
            safeprint(io, o.x)
        end
    end
end
let src = code_typed1(cfg_finalization2, (IO,))
    @test count(isinvoke(:add_finalization_count!), src.code) == 1
end
let
    init_finalization_count!()
    cfg_finalization2(IOBuffer())
    @test get_finalization_count() == 1000
end

function cfg_finalization3(io)
    for i = -999:1000
        o = DoAllocWithFieldInter(i)
        register_finalizer!(o)
        if i == 1000
            safeprint(io, o.x, '\n')
        elseif i > 0
            safeprint(io, o.x)
        end
    end
end
let src = code_typed1(cfg_finalization3, (IO,))
    @test count(isinvoke(:add_finalization_count!), src.code) == 1
end
let
    init_finalization_count!()
    cfg_finalization3(IOBuffer())
    @test get_finalization_count() == 1000
end

function cfg_finalization4(io)
    for i = -999:1000
        o = DoAllocWithFieldInter(1)
        o.x = i # with `setfield!`
        register_finalizer!(o)
        if i == 1000
            safeprint(io, o.x, '\n')
        elseif i > 0
            safeprint(io, o.x)
        end
    end
end
let src = code_typed1(cfg_finalization4, (IO,))
    @test count(isinvoke(:add_finalization_count!), src.code) == 1
end
let
    init_finalization_count!()
    cfg_finalization4(IOBuffer())
    @test get_finalization_count() == 1000
end

function cfg_finalization5(io)
    for i = -999:1000
        o = DoAllocWithFieldInter(i)
        if i == 1000
            safeprint(io, o.x, '\n')
        elseif i > 0
            safeprint(io, o.x)
        end
        register_finalizer!(o)
    end
end
let src = code_typed1(cfg_finalization5, (IO,))
    @test count(isinvoke(:add_finalization_count!), src.code) == 1
end
let
    init_finalization_count!()
    cfg_finalization5(IOBuffer())
    @test get_finalization_count() == 1000
end

function cfg_finalization6(io)
    for i = -999:1000
        o = DoAllocWithField(0)
        if i == 1000
            o.x = i # with `setfield!`
        elseif i > 0
            safeprint(io, o.x, '\n')
        end
    end
end
let src = code_typed1(cfg_finalization6, (IO,))
    @test count(isinvoke(:add_finalization_count!), src.code) == 1
end
let
    init_finalization_count!()
    cfg_finalization6(IOBuffer())
    @test get_finalization_count() == 1000
end

function cfg_finalization7(io)
    for i = -999:1000
        o = DoAllocWithField(0)
        o.x = 0
        if i == 1000
            o.x = i # with `setfield!`
        end
        o.x = i
        if i == 999
            o.x = i
        end
        o.x = 0
        if i == 1000
            o.x = i
        end
    end
end
let src = code_typed1(cfg_finalization7, (IO,))
    @test count(isinvoke(:add_finalization_count!), src.code) == 1
end
let
    init_finalization_count!()
    cfg_finalization7(IOBuffer())
    @test get_finalization_count() == 1000
end

# Load forwarding with `finalizer` elision
let src = code_typed1((Int,)) do x
        xs = finalizer(Ref(x)) do obj
            @noinline
            Base.@assume_effects :nothrow :notaskstate
            Core.println("finalizing: ", obj[])
        end
        Base.@assume_effects :nothrow @noinline println("xs[] = ", @inline xs[])
        return xs[]
    end
    @test count(iscall((src, getfield)), src.code) == 0
end
let src = code_typed1((Int,)) do x
        xs = finalizer(Ref(x)) do obj
            @noinline
            Base.@assume_effects :nothrow :notaskstate
            Core.println("finalizing: ", obj[])
        end
        Base.@assume_effects :nothrow @noinline println("xs[] = ", @inline xs[])
        xs[] += 1
        return xs[]
    end
    @test count(iscall((src, getfield)), src.code) == 0
    @test count(iscall((src, setfield!)), src.code) == 1
end

# optimize `[push!|pushfirst!](::Vector{Any}, x...)`
@testset "optimize `$f(::Vector{Any}, x...)`" for f = Any[push!, pushfirst!]
    @eval begin
        for T in [Int, Any]
            let src = code_typed1((Vector{T}, T)) do xs, x
                    $f(xs, x)
                end
                @test count(iscall((src, $f)), src.code) == 0
            end
            let effects = Base.infer_effects((Vector{T}, T)) do xs, x
                    $f(xs, x)
                end
                @test Compiler.Compiler.is_terminates(effects)
            end
            let src = code_typed1((Vector{T}, T, T)) do xs, x, y
                    $f(xs, x, y)
                end
                @test count(iscall((src, $f)), src.code) == 0
            end
        end
        let xs = Any[]
            $f(xs, :x, "y", 'z')
            @test xs[1] === :x
            @test xs[2] == "y"
            @test xs[3] === 'z'
        end
    end
end

using .Compiler: is_declared_inline, is_declared_noinline

# https://github.com/JuliaLang/julia/issues/45050
@testset "propagate :meta annotations to keyword sorter methods" begin
    # @inline, @noinline, @constprop
    let @inline f(::Any; x::Int=1) = 2x
        @test is_declared_inline(only(methods(f)))
        @test is_declared_inline(only(methods(Core.kwcall, (Any, typeof(f), Vararg))))
    end
    let @noinline f(::Any; x::Int=1) = 2x
        @test is_declared_noinline(only(methods(f)))
        @test is_declared_noinline(only(methods(Core.kwcall, (Any, typeof(f), Vararg))))
    end
    let Base.@constprop :aggressive f(::Any; x::Int=1) = 2x
        @test Compiler.is_aggressive_constprop(only(methods(f)))
        @test Compiler.is_aggressive_constprop(only(methods(Core.kwcall, (Any, typeof(f), Vararg))))
    end
    let Base.@constprop :none f(::Any; x::Int=1) = 2x
        @test Compiler.is_no_constprop(only(methods(f)))
        @test Compiler.is_no_constprop(only(methods(Core.kwcall, (Any, typeof(f), Vararg))))
    end
    # @nospecialize
    let f(@nospecialize(A::Any); x::Int=1) = 2x
        @test only(methods(f)).nospecialize == 1
        @test only(methods(Core.kwcall, (Any, typeof(f), Vararg))).nospecialize == 4
    end
    let f(::Any; x::Int=1) = (@nospecialize; 2x)
        @test only(methods(f)).nospecialize == -1
        @test only(methods(Core.kwcall, (Any, typeof(f), Vararg))).nospecialize == -1
    end
    # Base.@assume_effects
    let Base.@assume_effects :notaskstate f(::Any; x::Int=1) = 2x
        @test Compiler.decode_effects_override(only(methods(f)).purity).notaskstate
        @test Compiler.decode_effects_override(only(methods(Core.kwcall, (Any, typeof(f), Vararg))).purity).notaskstate
    end
    # propagate multiple metadata also
    let @inline Base.@assume_effects :notaskstate Base.@constprop :aggressive f(::Any; x::Int=1) = (@nospecialize; 2x)
        @test is_declared_inline(only(methods(f)))
        @test Compiler.is_aggressive_constprop(only(methods(f)))
        @test is_declared_inline(only(methods(Core.kwcall, (Any, typeof(f), Vararg))))
        @test Compiler.is_aggressive_constprop(only(methods(Core.kwcall, (Any, typeof(f), Vararg))))
        @test only(methods(f)).nospecialize == -1
        @test only(methods(Core.kwcall, (Any, typeof(f), Vararg))).nospecialize == -1
        @test Compiler.decode_effects_override(only(methods(f)).purity).notaskstate
        @test Compiler.decode_effects_override(only(methods(Core.kwcall, (Any, typeof(f), Vararg))).purity).notaskstate
    end
end

# Test that one opaque closure capturing another gets inlined properly.
function oc_capture_oc(z)
    oc1 = @opaque x->x
    oc2 = @opaque y->oc1(y)
    return oc2(z)
end
@test fully_eliminated(oc_capture_oc, (Int,))

# inlining with unmatched type parameters
@eval struct OldVal{T}
    (OV::Type{OldVal{T}})() where T = $(Expr(:new, :OV))
end
@test OldVal{0}() === OldVal{0}.instance
function with_unmatched_typeparam()
    f(x::OldVal{i}) where {i} = i
    r = 0
    for i = 1:10000
        r += f(OldVal{i}())
    end
    return r
end
let src = code_typed1(with_unmatched_typeparam)
    found = nothing
    for x in src.code
        if isexpr(x, :call) && length(x.args) == 1
            found = x
            break
        end
    end
    @test isnothing(found) || (source=src, statement=found)
end

function twice_sitofp(x::Int, y::Int)
    x = Base.sitofp(Float64, x)
    y = Base.sitofp(Float64, y)
    return (x, y)
end

# Test that semi-concrete eval can inline constant results
let src = code_typed1((Int,)) do x
        twice_sitofp(x, 2)
    end
    @test count(iscall((src, Base.sitofp)), src.code) == 1
end

# `@noinline` annotations with semi-concrete eval
let src = code_typed1((Int,)) do x
        @noinline twice_sitofp(x, 2)
    end
    @test count(isinvoke(:twice_sitofp), src.code) == 1
end

# `Base.@constprop :aggressive` forces semi-concrete eval, but it should still not be inlined
@noinline Base.@constprop :aggressive function twice_sitofp_noinline(x::Int, y::Int)
    x = Base.sitofp(Float64, x)
    y = Base.sitofp(Float64, y)
    return (x, y)
end

let src = code_typed1((Int,)) do x
        twice_sitofp_noinline(x, 2)
    end
    @test count(isinvoke(:twice_sitofp_noinline), src.code) == 1
end

# Test getfield modeling of Type{Ref{_A}} where _A
let getfield_tfunc(@nospecialize xs...) =
        Compiler.getfield_tfunc(Compiler.fallback_lattice, xs...)
    @test getfield_tfunc(Type, Core.Const(:parameters)) !== Union{}
    @test !isa(getfield_tfunc(Type{Tuple{Union{Int, Float64}, Int}}, Core.Const(:name)), Core.Const)
    @test !isa(getfield_tfunc(Type{Tuple{Any}}, Core.Const(:name)), Core.Const)
end
@test fully_eliminated(Base.ismutable, Tuple{Base.RefValue})

# TODO: Remove compute sparams for vararg_retrieval
fvarargN_inline(x::Tuple{Vararg{Int, N}}) where {N} = N
fvarargN_inline(args...) = fvarargN_inline(args)
let src = code_typed1(fvarargN_inline, (Tuple{Vararg{Int}},))
    @test_broken count(iscall((src, Core._compute_sparams)), src.code) == 0 &&
                 count(iscall((src, Core._svec_ref)), src.code) == 0 &&
                 count(iscall((src, Core.nfields)), src.code) == 1
end

# Test effect annotation of declined inline unionsplit
f_union_unmatched(x::Union{Nothing, Type{T}}) where {T} = nothing
let src = code_typed1((Any,)) do x
        if isa(x, Union{Nothing, Type})
            f_union_unmatched(x)
        end
        nothing
    end
    @test count(iscall((src, f_union_unmatched)), src.code) == 0
end

# modifyfield! handling
# =====================

isinvokemodify(y) = @nospecialize(x) -> isinvokemodify(y, x)
isinvokemodify(sym::Symbol, @nospecialize(x)) = isinvokemodify(mi->mi.def.name===sym, x)
isinvokemodify(pred::Function, @nospecialize(x)) = isexpr(x, :invoke_modify) && pred((x.args[1]::CodeInstance).def)

mutable struct Atomic{T}
    @atomic x::T
end
let src = code_typed1((Atomic{Int},)) do a
        @atomic a.x + 1
    end
    @test count(isinvokemodify(:+), src.code) == 1
end
let src = code_typed1((Atomic{Int},)) do a
        @atomic a.x += 1
    end
    @test count(isinvokemodify(:+), src.code) == 1
end
let src = code_typed1((Atomic{Int},)) do a
        @atomic a.x max 10
    end
    @test count(isinvokemodify(:max), src.code) == 1
end
# simple union split handling
mymax(x::T, y::T) where T<:Real = max(x, y)
mymax(x::T, y::Real) where T<:Real = convert(T, max(x, y))::T
let src = code_typed1((Atomic{Int},Union{Int,Float64})) do a, b
        @atomic a.x mymax b
    end
    @test count(isinvokemodify(:mymax), src.code) == 2
end
global x_global_inc::Int = 1
let src = code_typed1(()) do
        @atomic (@__MODULE__).x_global_inc += 1
    end
    @test count(isinvokemodify(:+), src.code) == 1
end
let src = code_typed1((Ptr{Int},)) do a
        unsafe_modify!(a, +, 1)
    end
    @test count(isinvokemodify(:+), src.code) == 1
end
let src = code_typed1((AtomicMemoryRef{Int},)) do a
        Core.memoryrefmodify!(a, +, 1, :sequentially_consistent, true)
    end
    @test count(isinvokemodify(:+), src.code) == 1
end

# apply `ssa_inlining_pass` multiple times
func_mul_int(a::Int, b::Int) = Core.Intrinsics.mul_int(a, b)
multi_inlining1(a::Int, b::Int) = @noinline func_mul_int(a, b)
let i::Int, continue_::Bool
    interp = Compiler.NativeInterpreter()
    # check if callsite `@noinline` annotation works
    ir, = only(Base.code_ircode(multi_inlining1, (Int,Int); optimize_until="inlining", interp))
    i = findfirst(isinvoke(:func_mul_int), ir.stmts.stmt)
    @test i !== nothing
    # now delete the callsite flag, and see the second inlining pass can inline the call
    @eval Compiler $ir.stmts[$i][:flag] &= ~IR_FLAG_NOINLINE
    inlining = Compiler.InliningState(interp)
    ir = Compiler.ssa_inlining_pass!(ir, inlining, false)
    @test findfirst(isinvoke(:func_mul_int), ir.stmts.stmt) === nothing
    @test (i = findfirst(iscall((ir, Core.Intrinsics.mul_int)), ir.stmts.stmt)) !== nothing
    lins = Compiler.IRShow.buildLineInfoNode(ir.debuginfo, nothing, i)
    @test (continue_ = length(lins) == 2) # :multi_inlining1 -> :func_mul_int
    if continue_
        def1 = lins[1].method
        @test def1 isa Core.MethodInstance && def1.def.name === :multi_inlining1
        def2 = lins[2].method
        @test def2 isa Core.MethodInstance && def2.def.name === :func_mul_int
    end
end

call_func_mul_int(a::Int, b::Int) = @noinline func_mul_int(a, b)
multi_inlining2(a::Int, b::Int) = call_func_mul_int(a, b)
let i::Int, continue_::Bool
    interp = Compiler.NativeInterpreter()
    # check if callsite `@noinline` annotation works
    ir, = only(Base.code_ircode(multi_inlining2, (Int,Int); optimize_until="inlining", interp))
    i = findfirst(isinvoke(:func_mul_int), ir.stmts.stmt)
    @test i !== nothing
    # now delete the callsite flag, and see the second inlining pass can inline the call
    @eval Compiler $ir.stmts[$i][:flag] &= ~IR_FLAG_NOINLINE
    inlining = Compiler.InliningState(interp)
    ir = Compiler.ssa_inlining_pass!(ir, inlining, false)
    @test findfirst(isinvoke(:func_mul_int), ir.stmts.stmt) === nothing
    @test (i = findfirst(iscall((ir, Core.Intrinsics.mul_int)), ir.stmts.stmt)) !== nothing
    lins = Compiler.IRShow.buildLineInfoNode(ir.debuginfo, nothing, i)
    @test_broken (continue_ = length(lins) == 3) # see TODO in `ir_inline_linetable!`
    if continue_
        def1 = lins[1].method
        @test def1 isa Core.MethodInstance && def1.def.name === :multi_inlining2
        def2 = lins[2].method
        @test def2 isa Core.MethodInstance && def2.def.name === :call_func_mul_int
        def3 = lins[3].method
        @test def3 isa Core.MethodInstance && def3.def.name === :call_func_mul_int
    end
end

# Test special purpose inliner for Core.ifelse
f_ifelse_1(a, b) = Core.ifelse(true, a, b)
f_ifelse_2(a, b) = Core.ifelse(false, a, b)
f_ifelse_3(a, b) = Core.ifelse(a, true, b)

@test fully_eliminated(f_ifelse_1, Tuple{Any, Any}; retval=Core.Argument(2))
@test fully_eliminated(f_ifelse_2, Tuple{Any, Any}; retval=Core.Argument(3))
@test !fully_eliminated(f_ifelse_3, Tuple{Any, Any})

# inline_splatnew for abstract `NamedTuple`
@eval construct_splatnew(T, fields) = $(Expr(:splatnew, :T, :fields))
for tt = Any[(Int,Int), (Integer,Integer), (Any,Any)]
    let src = code_typed1(tt) do a, b
            construct_splatnew(NamedTuple{(:a,:b),typeof((a,b))}, (a,b))
        end
        @test count(issplatnew, src.code) == 0
        @test count(isnew, src.code) == 1
    end
end

# optimize away `NamedTuple`s used for handling `@nospecialize`d keyword-argument
# https://github.com/JuliaLang/julia/pull/47059
abstract type TestCallInfo end
struct TestNewInstruction
    stmt::Any
    type::Any
    info::TestCallInfo
    line::Int32
    flag::UInt8
    function TestNewInstruction(@nospecialize(stmt), @nospecialize(type), @nospecialize(info::TestCallInfo),
                            line::Int32, flag::UInt8)
        return new(stmt, type, info, line, flag)
    end
end
@nospecialize
function TestNewInstruction(newinst::TestNewInstruction;
    stmt=newinst.stmt,
    type=newinst.type,
    info::TestCallInfo=newinst.info,
    line::Int32=newinst.line,
    flag::UInt8=newinst.flag)
    return TestNewInstruction(stmt, type, info, line, flag)
end
@specialize
let src = code_typed1((TestNewInstruction,Any,Any,TestCallInfo)) do newinst, stmt, type, info
        TestNewInstruction(newinst; stmt, type, info)
    end
    @test count(issplatnew, src.code) == 0
    @test count(iscall((src,NamedTuple)), src.code) == 0
    @test count(isnew, src.code) == 1
end

# Test that inlining can still use nothrow information from concrete-eval
# even if the result itself is too big to be inlined, and nothrow is not
# known without concrete-eval
const THE_BIG_TUPLE = ntuple(identity, 1024);
function return_the_big_tuple(err::Bool)
    err && error("BAD")
    return THE_BIG_TUPLE
end
@test fully_eliminated() do
    return_the_big_tuple(false)[1]
end
@test fully_eliminated() do
    @inline return_the_big_tuple(false)[1]
end

# inlineable but removable call should be eligible for DCE
Base.@assume_effects :removable @inline function inlineable_effect_free(a::Float64)
    a == Inf && return zero(a)
    return sin(a) + cos(a)
end
@test fully_eliminated((Float64,)) do a
    b = inlineable_effect_free(a)
    c = inlineable_effect_free(b)
    nothing
end

# https://github.com/JuliaLang/julia/issues/47374
function f47374(x)
    [f47374(i, x) for i in 1:1]
end
function f47374(i::Int, x)
    return 1.0
end
@test f47374(rand(1)) == Float64[1.0]

# compiler should recognize effectful :static_parameter
# https://github.com/JuliaLang/julia/issues/45490
issue45490_1(x::Union{T, Nothing}, y::Union{T, Nothing}) where {T} = T
issue45490_2(x::Union{T, Nothing}, y::Union{T, Nothing}) where {T} = (typeof(T); nothing)
for f = (issue45490_1, issue45490_2)
    src = code_typed1(f, (Any,Any))
    @test any(src.code) do @nospecialize x
        isexpr(x, :static_parameter)
    end
    @test_throws UndefVarError f(nothing, nothing)
end

# inline effect-free :static_parameter, required for semi-concrete interpretation accuracy
# https://github.com/JuliaLang/julia/issues/47349
function make_issue47349(::Val{N}) where {N}
    pickargs(::Val{N}) where {N} = (@nospecialize(x::Tuple)) -> x[N]
    return pickargs(Val{N-1}())
end
let src = code_typed1(make_issue47349(Val{4}()), (Any,))
    @test !any(src.code) do @nospecialize x
        isexpr(x, :static_parameter)
    end
    @test Base.return_types((Int,)) do x
        make_issue47349(Val(4))((x,nothing,Int))
    end |> only === Type{Int}
end

# Test that irinterp can make use of constant results even if they're big
# Check that pure functions with non-inlineable results still get deleted
struct BigSemi
    x::NTuple{1024, Int}
end
@Base.assume_effects :total @noinline make_big_tuple(x::Int) = ntuple(x->x+1, 1024)::NTuple{1024, Int}
BigSemi(y::Int, x::Int) = BigSemi(make_big_tuple(x))
function elim_full_ir(y)
    bs = BigSemi(y, 10)
    return Val{bs.x[1]}()
end

@test fully_eliminated(elim_full_ir, Tuple{Int})

# union splitting should account for uncovered call signature
# https://github.com/JuliaLang/julia/issues/48397
f48397(::Bool) = :ok
f48397(::Tuple{String,String}) = :ok
let src = code_typed1((Union{Bool,Tuple{String,Any}},)) do x
        f48397(x)
    end
    @test any(iscall((src, Core.throw_methoderror)), src.code) # fallback method error)
end
g48397::Union{Bool,Tuple{String,Any}} = ("48397", 48397)
let res = @test_throws MethodError let
        Base.Experimental.@force_compile
        f48397(g48397)
    end
    err = res.value
    @test err.f === f48397 && err.args === (g48397,)
end
let res = @test_throws MethodError let
        Base.Experimental.@force_compile
        convert(Union{Bool,Tuple{String,String}}, g48397)
    end
    err = res.value
    @test err.f === convert && err.args === (Union{Bool,Tuple{String,String}}, g48397)
end

# https://github.com/JuliaLang/julia/issues/49050
abstract type Issue49050AbsTop{T,N} end
abstract type Issue49050Abs1{T, N} <: Issue49050AbsTop{T,N} end
abstract type Issue49050Abs2{T} <: Issue49050Abs1{T,3} end
struct Issue49050Concrete{T} <: Issue49050Abs2{T}
    x::T
end
issue49074(::Type{Issue49050AbsTop{T,N}}) where {T,N} = Issue49050AbsTop{T,N}
Base.@assume_effects :foldable issue49074(::Type{C}) where {C<:Issue49050AbsTop} = issue49074(supertype(C))
let src = code_typed1() do
        issue49074(Issue49050Concrete)
    end
    @test any(isinvoke(:issue49074), src.code)
end
let result = @test_throws MethodError issue49074(Issue49050Concrete)
    @test result.value.f === issue49074
    @test result.value.args === (Any,)
end

# inlining of `TypeName`
@test fully_eliminated() do
    Ref.body.name
end

# Regression for finalizer inlining with more complex control flow
global finalizer_escape::Int = 0
mutable struct FinalizerEscapeTest
    x::Int
    function FinalizerEscapeTest()
        this = new(0)
        finalizer(this) do this
            global finalizer_escape
            finalizer_escape = this.x
        end
        return this
    end
end

function run_finalizer_escape_test1(b1, b2)
    x = FinalizerEscapeTest()
    x.x = 1
    if b1
        x.x = 2
    end
    if b2
        Base.donotdelete(b2)
    end
    x.x = 3
    return nothing
end

function run_finalizer_escape_test2(b1, b2)
    x = FinalizerEscapeTest()
    x.x = 1
    if b1
        x.x = 2
    end
    x.x = 3
    return nothing
end

for run_finalizer_escape_test in (run_finalizer_escape_test1, run_finalizer_escape_test2)
    global finalizer_escape::Int = 0

    let src = code_typed1(run_finalizer_escape_test, Tuple{Bool, Bool})
        @test any(iscall((src, Core.setglobal!)), src.code)
    end

    let
        run_finalizer_escape_test(true, true)
        @test finalizer_escape == 3
    end
end

# `compilesig_invokes` inlining option
@newinterp NoCompileSigInvokes
Compiler.OptimizationParams(::NoCompileSigInvokes) =
    Compiler.OptimizationParams(; compilesig_invokes=false)
@noinline no_compile_sig_invokes(@nospecialize x) = (x !== Any && !Base.has_free_typevars(x))
# test the single dispatch candidate case
let src = code_typed1((Type,)) do x
        no_compile_sig_invokes(x)
    end
    @test count(src.code) do @nospecialize x
        isinvoke(:no_compile_sig_invokes, x) &&
        (x.args[1]::Core.CodeInstance).def.specTypes == Tuple{typeof(no_compile_sig_invokes),Any}
    end == 1
end
let src = code_typed1((Type,); interp=NoCompileSigInvokes()) do x
        no_compile_sig_invokes(x)
    end
    @test count(src.code) do @nospecialize x
        isinvoke(:no_compile_sig_invokes, x) &&
        (x.args[1]::Core.CodeInstance).def.specTypes == Tuple{typeof(no_compile_sig_invokes),Type}
    end == 1
end
# test the union split case
let src = code_typed1((Union{DataType,UnionAll},)) do x
        no_compile_sig_invokes(x)
    end
    @test count(src.code) do @nospecialize x
        isinvoke(:no_compile_sig_invokes, x) &&
        (x.args[1]::Core.CodeInstance).def.specTypes == Tuple{typeof(no_compile_sig_invokes),Any}
    end == 2
end
let src = code_typed1((Union{DataType,UnionAll},); interp=NoCompileSigInvokes()) do x
        no_compile_sig_invokes(x)
    end
    @test count(src.code) do @nospecialize x
        isinvoke(:no_compile_sig_invokes, x) &&
        (x.args[1]::Core.CodeInstance).def.specTypes == Tuple{typeof(no_compile_sig_invokes),DataType}
    end == 1
    @test count(src.code) do @nospecialize x
        isinvoke(:no_compile_sig_invokes, x) &&
        (x.args[1]::Core.CodeInstance).def.specTypes == Tuple{typeof(no_compile_sig_invokes),UnionAll}
    end == 1
end

# https://github.com/JuliaLang/julia/issues/50612
f50612(x) = UInt32(x)
@test all(!isinvoke(:UInt32),get_code(f50612,Tuple{Char}))

# move inlineable constant values into statement position during `compact!`-ion
# so that we don't inline DCE-eligibile calls
Base.@assume_effects :nothrow function erase_before_inlining(x, y)
    z = sin(y)
    if x
        return "julia"
    end
    return z
end
@test fully_eliminated((Float64,); retval=5) do y
    length(erase_before_inlining(true, y))
end
@test fully_eliminated((Float64,); retval=(5,5)) do y
    z = erase_before_inlining(true, y)
    return length(z), length(z)
end

# continue const-prop' when concrete-eval result is too big
const THE_BIG_TUPLE_2 = ntuple(identity, 1024)
return_the_big_tuple2(a) = (a, THE_BIG_TUPLE_2)
let src = code_typed1() do
        return return_the_big_tuple2(42)[2]
    end
    @test count(isinvoke(:return_the_big_tuple2), src.code) == 0
end
let src = code_typed1() do
        return iterate(("1", '2'), 1)
    end
    @test count(isinvoke(:iterate), src.code) == 0
end

function issue53062(cond)
    x = Ref{Int}(0)
    if cond
        x[] = x
    else
        return -1
    end
end
@test !Compiler.is_nothrow(Base.infer_effects(issue53062, (Bool,)))
@test issue53062(false) == -1
@test_throws MethodError issue53062(true)

struct Issue52644
    tuple::Type{<:Tuple}
end
issue52644(::DataType) = :DataType
issue52644(::UnionAll) = :UnionAll
let ir = Base.code_ircode((Issue52644,); optimize_until="Inlining") do t
        issue52644(t.tuple)
    end |> only |> first
    ir.argtypes[1] = Tuple{}
    irfunc = Core.OpaqueClosure(ir)
    @test irfunc(Issue52644(Tuple{})) === :DataType
    @test irfunc(Issue52644(Tuple{<:Integer})) === :UnionAll
end
issue52644_single(x::DataType) = :DataType
let ir = Base.code_ircode((Issue52644,); optimize_until="Inlining") do t
        issue52644_single(t.tuple)
    end |> only |> first
    ir.argtypes[1] = Tuple{}
    irfunc = Core.OpaqueClosure(ir)
    @test irfunc(Issue52644(Tuple{})) === :DataType
    @test_throws MethodError irfunc(Issue52644(Tuple{<:Integer}))
end

foo_split(x::Float64) = 1
foo_split(x::Int) = 2
bar_inline_error() = foo_split(nothing)
bar_split_error() = foo_split(Core.compilerbarrier(:type,nothing))

let src = code_typed1(bar_inline_error, Tuple{})
    # Should inline method errors
    @test count(iscall((src, foo_split)), src.code) == 0
    @test count(iscall((src, Core.throw_methoderror)), src.code) > 0
end
let src = code_typed1(bar_split_error, Tuple{})
    # Should inline method errors
    @test count(iscall((src, foo_split)), src.code) == 0
    @test count(iscall((src, Core.throw_methoderror)), src.code) > 0
end

# finalizer inlining with EA
mutable struct ForeignBuffer{T}
    const ptr::Ptr{T}
end
mutable struct ForeignBufferChecker
    @atomic finalized::Bool
end
const foreign_buffer_checker = ForeignBufferChecker(false)
function foreign_alloc(::Type{T}, length) where T
    ptr = Libc.malloc(sizeof(T) * length)
    ptr = Base.unsafe_convert(Ptr{T}, ptr)
    obj = ForeignBuffer{T}(ptr)
    return finalizer(obj) do obj
        Base.@assume_effects :notaskstate :nothrow
        @atomic foreign_buffer_checker.finalized = true
        Libc.free(obj.ptr)
    end
end
function f_EA_finalizer(N::Int)
    workspace = foreign_alloc(Float64, N)
    GC.@preserve workspace begin
        (;ptr) = workspace
        Base.@assume_effects :nothrow @noinline println(devnull, "ptr = ", ptr)
    end
end
let src = code_typed1(foreign_alloc, (Type{Float64},Int,))
    @test count(iscall((src, Core.finalizer)), src.code) == 1
end
let src = code_typed1(f_EA_finalizer, (Int,))
    @test count(iscall((src, Core.finalizer)), src.code) == 0
end
let;Base.Experimental.@force_compile
    f_EA_finalizer(42000)
    @test foreign_buffer_checker.finalized
end

# JuliaLang/julia#56422:
# EA-based finalizer inlining should not result in an invalid IR in the existence of `PhiNode`s
function issue56422(cnd::Bool, N::Int)
    if cnd
        workspace = foreign_alloc(Float64, N)
    else
        workspace = foreign_alloc(Float64, N+1)
    end
    GC.@preserve workspace begin
        (;ptr) = workspace
        Base.@assume_effects :nothrow @noinline println(devnull, "ptr = ", ptr)
    end
end
let src = code_typed1(issue56422, (Bool,Int,))
    @test_broken count(iscall((src, Core.finalizer)), src.code) == 0
end

# Test that inlining doesn't unnecessarily move things to statement position
@noinline f_noinline_invoke(x::Union{Symbol,Nothing}=nothing) = Core.donotdelete(x)
g_noinline_invoke(x) = f_noinline_invoke(x)
let src = code_typed1(g_noinline_invoke, (Union{Symbol,Nothing},))
    @test !any(@nospecialize(x)->isa(x,GlobalRef), src.code)
end
