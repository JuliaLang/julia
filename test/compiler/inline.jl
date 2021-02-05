# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using Base.Meta
using Core: ReturnNode

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
    @test exf19122.args[2].args[1].args[1] == :inline
    @test exg19122.args[2].args[1].args[1] == :noinline

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

function fully_eliminated(f, args)
    let code = code_typed(f, args)[1][1].code
        return length(code) == 1 && isa(code[1], ReturnNode)
    end
end

function fully_eliminated(f, args, retval)
    let code = code_typed(f, args)[1][1].code
        return length(code) == 1 && isa(code[1], ReturnNode) && code[1].val == retval
    end
end

# check that type.mutable can be fully eliminated
f_mutable_nothrow(s::String) = Val{typeof(s).mutable}
@test fully_eliminated(f_mutable_nothrow, (String,))

# check that ifelse can be fully eliminated
function f_ifelse(x)
    a = ifelse(true, false, true)
    b = ifelse(a, true, false)
    return b ? x + 1 : x
end
# 2 for now because the compiler leaves a GotoNode around
@test_broken length(code_typed(f_ifelse, (String,))[1][1].code) <= 2

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
@test fully_eliminated(f_div, (Int,)) == 1
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
@test fully_eliminated(f_subtype, Tuple{}, false)

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
@test fully_eliminated(foo_apply_apply_type_svec, Tuple{}, NTuple{3, Float32})

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
@test fully_eliminated(f34900, Tuple{Int, Int}, Core.Argument(2))

@testset "check jl_ir_flag_inlineable for inline macro" begin
    @test ccall(:jl_ir_flag_inlineable, Bool, (Any,), first(methods(@inline x -> x)).source)
    @test !ccall(:jl_ir_flag_inlineable, Bool, (Any,), first(methods( x -> x)).source)
    @test ccall(:jl_ir_flag_inlineable, Bool, (Any,), first(methods(@inline function f(x) x end)).source)
    @test !ccall(:jl_ir_flag_inlineable, Bool, (Any,), first(methods(function f(x) x end)).source)
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
let ci = code_typed(NonIsBitsDims, Tuple{})[1].first
    @test length(ci.code) == 1 && isa(ci.code[1], ReturnNode) &&
        ci.code[1].val.value == NonIsBitsDims()
end

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
