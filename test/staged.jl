# This file is a part of Julia. License is MIT: https://julialang.org/license

# N.B.: This file is also run from interpreter.jl, so needs to be standalone-executable
using Test

using Random
using InteractiveUtils: code_llvm, code_native

@generated function staged_t1(a,b)
    if a == Int
        return :(a+b)
    else
        return :(a*b)
    end
end

@test staged_t1(1,2) == 3
@test staged_t1(1.0,0.5) == 0.5
@test staged_t1(1,0.5) == 1.5

tinline(a,b) = staged_t1(a,b)

@test !isa(tinline(1,2),Expr)
@test tinline(1,0.5) == 1.5

@generated function splat(a,b...)
    :( ($a,$b,a,b) )
end

@test splat(1,2,3) == (Int,(Int,Int),1,(2,3))

stagediobuf = IOBuffer()
@generated function splat2(a...)
    print(stagediobuf, a)
    :(nothing)
end

const intstr = string(Int)
splat2(1)
@test String(take!(stagediobuf)) == "($intstr,)"
splat2(1, 3)
@test String(take!(stagediobuf)) == "($intstr, $intstr)"
splat2(5, 2)
@test String(take!(stagediobuf)) == ""
splat2(1:3, 5.2)
@test String(take!(stagediobuf)) == "(UnitRange{$intstr}, Float64)"
splat2(3, 5:2:7)
@test String(take!(stagediobuf)) == "($intstr, StepRange{$intstr, $intstr})"
splat2(1, 2, 3, 4)
@test String(take!(stagediobuf)) == "($intstr, $intstr, $intstr, $intstr)"
splat2(1, 2, 3)
@test String(take!(stagediobuf)) == "($intstr, $intstr, $intstr)"
splat2(1:5, 3, 3:3)
@test String(take!(stagediobuf)) == "(UnitRange{$intstr}, $intstr, UnitRange{$intstr})"
splat2(1:5, 3, 3:3)
@test String(take!(stagediobuf)) == ""
splat2(1:5, 3:3, 3)
@test String(take!(stagediobuf)) == "(UnitRange{$intstr}, UnitRange{$intstr}, $intstr)"
splat2(1:5, 3:3)
@test String(take!(stagediobuf)) == "(UnitRange{$intstr}, UnitRange{$intstr})"
splat2(3, 3:5)
@test String(take!(stagediobuf)) == "($intstr, UnitRange{$intstr})"

# varargs specialization with parametric @generated functions (issue #8944)
@generated function splat3(A::AbstractArray{T,N}, indx::Base.RangeIndex...) where {T,N}
    print(stagediobuf, indx)
    :(nothing)
end
A = rand(5,5,3)
splat3(A, 1:2, 1:2, 1)
@test String(take!(stagediobuf)) == "(UnitRange{$intstr}, UnitRange{$intstr}, $intstr)"
splat3(A, 1:2, 1, 1:2)
@test String(take!(stagediobuf)) == "(UnitRange{$intstr}, $intstr, UnitRange{$intstr})"

B = view(A, 1:3, 2, 1:3)
@generated function mygetindex(S::SubArray, indices::Real...)
    T, N, A, I = S.parameters
    if N != length(indices)
        error("Wrong number of indices supplied")
    end
    Ip = I.parameters
    NP = length(Ip)
    indexexprs = Vector{Expr}(undef, NP)
    j = 1
    for i = 1:NP
        if Ip[i] == Int
            indexexprs[i] = :(S.indices[$i])
        else
            indexexprs[i] = :(S.indices[$i][indices[$j]])
            j += 1
        end
    end
    ex = :(S.parent[$(indexexprs...)])
    ex
end
@test mygetindex(B,2,2) == A[2,2,2]

# issue #8497
module MyTest8497
internalfunction(x) = x+1
@generated function h(x)
    quote
        internalfunction(x)
    end
end
end
@test MyTest8497.h(3) == 4

# static parameters (issue #8505)
@generated function foo1(a::Array{T,N}) where {N,T}
    "N = $N, T = $T"
end
@generated function foo2(a::Array{T,N}) where {T,N}
    "N = $N, T = $T"
end
@test foo1(randn(3,3)) == "N = 2, T = Float64"
@test foo2(randn(3,3)) == "N = 2, T = Float64"

# issue #9088
@generated function f9088(x, a=5)
    :(x+a)
end
@test f9088(7) == 12

# issue #10502
@generated function f10502(x...)
    :($x)
end
f10502() = ()
@test f10502(1) == (Int,)
@test f10502(1,2) == (Int,Int)
@test f10502(1,2,3) == (Int,Int,Int)

# One-line @generated functions
@generated oneliner(x,y) = :($x, x, $y, y)
@test oneliner(1, 2.) == (Int, 1, Float64, 2.)

# issue #11982
@generated function f11982(T)
    string(T.parameters[1])
end
@test f11982(Float32) == "Float32"
@test f11982(Int32) == "Int32"

# @generated functions that throw (shouldn't segfault or throw)
module TestGeneratedThrow
    using Test, Random

    @generated function bar(x)
        error("I'm not happy with type $x")
    end

    foo() = (bar(rand() > 0.5 ? 1 : 1.0); error("foo"))
    inited = false
    function __init__()
        code_typed(foo, (); optimize = false)
        @cfunction(foo, Cvoid, ())
        global inited = true
    end
    inited = false
end
@test TestGeneratedThrow.inited

# @generated functions including inner functions
@generated function _g_f_with_inner(x)
    return :(y -> y)
end
@test_throws ErrorException _g_f_with_inner(1)

@generated function _g_f_with_inner2(x)
    return y -> y
end
@test _g_f_with_inner2(1)(2) == 2

# @generated functions errors
const gf_err_ref = Ref{Int}()

gf_err_ref[] = 0
let gf_err, tsk = @async nothing # create a Task for yield to try to run
    @generated function gf_err()
        gf_err_ref[] += 1
        yield()
        gf_err_ref[] += 1000
    end
    Expected = ErrorException("task switch not allowed from inside staged nor pure functions")
    @test_throws Expected gf_err()
    @test_throws Expected gf_err()
    @test gf_err_ref[] < 1000
end

gf_err_ref[] = 0
let gf_err2
    @generated function gf_err2(::f) where {f}
        gf_err_ref[] += 1
        reflect = f.instance
        gf_err_ref[] += 10
        reflect(+, (Int, Int))
        gf_err_ref[] += 1000
        return nothing
    end
    Expected = ErrorException("code reflection cannot be used from generated functions")
    @test_throws Expected gf_err2(code_lowered)
    @test_throws Expected gf_err2(code_typed)
    @test_throws Expected gf_err2(code_llvm)
    @test_throws Expected gf_err2(code_native)
    @test gf_err_ref[] < 1000
end

# issue #15043
decorated = Set{DataType}()
let
    @generated function decorate(t)
        push!(decorated, t)
    end

    foo() = return nothing
    decorate(foo)
    @test in(typeof(foo), decorated)

    bar() = return 1
    decorate(bar)
    @test in(typeof(bar), decorated)
end

# issue #19897
@test code_lowered(staged_t1, (Int,Int)) isa Array  # check no error thrown

# issue #10178
@generated function f10178(x::X) where X
    :(x)
end
g10178(x) = f10178(x)
@test g10178(5) == 5
@generated function f10178(x::X) where X
    :(2x)
end
g10178(x) = f10178(x)
@test g10178(5) == 10

# issue #22135
@generated f22135(x::T) where T = x
@test f22135(1) === Int

# PR #22440

f22440kernel(x...) = x[1] + x[1]
f22440kernel(x::AbstractFloat) = x * x
f22440kernel(::Type{T}) where {T} = one(T)
f22440kernel(::Type{T}) where {T<:AbstractFloat} = zero(T)

function f22440_gen(world::UInt, source, _, y)
    match = only(Base._methods_by_ftype(Tuple{typeof(f22440kernel),y}, -1, world))
    code_info = Base.uncompressed_ir(match.method)
    Meta.partially_inline!(code_info.code, Any[], match.spec_types, Any[match.sparams...], 0, 0, :propagate)
    # TODO: this is mandatory: code_info.min_world = max(code_info.min_world, min_world[])
    # TODO: this is mandatory: code_info.max_world = min(code_info.max_world, max_world[])
    return code_info
end
@eval function f22440(y)
    $(Expr(:meta, :generated, f22440_gen))
    $(Expr(:meta, :generated_only))
end

@test f22440(Int) === f22440kernel(Int)
@test f22440(Float64) === f22440kernel(Float64)
@test f22440(Float32) === f22440kernel(Float32)
@test f22440(0.0) === f22440kernel(0.0)
@test f22440(0.0f0) === f22440kernel(0.0f0)
@test f22440(0) === f22440kernel(0)

# PR #23168

@eval function f23168(a, x)
    push!(a, 1)
    if @generated
        :(y = $(+)(x, x))
    else
        y = $(*)(2, x)
    end
    push!(a, y)
    if @generated
        :(y = (y, $x))
    else
        y = (y, typeof(x))
    end
    push!(a, 3)
    return y
end

let a = Any[]
    @test f23168(a, 3) == (6, Int)
    @test a == [1, 6, 3]
    @test occursin("(+)(", string(code_lowered(f23168, (Vector{Any},Int))))
    @test occursin("(*)(2", string(Base.uncompressed_ir(first(methods(f23168)))))
    @test occursin("(*)(2", string(code_lowered(f23168, (Vector{Any},Int), generated=false)))
    @test occursin("Base.add_int", string(code_typed(f23168, (Vector{Any},Int))))
end

# issue #18747
@test_throws ErrorException eval(:(f(x) = @generated g() = x))

@generated function f30284(x)
    quote
        local x
    end
end

@test_throws ErrorException("syntax: local variable name \"x\" conflicts with an argument") f30284(1)

# issue #33243
@generated function f33243()
    :(global x33243 = 2)
end
@test_throws ErrorException f33243()
global x33243::Any
@test f33243() === 2
@test x33243 === 2

# https://github.com/JuliaDebug/CassetteOverlay.jl/issues/12
# generated function with varargs and unfortunately placed unused slot
@generated function f_vararg_generated(args...)
    local unusedslot4
    local unusedslot5
    local unusedslot6
    :($args)
end
g_vararg_generated() = f_vararg_generated((;), (;), Base.inferencebarrier((;)))
let tup = g_vararg_generated()
    @test all(==(typeof((;))), tup)
    # This is just to make sure that the test is actually testing what we want:
    # the test only works if there is an unused that matches the position of
    # the inferencebarrier argument above (N.B. the generator function itself
    # shifts everything over by 1)
    @test_broken only(code_lowered(only(methods(f_vararg_generated)).generator.gen)).slotflags[5] == 0x00
end

# respect a given linetable in code generation
# https://github.com/JuliaLang/julia/pull/47750
let world = Base.get_world_counter()
    match = Base._which(Tuple{typeof(sin), Int}; world)
    mi = Core.Compiler.specialize_method(match)
    lwr = Core.Compiler.retrieve_code_info(mi, world)
    nstmts = length(lwr.code)
    di = Core.DebugInfo(Core.Compiler.DebugInfoStream(mi, lwr.debuginfo, nstmts), nstmts)
    lwr.debuginfo = di
    @eval function sin_generated(a)
        $(Expr(:meta, :generated, Returns(lwr)))
        $(Expr(:meta, :generated_only))
    end
    src = only(code_lowered(sin_generated, (Int,)))
    @test src.debuginfo === di
    @test sin_generated(42) == sin(42)
end

# Allow passing unreachable insts in generated codeinfo
let
    dummy() = return
    dummy_m = which(dummy, Tuple{})

    src = Base.uncompressed_ir(dummy_m)
    src.code = Any[
        # block 1
        Core.ReturnNode(nothing),
        # block 2
        Core.ReturnNode(),
    ]
    nstmts = length(src.code)
    nslots = 1
    src.ssavaluetypes = nstmts
    src.debuginfo = Core.DebugInfo(:f_unreachable_generated)
    src.ssaflags = fill(Int32(0), nstmts)
    src.slotflags = fill(0, nslots)
    src.slottypes = Any[Any]

    @eval function f_unreachable()
        $(Expr(:meta, :generated, Returns(src)))
        $(Expr(:meta, :generated_only))
    end

    ir, _ = Base.code_ircode(f_unreachable, ()) |> only
    @test length(ir.cfg.blocks) == 1
end

function generate_lambda_ex(world::UInt, source::Method,
                            argnames, spnames, @nospecialize body)
    stub = Core.GeneratedFunctionStub(identity, Core.svec(argnames...), Core.svec(spnames...))
    return stub(world, source, body)
end

# Test that `Core.CachedGenerator` works as expected
struct Generator54916 <: Core.CachedGenerator end
function (::Generator54916)(world::UInt, source::Method, args...)
    return generate_lambda_ex(world, source,
        (:doit54916, :func, :arg), (), :(func(arg)))
end
@eval function doit54916(func, arg)
    $(Expr(:meta, :generated, Generator54916()))
    $(Expr(:meta, :generated_only))
end
@test doit54916(sin, 1) == sin(1)
let mi = only(methods(doit54916)).specializations
    ci = mi.cache::Core.CodeInstance
    found = false
    while true
        if ci.owner === :uninferred && ci.inferred isa Core.CodeInfo
            found = true
            break
        end
        isdefined(ci, :next) || break
        ci = ci.next
    end
    @test found
end

# Test that writing a bad cassette-style pass gives the expected error (#49715)
function generator49715(world, source, self, f, tt)
    tt = tt.parameters[1]
    sig = Tuple{f, tt.parameters...}
    mi = Base._which(sig; world)
    error("oh no")
    return generate_lambda_ex(world, source,
        (:doit49715, :f, :tt), (), nothing)
end
@eval function doit49715(f, tt)
    $(Expr(:meta, :generated, generator49715))
    $(Expr(:meta, :generated_only))
end
@test_throws "oh no" doit49715(sin, Tuple{Int})

# Test that the CodeInfo returned from generated function need not match the generator.
function overdubbee54341(a, b)
    a + b
end
const overdubee_codeinfo54341 = code_lowered(overdubbee54341, Tuple{Any, Any})[1]
function overdub_generator54341(world::UInt, source::Method, selftype, fargtypes)
    if length(fargtypes) != 2
        return generate_lambda_ex(world, source,
            (:overdub54341, :args), (), :(error("Wrong number of arguments")))
    else
        return copy(overdubee_codeinfo54341)
    end
end
@eval function overdub54341(args...)
    $(Expr(:meta, :generated, overdub_generator54341))
    $(Expr(:meta, :generated_only))
end
@test overdub54341(1, 2) == 3
# check if the inlining pass handles `nargs`/`isva` correctly
@test first(only(code_typed((Int,Int)) do x, y; @inline overdub54341(x, y); end)) isa Core.CodeInfo
@test first(only(code_typed((Int,)) do x; @inline overdub54341(x, 1); end)) isa Core.CodeInfo
@test_throws "Wrong number of arguments" overdub54341(1, 2, 3)

# Test the module resolution scope of generated methods that are type constructors
module GeneratedScope57417
    using Test
    import ..generate_lambda_ex
    const x = 1
    struct Generator; end
    @generated (::Generator)() = :x
    f(x::Int) = 1
    module OtherModule
        import ..f
        const x = 2
        @generated f(::Float64) = :x
    end
    import .OtherModule: f
    @test Generator()() == 1
    @test f(1.0) == 2

    function g_generator(world::UInt, source::Method, _)
        return generate_lambda_ex(world, source, (:g,), (), :(return x))
    end

    @eval function g()
        $(Expr(:meta, :generated, g_generator))
        $(Expr(:meta, :generated_only))
    end
    @test g() == 1
end
