# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

# code_native / code_llvm (issue #8239)
# It's hard to really test these, but just running them should be
# sufficient to catch segfault bugs.

module ReflectionTest
using Test, Random

function test_ast_reflection(freflect, f, types)
    @test !isempty(freflect(f, types))
    nothing
end

function test_bin_reflection(freflect, f, types)
    iob = IOBuffer()
    freflect(iob, f, types)
    str = String(take!(iob))
    @test !isempty(str)
    nothing
end

function test_code_reflection(freflect, f, types, tester)
    tester(freflect, f, types)
    tester(freflect, f, (types.parameters...,))
    nothing
end

function test_code_reflections(tester, freflect)
    test_code_reflection(freflect, occursin,
                         Tuple{Regex, AbstractString}, tester) # abstract type
    test_code_reflection(freflect, +, Tuple{Int, Int}, tester) # leaftype signature
    test_code_reflection(freflect, +,
                         Tuple{Array{Float32}, Array{Float32}}, tester) # incomplete types
    test_code_reflection(freflect, Module, Tuple{}, tester) # Module() constructor (transforms to call)
    test_code_reflection(freflect, Array{Int64}, Tuple{Array{Int32}}, tester) # with incomplete types
    test_code_reflection(freflect, muladd, Tuple{Float64, Float64, Float64}, tester)
end

test_code_reflections(test_ast_reflection, code_lowered)
test_code_reflections(test_ast_reflection, code_typed)

end # module ReflectionTest

# isbits, isbitstype

@test !isbitstype(Array{Int})
@test isbitstype(Float32)
@test isbitstype(Int)
@test !isbitstype(AbstractString)
@test isbitstype(Tuple{Int, Vararg{Int, 2}})
@test !isbitstype(Tuple{Int, Vararg{Int}})
@test !isbitstype(Tuple{Integer, Vararg{Int, 2}})
@test isbitstype(Tuple{Int, Vararg{Any, 0}})
@test isbitstype(Tuple{Vararg{Any, 0}})
@test isbits(1)
@test isbits((1,2))
@test !isbits([1])
@test isbits(nothing)

# issue #16670
@test isconcretetype(Int)
@test isconcretetype(Vector{Int})
@test isconcretetype(Tuple{Int, Vararg{Int, 2}})
@test !isconcretetype(Tuple{Any})
@test !isconcretetype(Tuple{Integer, Vararg{Int, 2}})
@test !isconcretetype(Tuple{Int, Vararg{Int}})
@test !isconcretetype(Type{Tuple{Integer, Vararg{Int}}})
@test !isconcretetype(Type{Vector})
@test !isconcretetype(Type{Int})
@test !isconcretetype(Tuple{Type{Int}})
@test isconcretetype(DataType)
@test isconcretetype(Union)
@test !isconcretetype(Union{})
@test isconcretetype(Tuple{Union{}})
@test !isconcretetype(Complex)
@test !isconcretetype(Complex.body)
@test !isconcretetype(AbstractArray{Int,1})
struct AlwaysHasLayout{T}
    x
end
@test !isconcretetype(AlwaysHasLayout) && !isconcretetype(AlwaysHasLayout.body)
@test isconcretetype(AlwaysHasLayout{Any})
@test isconcretetype(Ptr{Cvoid})
@test !isconcretetype(Ptr) && !isconcretetype(Ptr.body)

# issue #10165
i10165(::Type) = 0
i10165(::Type{AbstractArray{T,n}}) where {T,n} = 1
@test i10165(AbstractArray{Int,n} where n) == 0
@test which(i10165, Tuple{Type{AbstractArray{Int,n} where n},}).sig == Tuple{typeof(i10165),Type}

# fullname
@test fullname(Base) == (:Base,)
@test fullname(Base.Iterators) == (:Base, :Iterators)

const a_const = 1
not_const = 1
@test isconst(@__MODULE__, :a_const) == true
@test isconst(Base, :pi) == true
@test isconst(@__MODULE__, :pi) == true
@test isconst(@__MODULE__, :not_const) == false
@test isconst(@__MODULE__, :is_not_defined) == false

@test ismutable(1) == false
@test ismutable([]) == true

## find bindings tests
@test ccall(:jl_get_module_of_binding, Any, (Any, Any), Base, :sin)==Base

# For curmod_*
include("testenv.jl")

module TestMod7648
using Test
import Base.convert
import ..curmod_name, ..curmod
export a9475, foo9475, c7648, foo7648, foo7648_nomethods, Foo7648

const c7648 = 8
d7648 = 9
const f7648 = 10
foo7648(x) = x
function foo7648_nomethods end
mutable struct Foo7648 end

module TestModSub9475
    using Test
    using ..TestMod7648
    import ..curmod_name
    export a9475, foo9475
    a9475 = 5
    b9475 = 7
    foo9475(x) = x
    let
        @test Base.binding_module(@__MODULE__, :a9475) == @__MODULE__
        @test Base.binding_module(@__MODULE__, :c7648) == TestMod7648
        @test Base.nameof(@__MODULE__) == :TestModSub9475
        @test Base.fullname(@__MODULE__) == (curmod_name..., :TestMod7648, :TestModSub9475)
        @test Base.parentmodule(@__MODULE__) == TestMod7648
    end
end # module TestModSub9475

using .TestModSub9475

let
    @test Base.binding_module(@__MODULE__, :d7648) == @__MODULE__
    @test Base.binding_module(@__MODULE__, :a9475) == TestModSub9475
    @test Base.nameof(@__MODULE__) == :TestMod7648
    @test Base.parentmodule(@__MODULE__) == curmod
end
end # module TestMod7648

let
    @test Base.binding_module(TestMod7648, :d7648) == TestMod7648
    @test Base.binding_module(TestMod7648, :a9475) == TestMod7648.TestModSub9475
    @test Base.binding_module(TestMod7648.TestModSub9475, :b9475) == TestMod7648.TestModSub9475
    @test Set(names(TestMod7648))==Set([:TestMod7648, :a9475, :foo9475, :c7648, :foo7648, :foo7648_nomethods, :Foo7648])
    @test Set(names(TestMod7648, all = true)) == Set([:TestMod7648, :TestModSub9475, :a9475, :foo9475, :c7648, :d7648, :f7648,
                                                :foo7648, Symbol("#foo7648"), :foo7648_nomethods, Symbol("#foo7648_nomethods"),
                                                :Foo7648, :eval, Symbol("#eval"), :include, Symbol("#include")])
    @test Set(names(TestMod7648, all = true, imported = true)) == Set([:TestMod7648, :TestModSub9475, :a9475, :foo9475, :c7648, :d7648, :f7648,
                                                      :foo7648, Symbol("#foo7648"), :foo7648_nomethods, Symbol("#foo7648_nomethods"),
                                                      :Foo7648, :eval, Symbol("#eval"), :include, Symbol("#include"),
                                                      :convert, :curmod_name, :curmod])
    @test isconst(TestMod7648, :c7648)
    @test !isconst(TestMod7648, :d7648)
end

let
    using .TestMod7648
    @test Base.binding_module(@__MODULE__, :a9475) == TestMod7648.TestModSub9475
    @test Base.binding_module(@__MODULE__, :c7648) == TestMod7648
    @test nameof(foo7648) == :foo7648
    @test parentmodule(foo7648, (Any,)) == TestMod7648
    @test parentmodule(foo7648) == TestMod7648
    @test parentmodule(foo7648_nomethods) == TestMod7648
    @test parentmodule(foo9475, (Any,)) == TestMod7648.TestModSub9475
    @test parentmodule(foo9475) == TestMod7648.TestModSub9475
    @test parentmodule(Foo7648) == TestMod7648
    @test nameof(Foo7648) == :Foo7648
    @test basename(functionloc(foo7648, (Any,))[1]) == "reflection.jl"
    @test first(methods(TestMod7648.TestModSub9475.foo7648)) == which(foo7648, (Int,))
    @test TestMod7648 == which(@__MODULE__, :foo7648)
    @test TestMod7648.TestModSub9475 == which(@__MODULE__, :a9475)
end

@test_throws ArgumentError("argument is not a generic function") which(===, Tuple{Int, Int})
@test_throws ArgumentError("argument is not a generic function") code_typed(===, Tuple{Int, Int})
@test_throws ArgumentError("argument is not a generic function") Base.return_types(===, Tuple{Int, Int})

module TestingExported
using Test
include("testenv.jl") # for curmod_str
import Base.isexported
global this_is_not_defined
export this_is_not_defined
@test_throws ErrorException("\"this_is_not_defined\" is not defined in module Main") which(Main, :this_is_not_defined)
@test_throws ErrorException("\"this_is_not_exported\" is not defined in module Main") which(Main, :this_is_not_exported)
@test isexported(@__MODULE__, :this_is_not_defined)
@test !isexported(@__MODULE__, :this_is_not_exported)
const a_value = 1
@test which(@__MODULE__, :a_value) === @__MODULE__
@test_throws ErrorException("\"a_value\" is not defined in module Main") which(Main, :a_value)
@test which(Main, :Core) === Main
@test !isexported(@__MODULE__, :a_value)
end

# PR 13825
let ex = :(a + b)
    @test string(ex) == "a + b"
end
foo13825(::Array{T, N}, ::Array, ::Vector) where {T, N} = nothing
@test startswith(string(first(methods(foo13825))),
                 "foo13825(::Array{T,N}, ::Array, ::Array{T,1} where T)")

mutable struct TLayout
    x::Int8
    y::Int16
    z::Int32
end
tlayout = TLayout(5,7,11)
@test fieldnames(TLayout) == (:x, :y, :z) == Base.propertynames(tlayout)
@test hasfield(TLayout, :y)
@test !hasfield(TLayout, :a)
@test hasproperty(tlayout, :x)
@test !hasproperty(tlayout, :p)
@test [(fieldoffset(TLayout,i), fieldname(TLayout,i), fieldtype(TLayout,i)) for i = 1:fieldcount(TLayout)] ==
    [(0, :x, Int8), (2, :y, Int16), (4, :z, Int32)]
@test fieldnames(Complex) === (:re, :im)
@test_throws BoundsError fieldtype(TLayout, 0)
@test_throws ArgumentError fieldname(TLayout, 0)
@test_throws BoundsError fieldoffset(TLayout, 0)
@test_throws BoundsError fieldtype(TLayout, 4)
@test_throws ArgumentError fieldname(TLayout, 4)
@test_throws BoundsError fieldoffset(TLayout, 4)

@test fieldtype(Tuple{Vararg{Int8}}, 1) === Int8
@test fieldtype(Tuple{Vararg{Int8}}, 10) === Int8
@test_throws BoundsError fieldtype(Tuple{Vararg{Int8}}, 0)
# issue #30505
@test fieldtype(Union{Tuple{Char},Tuple{Char,Char}},2) === Char
@test_throws BoundsError fieldtype(Union{Tuple{Char},Tuple{Char,Char}},3)

@test fieldnames(NTuple{3, Int}) == ntuple(i -> fieldname(NTuple{3, Int}, i), 3) == (1, 2, 3)
@test_throws ArgumentError fieldnames(Union{})
@test_throws BoundsError fieldname(NTuple{3, Int}, 0)
@test_throws BoundsError fieldname(NTuple{3, Int}, 4)

@test fieldnames(NamedTuple{(:z,:a)}) === (:z,:a)
@test fieldname(NamedTuple{(:z,:a)}, 1) === :z
@test fieldname(NamedTuple{(:z,:a)}, 2) === :a
@test_throws ArgumentError fieldname(NamedTuple{(:z,:a)}, 3)
@test_throws ArgumentError fieldnames(NamedTuple)
@test_throws ArgumentError fieldnames(NamedTuple{T,Tuple{Int,Int}} where T)
@test_throws ArgumentError fieldnames(Real)
@test_throws ArgumentError fieldnames(AbstractArray)

@test fieldtype((NamedTuple{T,Tuple{Int,String}} where T), 1) === Int
@test fieldtype((NamedTuple{T,Tuple{Int,String}} where T), 2) === String
@test_throws BoundsError fieldtype((NamedTuple{T,Tuple{Int,String}} where T), 3)

@test fieldtype(NamedTuple, 42) === Any
@test_throws BoundsError fieldtype(NamedTuple, 0)
@test_throws BoundsError fieldtype(NamedTuple, -1)

@test fieldtype(NamedTuple{(:a,:b)}, 1) === Any
@test fieldtype(NamedTuple{(:a,:b)}, 2) === Any
@test fieldtype((NamedTuple{(:a,:b),T} where T<:Tuple{Vararg{Integer}}), 2) === Integer
@test_throws BoundsError fieldtype(NamedTuple{(:a,:b)}, 3)

# issue #32697
@test fieldtype(NamedTuple{(:x,:y), T} where T <: Tuple{Int, Union{Float64, Missing}}, :x) == Int
@test fieldtype(NamedTuple{(:x,:y), T} where T <: Tuple{Int, Union{Float64, Missing}}, :y) == Union{Float64, Missing}

@test fieldtypes(NamedTuple{(:a,:b)}) == (Any, Any)
@test fieldtypes((NamedTuple{T,Tuple{Int,String}} where T)) === (Int, String)
@test fieldtypes(TLayout) === (Int8, Int16, Int32)

import Base: datatype_alignment, return_types
@test datatype_alignment(UInt16) == 2
@test datatype_alignment(TLayout) == 4
let rts = return_types(TLayout)
    @test length(rts) == 2 # general constructor and specific constructor
    @test all(rts .== TLayout)
end

# issue #15447
@noinline function f15447(s, a)
    if s
        return a
    else
        nb = 0
        return nb
    end
end
@test functionloc(f15447)[2] > 0

# issue #14346
@noinline function f14346(id, mask, limit)
    if id <= limit && mask[id]
        return true
    end
end
@test functionloc(f14346)[2] == @__LINE__() - 4

# test jl_get_llvm_fptr. We test functions both in and definitely not in the system image
definitely_not_in_sysimg() = nothing
for (f, t) in Any[(definitely_not_in_sysimg, Tuple{}),
                  (Base.:+, Tuple{Int, Int})]
    meth = which(f, t)
    tt = Tuple{typeof(f), t.parameters...}
    (ti, env) = ccall(:jl_type_intersection_with_env, Any, (Any, Any), tt, meth.sig)::Core.SimpleVector
    @test ti === tt # intersection should be a subtype
    world = Core.Compiler.get_world_counter()
    linfo = ccall(:jl_specializations_get_linfo, Ref{Core.MethodInstance}, (Any, Any, Any, UInt), meth, tt, env, world)
    params = Base.CodegenParams()
    llvmf1 = ccall(:jl_get_llvmf_decl, Ptr{Cvoid}, (Any, UInt, Bool, Base.CodegenParams), linfo::Core.MethodInstance, world, true, params)
    @test llvmf1 != C_NULL
    llvmf2 = ccall(:jl_get_llvmf_decl, Ptr{Cvoid}, (Any, UInt, Bool, Base.CodegenParams), linfo::Core.MethodInstance, world, false, params)
    @test llvmf2 != C_NULL
    @test ccall(:jl_get_llvm_fptr, Ptr{Cvoid}, (Ptr{Cvoid},), llvmf1) != C_NULL
    @test ccall(:jl_get_llvm_fptr, Ptr{Cvoid}, (Ptr{Cvoid},), llvmf2) != C_NULL
end

# issue #15714
# show variable names for slots and suppress spurious type warnings
function f15714(array_var15714)
    for index_var15714 in eachindex(array_var15714)
        array_var15714[index_var15714] += 0
    end
end

function g15714(array_var15714)
    for index_var15714 in eachindex(array_var15714)
        array_var15714[index_var15714] += 0
    end
    let index_var15714
        for outer index_var15714 in eachindex(array_var15714)
            array_var15714[index_var15714] += 0
        end
        index_var15714
    end
    let index_var15714
        for outer index_var15714 in eachindex(array_var15714)
            array_var15714[index_var15714] += 0
        end
        index_var15714
    end
end

import InteractiveUtils.code_warntype

used_dup_var_tested15714 = false
used_unique_var_tested15714 = false
function test_typed_ast_printing(Base.@nospecialize(f), Base.@nospecialize(types), must_used_vars)
    src, rettype = code_typed(f, types, optimize=false)[1]
    dupnames = Set()
    slotnames = Set()
    for name in src.slotnames
        if name in slotnames || name === Symbol("")
            push!(dupnames, name)
        else
            push!(slotnames, name)
        end
    end
    # Make sure must_used_vars are in slotnames
    for name in must_used_vars
        @test name in slotnames
    end
    must_used_checked = Dict{Symbol,Bool}()
    for sym in must_used_vars
        must_used_checked[sym] = false
    end
    for str in (sprint(io -> code_warntype(io, f, types, optimize=false)),
                repr("text/plain", src))
        for var in must_used_vars
            @test occursin(string(var), str)
        end
        # Check that we are not printing the bare slot numbers
        for i in 1:length(src.slotnames)
            name = src.slotnames[i]
            if name in dupnames
                if name in must_used_vars && occursin(Regex("_$i\\b"), str)
                    must_used_checked[name] = true
                    global used_dup_var_tested15714 = true
                end
            else
                @test !occursin(Regex("_$i\\b"), str)
                if name in must_used_vars
                    global used_unique_var_tested15714 = true
                end
            end
        end
    end
    for sym in must_used_vars
        if sym in dupnames
            @test must_used_checked[sym]
        end
        must_used_checked[sym] = false
    end
    # Make sure printing an AST outside CodeInfo still works.
    str = sprint(show, src.code)
    # Check that we are printing the slot numbers when we don't have the context
    # Use the variable names that we know should be present in the optimized AST
    for i in 2:length(src.slotnames)
        name = src.slotnames[i]
        if name in must_used_vars && occursin(Regex("_$i\\b"), str)
            must_used_checked[name] = true
        end
    end
    for sym in must_used_vars
        @test must_used_checked[sym]
    end
end
test_typed_ast_printing(f15714, Tuple{Vector{Float32}},
                        [:array_var15714,  :index_var15714])
test_typed_ast_printing(g15714, Tuple{Vector{Float32}},
                        [:array_var15714,  :index_var15714])
#This test doesn't work with the new optimizer because we drop slotnames
#We may want to test it against debug info eventually
#@test used_dup_var_tested15715
@test used_unique_var_tested15714

let li = typeof(fieldtype).name.mt.cache.func::Core.MethodInstance,
    lrepr = string(li),
    mrepr = string(li.def),
    lmime = repr("text/plain", li),
    mmime = repr("text/plain", li.def)

    @test lrepr == lmime == "MethodInstance for fieldtype(...)"
    @test mrepr == mmime == "fieldtype(...) in Core"
end


# Linfo Tracing test
function tracefoo end
# Method Tracing test
methtracer(x::Ptr{Cvoid}) = (@test isa(unsafe_pointer_to_objref(x), Method); global didtrace = true; nothing)
let cmethtracer = @cfunction(methtracer, Cvoid, (Ptr{Cvoid},))
    ccall(:jl_register_newmeth_tracer, Cvoid, (Ptr{Cvoid},), cmethtracer)
end
didtrace = false
tracefoo2(x, y) = x*y
@test didtrace
didtrace = false
tracefoo(x::Int64, y::Int64) = x*y
@test didtrace
didtrace = false
ccall(:jl_register_newmeth_tracer, Cvoid, (Ptr{Cvoid},), C_NULL)

# test for reflection over large method tables
for i = 1:100; @eval fLargeTable(::Val{$i}, ::Any) = 1; end
for i = 1:100; @eval fLargeTable(::Any, ::Val{$i}) = 2; end
fLargeTable(::Any...) = 3
@test length(methods(fLargeTable, Tuple{})) == 1
fLargeTable(::Complex, ::Complex) = 4
fLargeTable(::Union{ComplexF32, ComplexF64}...) = 5
@test length(methods(fLargeTable, Tuple{})) == 1
fLargeTable() = 4
@test length(methods(fLargeTable)) == 204
@test length(methods(fLargeTable, Tuple{})) == 1
@test fLargeTable(1im, 2im) == 4
@test fLargeTable(1.0im, 2.0im) == 5
@test_throws MethodError fLargeTable(Val(1), Val(1))
@test fLargeTable(Val(1), 1) == 1
@test fLargeTable(1, Val(1)) == 2

# issue #15280
function f15280(x) end
@test functionloc(f15280)[2] > 0

# bug found in #16850, Base.url with backslashes on Windows
function module_depth(from::Module, to::Module)
    if from === to || parentmodule(to) === to
        return 0
    else
        return 1 + module_depth(from, parentmodule(to))
    end
end
function has_backslashes(mod::Module)
    for n in names(mod, all = true, imported = true)
        isdefined(mod, n) || continue
        Base.isdeprecated(mod, n) && continue
        f = getfield(mod, n)
        if isa(f, Module) && module_depth(Main, f) <= module_depth(Main, mod)
            continue
        end
        h = has_backslashes(f)
        h === nothing || return h
    end
    return nothing
end
function has_backslashes(f::Function)
    for m in methods(f)
        h = has_backslashes(m)
        h === nothing || return h
    end
    return nothing
end
function has_backslashes(meth::Method)
    if '\\' in string(meth.file)
        return meth
    else
        return nothing
    end
end
has_backslashes(x) = nothing
h16850 = has_backslashes(Base)
if Sys.iswindows()
    if h16850 === nothing
        @warn """No methods found in Base with backslashes in file name,
                 skipping test for `Base.url`"""
    else
        @test !('\\' in Base.url(h16850))
    end
else
    @test h16850 === nothing
end

# PR #18888: code_typed shouldn't cache, return_types should
f18888() = nothing
let
    world = Core.Compiler.get_world_counter()
    m = first(methods(f18888, Tuple{}))
    @test m.specializations === nothing
    ft = typeof(f18888)

    code_typed(f18888, Tuple{}; optimize=false)
    @test m.specializations isa Core.TypeMapEntry  # uncached, but creates the specializations entry
    mi = Core.Compiler.specialize_method(m, Tuple{ft}, Core.svec())
    @test Core.Compiler.inf_for_methodinstance(mi, world) === nothing
    @test !isdefined(mi, :cache)

    code_typed(f18888, Tuple{}; optimize=true)
    @test !isdefined(mi, :cache)

    Base.return_types(f18888, Tuple{})
    @test Core.Compiler.inf_for_methodinstance(mi, world) === mi.cache
    @test mi.cache isa Core.CodeInstance
    @test !isdefined(mi.cache, :next)
end

# New reflection methods in 0.6
struct ReflectionExample{T<:AbstractFloat, N}
    x::Tuple{T, N}
end

@test !isabstracttype(Union{})
@test !isabstracttype(Union{Int,Float64})
@test isabstracttype(AbstractArray)
@test isabstracttype(AbstractSet{Int})
@test !isabstracttype(ReflectionExample)
@test !isabstracttype(Int)
@test !isabstracttype(TLayout)

@test !isprimitivetype(Union{})
@test !isprimitivetype(Union{Int,Float64})
@test !isprimitivetype(AbstractArray)
@test !isprimitivetype(AbstractSet{Int})
@test !isprimitivetype(ReflectionExample)
@test isprimitivetype(Int)
@test !isprimitivetype(TLayout)

@test !isstructtype(Union{})
@test !isstructtype(Union{Int,Float64})
@test !isstructtype(AbstractArray)
@test !isstructtype(AbstractSet{Int})
@test isstructtype(ReflectionExample)
@test !isstructtype(Int)
@test isstructtype(TLayout)

@test Base.parameter_upper_bound(ReflectionExample, 1) === AbstractFloat
@test Base.parameter_upper_bound(ReflectionExample, 2) === Any
@test Base.parameter_upper_bound(ReflectionExample{T, N} where T where N <: Real, 2) === Real

let
    wrapperT(T) = Base.typename(T).wrapper
    @test @inferred wrapperT(ReflectionExample{Float64, Int64}) == ReflectionExample
    @test @inferred wrapperT(ReflectionExample{Float64, N} where N) == ReflectionExample
    @test @inferred wrapperT(ReflectionExample{T, Int64} where T) == ReflectionExample
    @test @inferred wrapperT(ReflectionExample) == ReflectionExample
    @test @inferred wrapperT(Union{ReflectionExample{Union{},1},ReflectionExample{Float64,1}}) == ReflectionExample
    @test_throws(ErrorException("typename does not apply to unions whose components have different typenames"),
                 Base.typename(Union{Int, Float64}))
end

# sizeof and nfields
@test sizeof(Int16) == 2
@test sizeof(ComplexF64) == 16
primitive type ParameterizedByte__{A,B} 8 end
@test sizeof(ParameterizedByte__) == 1
@test sizeof(nothing) == 0
@test sizeof(()) == 0
struct TypeWithIrrelevantParameter{T}
    x::Int32
end
@test sizeof(TypeWithIrrelevantParameter) == sizeof(Int32)
@test sizeof(TypeWithIrrelevantParameter{Int8}) == sizeof(Int32)
@test sizeof(:abc) == 3
@test sizeof(Symbol("")) == 0
@test_throws(ErrorException("Abstract type Real does not have a definite size."),
             sizeof(Real))
@test sizeof(Union{ComplexF32,ComplexF64}) == 16
@test sizeof(Union{Int8,UInt8}) == 1
@test_throws ErrorException sizeof(AbstractArray)
@test_throws ErrorException sizeof(Tuple)
@test_throws ErrorException sizeof(Tuple{Any,Any})
@test_throws ErrorException sizeof(String)
@test_throws ErrorException sizeof(Vector{Int})
@test_throws ErrorException sizeof(Symbol)
@test_throws ErrorException sizeof(Core.SimpleVector)
@test_throws ErrorException sizeof(Union{})

@test nfields((1,2)) == 2
@test nfields(()) == 0
@test nfields(nothing) == fieldcount(Nothing) == 0
@test nfields(1) == 0
@test_throws ArgumentError fieldcount(Union{})
@test fieldcount(Tuple{Any,Any,T} where T) == 3
@test fieldcount(Complex) == fieldcount(ComplexF32) == 2
@test fieldcount(Union{ComplexF32,ComplexF64}) == 2
@test fieldcount(Int) == 0
@test_throws(ArgumentError("type does not have a definite number of fields"),
             fieldcount(Union{Complex,Pair}))
@test_throws ArgumentError fieldcount(Real)
@test_throws ArgumentError fieldcount(AbstractArray)
@test_throws ArgumentError fieldcount(Tuple{Any,Vararg{Any}})

# PR #22979

function test_similar_codeinfo(a, b)
    @test a.code == b.code
    @test a.slotnames == b.slotnames
    @test a.slotflags == b.slotflags
end

@generated f22979(x...) = (y = 1; :(x[1] + x[2]))
let
    x22979 = (1, 2.0, 3.0 + im)
    T22979 = Tuple{typeof(f22979), typeof.(x22979)...}
    world = Core.Compiler.get_world_counter()
    mtypes, msp, m = Base._methods_by_ftype(T22979, -1, world)[1]
    instance = Core.Compiler.specialize_method(m, mtypes, msp)
    cinfo_generated = Core.Compiler.get_staged(instance)
    @test_throws ErrorException Base.uncompressed_ast(m)

    test_similar_codeinfo(code_lowered(f22979, typeof(x22979))[1], cinfo_generated)

    cinfos = code_lowered(f22979, typeof.(x22979), generated=true)
    @test length(cinfos) == 1
    cinfo = cinfos[1]
    test_similar_codeinfo(cinfo, cinfo_generated)
    @test_throws ErrorException code_lowered(f22979, typeof.(x22979), generated=false)
end


module MethodDeletion
using Test, Random

# Deletion after compiling top-level call
bar1(x) = 1
bar1(x::Int) = 2
foo1(x) = bar1(x)
faz1(x) = foo1(x)
@test faz1(1) == 2
@test faz1(1.0) == 1
m = first(methods(bar1, Tuple{Int}))
Base.delete_method(m)
@test bar1(1) == 1
@test bar1(1.0) == 1
@test foo1(1) == 1
@test foo1(1.0) == 1
@test faz1(1) == 1
@test faz1(1.0) == 1

# Deletion after compiling middle-level call
bar2(x) = 1
bar2(x::Int) = 2
foo2(x) = bar2(x)
faz2(x) = foo2(x)
@test foo2(1) == 2
@test foo2(1.0) == 1
m = first(methods(bar2, Tuple{Int}))
Base.delete_method(m)
@test bar2(1.0) == 1
@test bar2(1) == 1
@test foo2(1) == 1
@test foo2(1.0) == 1
@test faz2(1) == 1
@test faz2(1.0) == 1

# Deletion after compiling low-level call
bar3(x) = 1
bar3(x::Int) = 2
foo3(x) = bar3(x)
faz3(x) = foo3(x)
@test bar3(1) == 2
@test bar3(1.0) == 1
m = first(methods(bar3, Tuple{Int}))
Base.delete_method(m)
@test bar3(1) == 1
@test bar3(1.0) == 1
@test foo3(1) == 1
@test foo3(1.0) == 1
@test faz3(1) == 1
@test faz3(1.0) == 1

# Deletion before any compilation
bar4(x) = 1
bar4(x::Int) = 2
foo4(x) = bar4(x)
faz4(x) = foo4(x)
m = first(methods(bar4, Tuple{Int}))
Base.delete_method(m)
@test bar4(1) == 1
@test bar4(1.0) == 1
@test foo4(1) == 1
@test foo4(1.0) == 1
@test faz4(1) == 1
@test faz4(1.0) == 1

# Methods with keyword arguments
fookw(x; direction=:up) = direction
fookw(y::Int) = 2
@test fookw("string") == :up
@test fookw(1) == 2
m = collect(methods(fookw))[2]
Base.delete_method(m)
@test fookw(1) == 2
@test_throws MethodError fookw("string")

# functions with many methods
types = (Float64, Int32, String)
for T1 in types, T2 in types, T3 in types
    @eval foomany(x::$T1, y::$T2, z::$T3) = y
end
@test foomany(Int32(5), "hello", 3.2) == "hello"
m = first(methods(foomany, Tuple{Int32, String, Float64}))
Base.delete_method(m)
@test_throws MethodError foomany(Int32(5), "hello", 3.2)

struct EmptyType end
Base.convert(::Type{EmptyType}, x::Integer) = EmptyType()
m = first(methods(convert, Tuple{Type{EmptyType}, Integer}))
Base.delete_method(m)
@test_throws MethodError convert(EmptyType, 1)

# parametric methods
parametric(A::Array{T,N}, i::Vararg{Int,N}) where {T,N} = N
@test parametric(rand(2,2), 1, 1) == 2
m = first(methods(parametric))
Base.delete_method(m)
@test_throws MethodError parametric(rand(2,2), 1, 1)

# Deletion and ambiguity detection
foo(::Int, ::Int) = 1
foo(::Real, ::Int) = 2
foo(::Int, ::Real) = 3
@test all(map(g->g.ambig==nothing, methods(foo)))
Base.delete_method(first(methods(foo)))
@test !all(map(g->g.ambig==nothing, methods(foo)))
@test_throws MethodError foo(1, 1)
foo(::Int, ::Int) = 1
foo(1, 1)
@test map(g->g.ambig==nothing, methods(foo)) == [true, false, false]
Base.delete_method(first(methods(foo)))
@test_throws MethodError foo(1, 1)
@test map(g->g.ambig==nothing, methods(foo)) == [false, false]

# multiple deletions and ambiguities
typeparam(::Type{T}, a::Array{T}) where T<:AbstractFloat = 1
typeparam(::Type{T}, a::Array{T}) where T = 2
for mth in collect(methods(typeparam))
    Base.delete_method(mth)
end
typeparam(::Type{T}, a::AbstractArray{T}) where T<:AbstractFloat = 1
typeparam(::Type{T}, a::AbstractArray{T}) where T = 2
@test typeparam(Float64, rand(2))  == 1
@test typeparam(Int, rand(Int, 2)) == 2

# prior ambiguities (issue #28899)
uambig(::Union{Int,Nothing}) = 1
uambig(::Union{Float64,Nothing}) = 2
@test uambig(1) == 1
@test uambig(1.0) == 2
@test_throws MethodError uambig(nothing)
m = which(uambig, Tuple{Int})
Base.delete_method(m)
@test_throws MethodError uambig(1)
@test uambig(1.0) == 2
@test uambig(nothing) == 2

end

module HasmethodKwargs
using Test
f(x::Int; y=3) = x + y
@test hasmethod(f, Tuple{Int})
@test hasmethod(f, Tuple{Int}, ())
@test hasmethod(f, Tuple{Int}, (:y,))
@test !hasmethod(f, Tuple{Int}, (:jeff,))
@test !hasmethod(f, Tuple{Int}, (:y,), world=typemin(UInt))
g(; b, c, a) = a + b + c
h(; kwargs...) = 4
for gh = (g, h)
    @test hasmethod(gh, Tuple{})
    @test hasmethod(gh, Tuple{}, ())
    @test hasmethod(gh, Tuple{}, (:a,))
    @test hasmethod(gh, Tuple{}, (:a, :b))
    @test hasmethod(gh, Tuple{}, (:a, :b, :c))
end
@test !hasmethod(g, Tuple{}, (:a, :b, :c, :d))
@test hasmethod(h, Tuple{}, (:a, :b, :c, :d))
end

# issue #31353
function f31353(f, x::Array{<:Dict})
end
@test  hasmethod(f31353, Tuple{Any, Array{D}} where D<:Dict)
@test !hasmethod(f31353, Tuple{Any, Array{D}} where D<:AbstractDict)

# issue #26267
module M26267
import Test
foo(x) = x
end
@test !(:Test in names(M26267, all=true, imported=false))
@test :Test in names(M26267, all=true, imported=true)
@test :Test in names(M26267, all=false, imported=true)

# issue #20872
f20872(::Val{N}, ::Val{N}) where {N} = true
f20872(::Val, ::Val) = false
@test which(f20872, Tuple{Val{N},Val{N}} where N).sig == Tuple{typeof(f20872), Val{N}, Val{N}} where N
@test which(f20872, Tuple{Val,Val}).sig == Tuple{typeof(f20872), Val, Val}
@test which(f20872, Tuple{Val,Val{N}} where N).sig == Tuple{typeof(f20872), Val, Val}
@test_throws ErrorException which(f20872, Tuple{Any,Val{N}} where N)

module M29962 end
# make sure checking if a binding is deprecated does not resolve it
@test !Base.isdeprecated(M29962, :sin) && !Base.isbindingresolved(M29962, :sin)

# @locals
using Base: @locals
let
    local x, y
    global z
    @test isempty(keys(@locals))
    x = 1
    @test @locals() == Dict{Symbol,Any}(:x=>1)
    y = ""
    @test @locals() == Dict{Symbol,Any}(:x=>1,:y=>"")
    for i = 8:8
        @test @locals() == Dict{Symbol,Any}(:x=>1,:y=>"",:i=>8)
    end
    for i = 42:42
        local x
        @test @locals() == Dict{Symbol,Any}(:y=>"",:i=>42)
    end
    @test @locals() == Dict{Symbol,Any}(:x=>1,:y=>"")
    x = (y,)
    @test @locals() == Dict{Symbol,Any}(:x=>("",),:y=>"")
end

function _test_at_locals1(::Any, ::Any)
    x = 1
    @test @locals() == Dict{Symbol,Any}(:x=>1)
end
_test_at_locals1(1,1)
function _test_at_locals2(a::Any, ::Any, c::T) where T
    x = 2
    @test @locals() == Dict{Symbol,Any}(:x=>2,:a=>a,:c=>c,:T=>typeof(c))
end
_test_at_locals2(1,1,"")
_test_at_locals2(1,1,0.5f0)

@testset "issue #31687" begin
    import InteractiveUtils._dump_function

    @noinline f31687_child(i) = f31687_nonexistent(i)
    f31687_parent() = f31687_child(0)
    params = Base.CodegenParams(cached=false)
    _dump_function(f31687_parent, Tuple{},
                   #=native=#false, #=wrapper=#false, #=strip=#false,
                   #=dump_module=#true, #=syntax=#:att, #=optimize=#false, :none,
                   params)
end

@test nameof(Any) === :Any
@test nameof(:) === :Colon
@test nameof(Core.Intrinsics.mul_int) === :mul_int
@test nameof(Core.Intrinsics.arraylen) === :arraylen

module TestMod33403
f(x) = 1
f(x::Int) = 2
g() = 3

module Sub
import ..TestMod33403: f
f(x::Char) = 3
end
end

@testset "methods with module" begin
    using .TestMod33403: f, g
    @test length(methods(f)) == 3
    @test length(methods(f, (Int,))) == 1

    @test length(methods(f, TestMod33403)) == 2
    @test length(methods(f, [TestMod33403])) == 2
    @test length(methods(f, (Int,), TestMod33403)) == 1
    @test length(methods(f, (Int,), [TestMod33403])) == 1

    @test length(methods(f, TestMod33403.Sub)) == 1
    @test length(methods(f, [TestMod33403.Sub])) == 1
    @test length(methods(f, (Char,), TestMod33403.Sub)) == 1
    @test length(methods(f, (Int,), TestMod33403.Sub)) == 0

    @test length(methods(g, ())) == 1
end
