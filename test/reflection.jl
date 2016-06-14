# This file is a part of Julia. License is MIT: http://julialang.org/license

# code_native / code_llvm (issue #8239)
# It's hard to really test these, but just running them should be
# sufficient to catch segfault bugs.

module ReflectionTest
using Base.Test

function test_ast_reflection(freflect, f, types)
    @test !isempty(freflect(f, types))
end

function test_bin_reflection(freflect, f, types)
    iob = IOBuffer()
    freflect(iob, f, types)
    str = takebuf_string(iob)
    @test !isempty(str)
end

function test_code_reflection(freflect, f, types, tester)
    tester(freflect, f, types)
    tester(freflect, f, (types.parameters...))
end

function test_code_reflections(tester, freflect)
    test_code_reflection(freflect, ismatch,
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
test_code_reflections(test_bin_reflection, code_llvm)
test_code_reflections(test_bin_reflection, code_native)
end

# code_warntype
module WarnType
using Base.Test

function warntype_hastag(f, types, tag)
    iob = IOBuffer()
    code_warntype(iob, f, types)
    str = takebuf_string(iob)
    !isempty(search(str, tag))
end

pos_stable(x) = x > 0 ? x : zero(x)
pos_unstable(x) = x > 0 ? x : 0

tag = Base.have_color ? Base.text_colors[:red] : "UNION"
@test warntype_hastag(pos_unstable, Tuple{Float64}, tag)
@test !warntype_hastag(pos_stable, Tuple{Float64}, tag)

type Stable{T,N}
    A::Array{T,N}
end
type Unstable{T}
    A::Array{T}
end
Base.getindex(A::Stable, i) = A.A[i]
Base.getindex(A::Unstable, i) = A.A[i]

tag = Base.have_color ? Base.text_colors[:red] : "ARRAY{FLOAT64,N}"
@test warntype_hastag(getindex, Tuple{Unstable{Float64},Int}, tag)
@test !warntype_hastag(getindex, Tuple{Stable{Float64,2},Int}, tag)
@test warntype_hastag(getindex, Tuple{Stable{Float64},Int}, tag)

# Make sure emphasis is not used for other functions
tag = Base.have_color ? Base.text_colors[:red] : "ANY"
iob = IOBuffer()
show(iob, expand(:(x->x^2)))
str = takebuf_string(iob)
@test isempty(search(str, tag))

module ImportIntrinsics15819
# Make sure changing the lookup path of an intrinsic doesn't break
# the heuristic for type instability warning.
# This can be any intrinsic that needs boxing
import Core.Intrinsics: sqrt_llvm, box, unbox
# Use import
sqrt15819(x::Float64) = box(Float64, sqrt_llvm(unbox(Float64, x)))
# Use fully qualified name
sqrt15819(x::Float32) = box(Float32, Core.Intrinsics.sqrt_llvm(unbox(Float32, x)))
end
foo11122(x) = @fastmath x - 1.0

# issue #11122, #13568 and #15819
@test !warntype_hastag(+, Tuple{Int,Int}, tag)
@test !warntype_hastag(-, Tuple{Int,Int}, tag)
@test !warntype_hastag(*, Tuple{Int,Int}, tag)
@test !warntype_hastag(/, Tuple{Int,Int}, tag)
@test !warntype_hastag(foo11122, Tuple{Float32}, tag)
@test !warntype_hastag(foo11122, Tuple{Float64}, tag)
@test !warntype_hastag(foo11122, Tuple{Int}, tag)
@test !warntype_hastag(sqrt, Tuple{Int}, tag)
@test !warntype_hastag(sqrt, Tuple{Float64}, tag)
@test !warntype_hastag(^, Tuple{Float64,Int32}, tag)
@test !warntype_hastag(^, Tuple{Float32,Int32}, tag)
@test !warntype_hastag(ImportIntrinsics15819.sqrt15819, Tuple{Float64}, tag)
@test !warntype_hastag(ImportIntrinsics15819.sqrt15819, Tuple{Float32}, tag)

end

# isbits

@test !isbits(Array{Int})
@test isbits(Float32)
@test isbits(Int)
@test !isbits(AbstractString)
@test isbits(Tuple{Int, Vararg{Int, 2}})
@test !isbits(Tuple{Int, Vararg{Int}})
@test !isbits(Tuple{Integer, Vararg{Int, 2}})
@test isbits(Tuple{Int, Vararg{Any, 0}})
@test isbits(Tuple{Vararg{Any, 0}})

# issue #16670
@test isleaftype(Tuple{Int, Vararg{Int, 2}})
@test !isleaftype(Tuple{Integer, Vararg{Int, 2}})
@test !isleaftype(Tuple{Int, Vararg{Int}})
@test isleaftype(Type{Tuple{Integer, Vararg{Int}}})
@test isleaftype(Type{Vector})

# issue #10165
i10165(::DataType) = 0
i10165{T,n}(::Type{AbstractArray{T,n}}) = 1
@test i10165(AbstractArray{Int}) == 0
@test which(i10165, Tuple{Type{AbstractArray{Int}},}).sig == Tuple{typeof(i10165),DataType}

# fullname
@test fullname(Base) == (:Base,)
@test fullname(Base.Pkg) == (:Base, :Pkg)

const a_const = 1
not_const = 1
@test isconst(:a_const) == true
@test isconst(Base, :pi) == true
@test isconst(:pi) == true
@test isconst(:not_const) == false
@test isconst(:is_not_defined) == false

@test isimmutable(1) == true
@test isimmutable([]) == false
@test isimmutable("abc") == true

## find bindings tests
@test ccall(:jl_get_module_of_binding, Any, (Any, Any), Base, :sin)==Base

module TestMod7648
using Base.Test
export a9475, c7648, foo7648

const c7648 = 8
d7648 = 9
const f7648 = 10
foo7648(x) = x

    module TestModSub9475
    using Base.Test
    using ..TestMod7648
    export a9475
    a9475 = 5
    b9475 = 7
    let
        @test Base.binding_module(:a9475)==current_module()
        @test Base.binding_module(:c7648)==TestMod7648
        @test Base.module_name(current_module())==:TestModSub9475
        @test Base.fullname(current_module())==(:TestMod7648, :TestModSub9475)
        @test Base.module_parent(current_module())==TestMod7648
    end
    end # module TestModSub9475

using .TestModSub9475

let
    @test Base.binding_module(:d7648)==current_module()
    @test Base.binding_module(:a9475)==TestModSub9475
    @test Base.module_name(current_module())==:TestMod7648
    @test Base.module_parent(current_module())==Main
end
end # module TestMod7648

let
    @test Base.binding_module(TestMod7648, :d7648)==TestMod7648
    @test Base.binding_module(TestMod7648, :a9475)==TestMod7648.TestModSub9475
    @test Base.binding_module(TestMod7648.TestModSub9475, :b9475)==TestMod7648.TestModSub9475
    @test Set(names(TestMod7648))==Set([:TestMod7648, :a9475, :c7648, :foo7648])
    @test isconst(TestMod7648, :c7648)
    @test !isconst(TestMod7648, :d7648)
end

let
    using TestMod7648
    @test Base.binding_module(:a9475)==TestMod7648.TestModSub9475
    @test Base.binding_module(:c7648)==TestMod7648
    @test Base.function_name(foo7648)==:foo7648
    @test Base.function_module(foo7648, (Any,))==TestMod7648
    @test basename(functionloc(foo7648, (Any,))[1]) == "reflection.jl"
    @test first(methods(TestMod7648.TestModSub9475.foo7648)) == @which foo7648(5)
    @test TestMod7648==@which foo7648
    @test TestMod7648.TestModSub9475==@which a9475
end

@test_throws ArgumentError which(is, Tuple{Int, Int})

module TestingExported
using Base.Test
import Base.isexported
global this_is_not_defined
export this_is_not_defined
@test_throws ErrorException which(:this_is_not_defined)
@test_throws ErrorException @which this_is_not_defined
@test_throws ErrorException which(:this_is_not_exported)
@test isexported(current_module(), :this_is_not_defined)
@test !isexported(current_module(), :this_is_not_exported)
const a_value = 1
@test which(:a_value) == current_module()
@test !isexported(current_module(), :a_value)
end

# issue #13264
@test isa((@which vcat(1...)), Method)

# issue #13464
let t13464 = "hey there sailor"
    try
        @which t13464[1,1] = (1.0,true)
        error("unexpected")
    catch err13464
        @test startswith(err13464.msg, "expression is not a function call, or is too complex")
    end
end

# PR 13825
let ex = :(a + b)
    @test string(ex) == "a + b"
    ex.typ = Integer
    @test string(ex) == "(a + b)::Integer"
end
foo13825{T,N}(::Array{T,N}, ::Array, ::Vector) = nothing
@test startswith(string(first(methods(foo13825))),
                 "foo13825{T,N}(::Array{T,N}, ::Array, ::Array{T<:Any,1})")

type TLayout
    x::Int8
    y::Int16
    z::Int32
end
tlayout = TLayout(5,7,11)
@test fieldnames(tlayout) == fieldnames(TLayout) == [:x, :y, :z]
@test [(fieldoffset(TLayout,i), fieldname(TLayout,i), fieldtype(TLayout,i)) for i = 1:nfields(TLayout)] ==
    [(0, :x, Int8), (2, :y, Int16), (4, :z, Int32)]
@test_throws BoundsError fieldtype(TLayout, 0)
@test_throws BoundsError fieldname(TLayout, 0)
@test_throws BoundsError fieldoffset(TLayout, 0)
@test_throws BoundsError fieldtype(TLayout, 4)
@test_throws BoundsError fieldname(TLayout, 4)
@test_throws BoundsError fieldoffset(TLayout, 4)

@test fieldnames((1,2,3)) == fieldnames(NTuple{3, Int}) == [fieldname(NTuple{3, Int}, i) for i = 1:3] == [1, 2, 3]
@test_throws BoundsError fieldname(NTuple{3, Int}, 0)
@test_throws BoundsError fieldname(NTuple{3, Int}, 4)

import Base: isstructtype, type_alignment, return_types
@test !isstructtype(Union{})
@test !isstructtype(Union{Int,Float64})
@test !isstructtype(Int)
@test isstructtype(TLayout)
@test type_alignment(UInt16) == 2
@test type_alignment(TLayout) == 4
let rts = return_types(TLayout)
    @test length(rts) >= 3 # general constructor, specific constructor, and call-to-convert adapter(s)
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
@test functionloc(f14346)[2] == @__LINE__-4

# test jl_get_llvm_fptr. We test functions both in and definitely not in the system image
definitely_not_in_sysimg() = nothing
for (f,t) in ((definitely_not_in_sysimg,Tuple{}),
        (Base.throw_boundserror,Tuple{UnitRange{Int64},Int64}))
    t = Base.tt_cons(Core.Typeof(f), Base.to_tuple_type(t))
    llvmf = ccall(:jl_get_llvmf, Ptr{Void}, (Any, Bool, Bool), t, false, true)
    @test llvmf != C_NULL
    @test ccall(:jl_get_llvm_fptr, Ptr{Void}, (Ptr{Void},), llvmf) != C_NULL
end

module MacroTest
export @macrotest
macro macrotest(x::Int, y::Symbol) end
macro macrotest(x::Int, y::Int)
    nothing #This is here because of #15280
end
end

let
    using MacroTest
    a = 1
    m = getfield(current_module(), Symbol("@macrotest"))
    @test which(m, Tuple{Int,Symbol})==@which @macrotest 1 a
    @test which(m, Tuple{Int,Int})==@which @macrotest 1 1

    @test first(methods(m,Tuple{Int, Int}))==@which MacroTest.@macrotest 1 1
    @test functionloc(@which @macrotest 1 1) == @functionloc @macrotest 1 1
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
    for index_var15714 in eachindex(array_var15714)
        array_var15714[index_var15714] += 0
    end
end

used_dup_var_tested15714 = false
used_unique_var_tested15714 = false
function test_typed_ast_printing(f::ANY, types::ANY, must_used_vars)
    li = code_typed(f, types)[1]
    dupnames = Set()
    slotnames = Set()
    for name in li.slotnames
        if name in slotnames
            push!(dupnames, name)
        else
            push!(slotnames, name)
        end
    end
    # Make sure must_used_vars are in slotnames
    for name in must_used_vars
        @test name in slotnames
    end
    for str in (sprint(io->code_warntype(io, f, types)),
                sprint(io->show(io, li)))
        # Test to make sure the clearing of file path below works
        # If we don't store the full path in line number node/ast printing
        # anymore, the test and the string replace below should be fixed.
        @test contains(str, @__FILE__)
        str = replace(str, @__FILE__, "")
        for var in must_used_vars
            @test contains(str, string(var))
        end
        @test !contains(str, "Any")
        @test !contains(str, "ANY")
        # Check that we are not printing the bare slot numbers
        for i in 1:length(li.slotnames)
            name = li.slotnames[i]
            if name in dupnames
                @test contains(str, "_$i")
                if name in must_used_vars
                    global used_dup_var_tested15714 = true
                end
            else
                @test !contains(str, "_$i")
                if name in must_used_vars
                    global used_unique_var_tested15714 = true
                end
            end
        end
    end
    # Make sure printing an AST outside LambdaInfo still works.
    str = sprint(io->show(io, Base.uncompressed_ast(li)))
    # Check that we are printing the slot numbers when we don't have the context
    # Use the variable names that we know should be present in the optimized AST
    for i in 2:length(li.slotnames)
        name = li.slotnames[i]
        if name in must_used_vars
            @test contains(str, "_$i")
        end
    end
end
test_typed_ast_printing(f15714, Tuple{Vector{Float32}},
                        [:array_var15714, :index_var15714])
test_typed_ast_printing(g15714, Tuple{Vector{Float32}},
                        [:array_var15714, :index_var15714])
@test used_dup_var_tested15714
@test used_unique_var_tested15714

# Linfo Tracing test
tracefoo(x, y) = x+y
didtrace = false
tracer(x::Ptr{Void}) = (@test isa(unsafe_pointer_to_objref(x), LambdaInfo); global didtrace = true; nothing)
ccall(:jl_register_method_tracer, Void, (Ptr{Void},), cfunction(tracer, Void, (Ptr{Void},)))
meth = which(tracefoo,Tuple{Any,Any})
ccall(:jl_trace_method, Void, (Any,), meth)
@test tracefoo(1, 2) == 3
ccall(:jl_untrace_method, Void, (Any,), meth)
@test didtrace
didtrace = false
@test tracefoo(1.0, 2.0) == 3.0
@test !didtrace
ccall(:jl_register_method_tracer, Void, (Ptr{Void},), C_NULL)

# Method Tracing test
methtracer(x::Ptr{Void}) = (@test isa(unsafe_pointer_to_objref(x), Method); global didtrace = true; nothing)
ccall(:jl_register_newmeth_tracer, Void, (Ptr{Void},), cfunction(methtracer, Void, (Ptr{Void},)))
tracefoo2(x, y) = x*y
@test didtrace
didtrace = false
tracefoo(x::Int64, y::Int64) = x*y
@test didtrace
didtrace = false
ccall(:jl_register_newmeth_tracer, Void, (Ptr{Void},), C_NULL)

# test for reflection over large method tables
for i = 1:100; @eval fLargeTable(::Val{$i}, ::Any) = 1; end
for i = 1:100; @eval fLargeTable(::Any, ::Val{$i}) = 2; end
fLargeTable(::Any...) = 3
fLargeTable(::Complex, ::Complex) = 4
fLargeTable(::Union{Complex64, Complex128}...) = 5
fLargeTable() = 4
@test length(methods(fLargeTable)) == 204
@test length(methods(fLargeTable, Tuple{})) == 1
@test fLargeTable(1im, 2im) == 4
@test fLargeTable(1.0im, 2.0im) == 5
@test_throws MethodError fLargeTable(Val{1}(), Val{1}())
@test fLargeTable(Val{1}(), 1) == 1
@test fLargeTable(1, Val{1}()) == 2

# issue #15280
function f15280(x) end
@test functionloc(f15280)[2] > 0

# bug found in #16850, Base.url with backslashes on Windows
function module_depth(from::Module, to::Module)
    if from === to
        return 0
    else
        return 1 + module_depth(from, module_parent(to))
    end
end
function has_backslashes(mod::Module)
    for n in names(mod, true, true)
        isdefined(mod, n) || continue
        f = getfield(mod, n)
        if isa(f, Module) && module_depth(Main, f) <= module_depth(Main, mod)
            continue
        end
        h = has_backslashes(f)
        isnull(h) || return h
    end
    return Nullable{Method}()
end
function has_backslashes(f::Function)
    for m in methods(f)
        h = has_backslashes(m)
        isnull(h) || return h
    end
    return Nullable{Method}()
end
function has_backslashes(meth::Method)
    if '\\' in string(meth.file)
        return Nullable{Method}(meth)
    else
        return Nullable{Method}()
    end
end
has_backslashes(x) = Nullable{Method}()
h16850 = has_backslashes(Base)
if is_windows()
    if isnull(h16850)
        warn("No methods found in Base with backslashes in file name, ",
             "skipping test for Base.url")
    else
        @test !('\\' in Base.url(get(h16850)))
    end
else
    @test isnull(h16850)
end
