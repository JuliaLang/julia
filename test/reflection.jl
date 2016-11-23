# This file is a part of Julia. License is MIT: http://julialang.org/license

# code_native / code_llvm (issue #8239)
# It's hard to really test these, but just running them should be
# sufficient to catch segfault bugs.

module ReflectionTest
using Base.Test

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
    tester(freflect, f, (types.parameters...))
    nothing
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

# Issue #16326
mktemp() do f, io
    OLDSTDOUT = STDOUT
    redirect_stdout(io)
    @test try @code_native map(abs, rand(3)); true; catch false; end
    redirect_stdout(OLDSTDOUT)
    nothing
end

end # module ReflectionTest

# code_warntype
module WarnType
using Base.Test

function warntype_hastag(f, types, tag)
    iob = IOBuffer()
    code_warntype(iob, f, types)
    str = String(take!(iob))
    return !isempty(search(str, tag))
end

pos_stable(x) = x > 0 ? x : zero(x)
pos_unstable(x) = x > 0 ? x : 0

tag = Base.have_color ? Base.error_color() : "UNION"
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

tag = Base.have_color ? Base.error_color() : "ARRAY{FLOAT64,N}"
@test warntype_hastag(getindex, Tuple{Unstable{Float64},Int}, tag)
@test !warntype_hastag(getindex, Tuple{Stable{Float64,2},Int}, tag)
@test warntype_hastag(getindex, Tuple{Stable{Float64},Int}, tag)

# Make sure emphasis is not used for other functions
tag = Base.have_color ? Base.error_color() : "ANY"
iob = IOBuffer()
show(iob, expand(:(x->x^2)))
str = String(take!(iob))
@test isempty(search(str, tag))

module ImportIntrinsics15819
# Make sure changing the lookup path of an intrinsic doesn't break
# the heuristic for type instability warning.
# This can be any intrinsic that needs boxing
import Core.Intrinsics: sqrt_llvm, box
# Use import
sqrt15819(x::Float64) = box(Float64, sqrt_llvm(x))
# Use fully qualified name
sqrt15819(x::Float32) = box(Float32, Core.Intrinsics.sqrt_llvm(x))
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
i10165(::Type) = 0
i10165{T,n}(::Type{AbstractArray{T,n}}) = 1
@test i10165(AbstractArray{Int,n} where n) == 0
@test which(i10165, Tuple{Type{AbstractArray{Int,n} where n},}).sig == Tuple{typeof(i10165),Type}

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

## find bindings tests
@test ccall(:jl_get_module_of_binding, Any, (Any, Any), Base, :sin)==Base

const curmod = current_module()
const curmod_name = fullname(curmod)

module TestMod7648
using Base.Test
import Base.convert
import ..curmod_name, ..curmod
export a9475, foo9475, c7648, foo7648, foo7648_nomethods, Foo7648

const c7648 = 8
d7648 = 9
const f7648 = 10
foo7648(x) = x
function foo7648_nomethods end
type Foo7648 end

module TestModSub9475
    using Base.Test
    using ..TestMod7648
    import ..curmod_name
    export a9475, foo9475
    a9475 = 5
    b9475 = 7
    foo9475(x) = x
    let
        @test Base.binding_module(:a9475) == current_module()
        @test Base.binding_module(:c7648) == TestMod7648
        @test Base.module_name(current_module()) == :TestModSub9475
        @test Base.fullname(current_module()) == (curmod_name..., :TestMod7648,
                                                  :TestModSub9475)
        @test Base.module_parent(current_module()) == TestMod7648
    end
end # module TestModSub9475

using .TestModSub9475

let
    @test Base.binding_module(:d7648) == current_module()
    @test Base.binding_module(:a9475) == TestModSub9475
    @test Base.module_name(current_module()) == :TestMod7648
    @test Base.module_parent(current_module()) == curmod
end
end # module TestMod7648

let
    @test Base.binding_module(TestMod7648, :d7648) == TestMod7648
    @test Base.binding_module(TestMod7648, :a9475) == TestMod7648.TestModSub9475
    @test Base.binding_module(TestMod7648.TestModSub9475, :b9475) == TestMod7648.TestModSub9475
    @test Set(names(TestMod7648))==Set([:TestMod7648, :a9475, :foo9475, :c7648, :foo7648, :foo7648_nomethods, :Foo7648])
    @test Set(names(TestMod7648, true)) == Set([:TestMod7648, :TestModSub9475, :a9475, :foo9475, :c7648, :d7648, :f7648,
                                                :foo7648, Symbol("#foo7648"), :foo7648_nomethods, Symbol("#foo7648_nomethods"),
                                                :Foo7648, :eval, Symbol("#eval")])
    @test Set(names(TestMod7648, true, true)) == Set([:TestMod7648, :TestModSub9475, :a9475, :foo9475, :c7648, :d7648, :f7648,
                                                      :foo7648, Symbol("#foo7648"), :foo7648_nomethods, Symbol("#foo7648_nomethods"),
                                                      :Foo7648, :eval, Symbol("#eval"), :convert, :curmod_name, :curmod])
    @test isconst(TestMod7648, :c7648)
    @test !isconst(TestMod7648, :d7648)
end

let
    using .TestMod7648
    @test Base.binding_module(:a9475) == TestMod7648.TestModSub9475
    @test Base.binding_module(:c7648) == TestMod7648
    @test Base.function_name(foo7648) == :foo7648
    @test Base.function_module(foo7648, (Any,)) == TestMod7648
    @test Base.function_module(foo7648) == TestMod7648
    @test Base.function_module(foo7648_nomethods) == TestMod7648
    @test Base.function_module(foo9475, (Any,)) == TestMod7648.TestModSub9475
    @test Base.function_module(foo9475) == TestMod7648.TestModSub9475
    @test Base.datatype_module(Foo7648) == TestMod7648
    @test Base.datatype_name(Foo7648) == :Foo7648
    @test basename(functionloc(foo7648, (Any,))[1]) == "reflection.jl"
    @test first(methods(TestMod7648.TestModSub9475.foo7648)) == @which foo7648(5)
    @test TestMod7648 == @which foo7648
    @test TestMod7648.TestModSub9475 == @which a9475
end

@test_throws ArgumentError which(===, Tuple{Int, Int})
@test_throws ArgumentError code_typed(===, Tuple{Int, Int})
@test_throws ArgumentError code_llvm(===, Tuple{Int, Int})
@test_throws ArgumentError code_native(===, Tuple{Int, Int})
@test_throws ArgumentError Base.return_types(===, Tuple{Int, Int})

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
                 "foo13825{T,N}(::Array{T,N}, ::Array, ::Array{T,1} where T)")

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
for (f, t) in Any[(definitely_not_in_sysimg, Tuple{}),
                  (Base.:+, Tuple{Int, Int})]
    meth = which(f, t)
    tt = Tuple{typeof(f), t.parameters...}
    env = (ccall(:jl_match_method, Any, (Any, Any), tt, meth.sig))[2]
    world = typemax(UInt)
    linfo = ccall(:jl_specializations_get_linfo, Ref{Core.MethodInstance}, (Any, Any, Any, UInt), meth, tt, env, world)
    params = Base.CodegenParams()
    llvmf = ccall(:jl_get_llvmf_decl, Ptr{Void}, (Any, UInt, Bool, Base.CodegenParams), linfo::Core.MethodInstance, world, true, params)
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
    using .MacroTest
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
    src, rettype = code_typed(f, types)[1]
    dupnames = Set()
    slotnames = Set()
    for name in src.slotnames
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
    for str in (sprint(code_warntype, f, types),
                stringmime("text/plain", src))
        for var in must_used_vars
            @test contains(str, string(var))
        end
        @test !contains(str, "Any")
        @test !contains(str, "ANY")
        # Check that we are not printing the bare slot numbers
        for i in 1:length(src.slotnames)
            name = src.slotnames[i]
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
    # Make sure printing an AST outside CodeInfo still works.
    str = sprint(show, src.code)
    # Check that we are printing the slot numbers when we don't have the context
    # Use the variable names that we know should be present in the optimized AST
    for i in 2:length(src.slotnames)
        name = src.slotnames[i]
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

let li = typeof(getfield).name.mt.cache.func::Core.MethodInstance,
    lrepr = string(li),
    mrepr = string(li.def),
    lmime = stringmime("text/plain", li),
    mmime = stringmime("text/plain", li.def)

    @test lrepr == lmime == "MethodInstance for getfield(...)"
    @test mrepr == mmime == "getfield(...) in Core"
end


# Linfo Tracing test
tracefoo(x, y) = x+y
didtrace = false
tracer(x::Ptr{Void}) = (@test isa(unsafe_pointer_to_objref(x), Core.MethodInstance); global didtrace = true; nothing)
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
@test length(methods(fLargeTable, Tuple{})) == 1
fLargeTable(::Complex, ::Complex) = 4
fLargeTable(::Union{Complex64, Complex128}...) = 5
@test length(methods(fLargeTable, Tuple{})) == 1
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
        Base.isdeprecated(mod, n) && continue
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

# Adds test for PR #17636
let a = @code_typed 1 + 1
    b = @code_lowered 1 + 1
    @test isa(a, Pair{CodeInfo, DataType})
    @test isa(b, CodeInfo)
    @test isa(a[1].code, Array{Any,1})
    @test isa(b.code, Array{Any,1})

    function thing(a::Array, b::Real)
        println("thing")
    end
    function thing(a::AbstractArray, b::Int)
        println("blah")
    end
    @test_throws MethodError thing(rand(10), 1)
    a = @code_typed thing(rand(10), 1)
    b = @code_lowered thing(rand(10), 1)
    @test length(a) == 0
    @test length(b) == 0
end

type A18434
end
(::Type{A18434})(x; y=1) = 1

global counter18434 = 0
function get_A18434()
    global counter18434
    counter18434 += 1
    return A18434
end
@which get_A18434()(1; y=2)
@test counter18434 == 1
@which get_A18434()(1, y=2)
@test counter18434 == 2

# PR #18888: code_typed shouldn't cache if not optimizing
let
    world = typemax(UInt)
    f18888() = return nothing
    m = first(methods(f18888, Tuple{}))
    @test m.specializations === nothing
    ft = typeof(f18888)

    code_typed(f18888, Tuple{}; optimize=false)
    @test m.specializations !== nothing  # uncached, but creates the specializations entry
    code = Core.Inference.code_for_method(m, Tuple{ft}, Core.svec(), world, true)
    @test !isdefined(code, :inferred)

    code_typed(f18888, Tuple{}; optimize=true)
    code = Core.Inference.code_for_method(m, Tuple{ft}, Core.svec(), world, true)
    @test isdefined(code, :inferred)
end

# Issue #18883, code_llvm/code_native for generated functions
@generated f18883() = nothing
@test !isempty(sprint(io->code_llvm(io, f18883, Tuple{})))
@test !isempty(sprint(io->code_native(io, f18883, Tuple{})))
