using Test
include("irutils.jl")

# control flow backedge should taint `terminates`
@test Base.infer_effects((Int,)) do n
    for i = 1:n; end
end |> !Core.Compiler.is_terminates

# interprocedural-recursion should taint `terminates` **appropriately**
function sumrecur(a, x)
    isempty(a) && return x
    return sumrecur(Base.tail(a), x + first(a))
end
@test Base.infer_effects(sumrecur, (Tuple{Int,Int,Int},Int)) |> Core.Compiler.is_terminates
@test Base.infer_effects(sumrecur, (Tuple{Int,Int,Int,Vararg{Int}},Int)) |> !Core.Compiler.is_terminates

# https://github.com/JuliaLang/julia/issues/45781
@test Base.infer_effects((Float32,)) do a
    out1 = promote_type(Irrational{:π}, Bool)
    out2 = sin(a)
    out1, out2
end |> Core.Compiler.is_terminates

# refine :consistent-cy effect inference using the return type information
@test Base.infer_effects((Any,)) do x
    taint = Ref{Any}(x) # taints :consistent-cy, but will be adjusted
    throw(taint)
end |> Core.Compiler.is_consistent
@test Base.infer_effects((Int,)) do x
    if x < 0
        taint = Ref(x) # taints :consistent-cy, but will be adjusted
        throw(DomainError(x, taint))
    end
    return nothing
end |> Core.Compiler.is_consistent
@test Base.infer_effects((Int,)) do x
    if x < 0
        taint = Ref(x) # taints :consistent-cy, but will be adjusted
        throw(DomainError(x, taint))
    end
    return x == 0 ? nothing : x # should `Union` of isbitstype objects nicely
end |> Core.Compiler.is_consistent
@test Base.infer_effects((Symbol,Any)) do s, x
    if s === :throw
        taint = Ref{Any}(":throw option given") # taints :consistent-cy, but will be adjusted
        throw(taint)
    end
    return s # should handle `Symbol` nicely
end |> Core.Compiler.is_consistent
@test Base.infer_effects((Int,)) do x
    return Ref(x)
end |> !Core.Compiler.is_consistent
@test Base.infer_effects((Int,)) do x
    return x < 0 ? Ref(x) : nothing
end |> !Core.Compiler.is_consistent
@test Base.infer_effects((Int,)) do x
    if x < 0
        throw(DomainError(x, lazy"$x is negative"))
    end
    return nothing
end |> Core.Compiler.is_foldable

# :the_exception expression should taint :consistent-cy
global inconsistent_var::Int = 42
function throw_inconsistent() # this is still :consistent
    throw(inconsistent_var)
end
function catch_inconsistent()
    try
        throw_inconsistent()
    catch err
        err
    end
end
@test !Core.Compiler.is_consistent(Base.infer_effects(catch_inconsistent))
cache_inconsistent() = catch_inconsistent()
function compare_inconsistent()
    a = cache_inconsistent()
    global inconsistent_var = 0
    b = cache_inconsistent()
    global inconsistent_var = 42
    return a === b
end
@test !compare_inconsistent()
# return type information shouldn't be able to refine it also
function catch_inconsistent(x::T) where T
    v = x
    try
        throw_inconsistent()
    catch err
        v = err::T
    end
    return v
end
@test !Core.Compiler.is_consistent(Base.infer_effects(catch_inconsistent, (Int,)))
cache_inconsistent(x) = catch_inconsistent(x)
function compare_inconsistent(x::T) where T
    x = one(T)
    a = cache_inconsistent(x)
    global inconsistent_var = 0
    b = cache_inconsistent(x)
    global inconsistent_var = 42
    return a === b
end
@test !compare_inconsistent(3)

# allocation/access of uninitialized fields should taint the :consistent-cy
struct Maybe{T}
    x::T
    Maybe{T}() where T = new{T}()
    Maybe{T}(x) where T = new{T}(x)
    Maybe(x::T) where T = new{T}(x)
end
Base.getindex(x::Maybe) = x.x

import Core.Compiler: Const, getfield_notundefined
for T = (Base.RefValue, Maybe) # both mutable and immutable
    for name = (Const(1), Const(:x))
        @test getfield_notundefined(T{String}, name)
        @test getfield_notundefined(T{Integer}, name)
        @test getfield_notundefined(T{Union{String,Integer}}, name)
        @test getfield_notundefined(Union{T{String},T{Integer}}, name)
        @test !getfield_notundefined(T{Int}, name)
        @test !getfield_notundefined(T{<:Integer}, name)
        @test !getfield_notundefined(T{Union{Int32,Int64}}, name)
        @test !getfield_notundefined(T, name)
    end
    # throw doesn't account for undefined behavior
    for name = (Const(0), Const(2), Const(1.0), Const(:y), Const("x"),
                Float64, String, Nothing)
        @test getfield_notundefined(T{String}, name)
        @test getfield_notundefined(T{Int}, name)
        @test getfield_notundefined(T{Integer}, name)
        @test getfield_notundefined(T{<:Integer}, name)
        @test getfield_notundefined(T{Union{Int32,Int64}}, name)
        @test getfield_notundefined(T, name)
    end
    # should not be too conservative when field isn't known very well but object information is accurate
    @test getfield_notundefined(T{String}, Int)
    @test getfield_notundefined(T{String}, Symbol)
    @test getfield_notundefined(T{Integer}, Int)
    @test getfield_notundefined(T{Integer}, Symbol)
    @test !getfield_notundefined(T{Int}, Int)
    @test !getfield_notundefined(T{Int}, Symbol)
    @test !getfield_notundefined(T{<:Integer}, Int)
    @test !getfield_notundefined(T{<:Integer}, Symbol)
end
# should be conservative when object information isn't accurate
@test !getfield_notundefined(Any, Const(1))
@test !getfield_notundefined(Any, Const(:x))
# tuples and namedtuples should be okay if not given accurate information
for TupleType = Any[Tuple{Int,Int,Int}, Tuple{Int,Vararg{Int}}, Tuple{Any}, Tuple,
                    NamedTuple{(:a, :b), Tuple{Int,Int}}, NamedTuple{(:x,),Tuple{Any}}, NamedTuple],
    FieldType = Any[Int, Symbol, Any]
    @test getfield_notundefined(TupleType, FieldType)
end

# TODO add equivalent test cases for `Ref` once we handle mutability more nicely
@test Base.infer_effects() do
    Maybe{Int}()
end |> !Core.Compiler.is_consistent
@test Base.infer_effects() do
    Maybe{Int}()[]
end |> !Core.Compiler.is_consistent
@test !fully_eliminated() do
    Maybe{Int}()[]
end
@test Base.infer_effects() do
    Maybe{String}()
end |> Core.Compiler.is_consistent
@test Base.infer_effects() do
    Maybe{String}()[]
end |> Core.Compiler.is_consistent
@test Base.return_types() do
    Maybe{String}()[] # this expression should be concrete evaluated
end |> only === Union{}

# effects propagation for `Core.invoke` calls
# https://github.com/JuliaLang/julia/issues/44763
global x44763::Int = 0
increase_x44763!(n) = (global x44763; x44763 += n)
invoke44763(x) = @invoke increase_x44763!(x)
@test Base.return_types() do
    invoke44763(42)
end |> only === Int
@test x44763 == 0

# Test that purity doesn't try to accidentally run unreachable code due to
# boundscheck elimination
function f_boundscheck_elim(n)
    # Inbounds here assumes that this is only ever called with n==0, but of
    # course the compiler has no way of knowing that, so it must not attempt
    # to run the @inbounds `getfield(sin, 1)`` that ntuple generates.
    ntuple(x->(@inbounds getfield(sin, x)), n)
end
@test Tuple{} <: code_typed(f_boundscheck_elim, Tuple{Int})[1][2]

# Test that purity modeling doesn't accidentally introduce new world age issues
f_redefine_me(x) = x+1
f_call_redefine() = f_redefine_me(0)
f_mk_opaque() = Base.Experimental.@opaque ()->Base.inferencebarrier(f_call_redefine)()
const op_capture_world = f_mk_opaque()
f_redefine_me(x) = x+2
@test op_capture_world() == 1
@test f_mk_opaque()() == 2

# backedge insertion for Any-typed, effect-free frame
const CONST_DICT = let d = Dict()
    for c in 'A':'z'
        push!(d, c => Int(c))
    end
    d
end
Base.@assume_effects :foldable getcharid(c) = CONST_DICT[c]
@noinline callf(f, args...) = f(args...)
function entry_to_be_invalidated(c)
    return callf(getcharid, c)
end
@test Base.infer_effects((Char,)) do x
    entry_to_be_invalidated(x)
end |> Core.Compiler.is_foldable
@test fully_eliminated(; retval=97) do
    entry_to_be_invalidated('a')
end
getcharid(c) = CONST_DICT[c] # now this is not eligible for concrete evaluation
@test Base.infer_effects((Char,)) do x
    entry_to_be_invalidated(x)
end |> !Core.Compiler.is_foldable
@test !fully_eliminated() do
    entry_to_be_invalidated('a')
end

@test !Core.Compiler.builtin_nothrow(Core.get_binding_type, Any[Rational{Int}, Core.Const(:foo)], Any)

# Nothrow for assignment to globals
global glob_assign_int::Int = 0
f_glob_assign_int() = global glob_assign_int += 1
let effects = Base.infer_effects(f_glob_assign_int, ())
    @test !Core.Compiler.is_effect_free(effects)
    @test Core.Compiler.is_nothrow(effects)
end
# Nothrow for setglobal!
global SETGLOBAL!_NOTHROW::Int = 0
let effects = Base.infer_effects() do
        setglobal!(@__MODULE__, :SETGLOBAL!_NOTHROW, 42)
    end
    @test Core.Compiler.is_nothrow(effects)
end

# we should taint `nothrow` if the binding doesn't exist and isn't fixed yet,
# as the cached effects can be easily wrong otherwise
# since the inference curently doesn't track "world-age" of global variables
@eval global_assignment_undefinedyet() = $(GlobalRef(@__MODULE__, :UNDEFINEDYET)) = 42
setglobal!_nothrow_undefinedyet() = setglobal!(@__MODULE__, :UNDEFINEDYET, 42)
let effects = Base.infer_effects() do
        global_assignment_undefinedyet()
    end
    @test !Core.Compiler.is_nothrow(effects)
end
let effects = Base.infer_effects() do
        setglobal!_nothrow_undefinedyet()
    end
    @test !Core.Compiler.is_nothrow(effects)
end
global UNDEFINEDYET::String = "0"
let effects = Base.infer_effects() do
        global_assignment_undefinedyet()
    end
    @test !Core.Compiler.is_nothrow(effects)
end
let effects = Base.infer_effects() do
        setglobal!_nothrow_undefinedyet()
    end
    @test !Core.Compiler.is_nothrow(effects)
end
@test_throws ErrorException setglobal!_nothrow_undefinedyet()

# Nothrow for setfield!
mutable struct SetfieldNothrow
    x::Int
end
f_setfield_nothrow() = SetfieldNothrow(0).x = 1
let effects = Base.infer_effects(f_setfield_nothrow, ())
    # Technically effect free even though we use the heap, since the
    # object doesn't escape, but the compiler doesn't know that.
    #@test Core.Compiler.is_effect_free(effects)
    @test Core.Compiler.is_nothrow(effects)
end

# nothrow for arrayset
@test Base.infer_effects((Vector{Int},Int)) do a, i
    a[i] = 0 # may throw
end |> !Core.Compiler.is_nothrow

# even if 2-arg `getfield` may throw, it should be still `:consistent`
@test Core.Compiler.is_consistent(Base.infer_effects(getfield, (NTuple{5, Float64}, Int)))

# SimpleVector allocation is consistent
@test Core.Compiler.is_consistent(Base.infer_effects(Core.svec))
@test Base.infer_effects() do
    Core.svec(nothing, 1, "foo")
end |> Core.Compiler.is_consistent

# fastmath operations are inconsistent
@test !Core.Compiler.is_consistent(Base.infer_effects((a,b)->@fastmath(a+b), (Float64,Float64)))

# issue 46122: @assume_effects for @ccall
@test Base.infer_effects((Vector{Int},)) do a
    Base.@assume_effects :effect_free @ccall jl_array_ptr(a::Any)::Ptr{Int}
end |> Core.Compiler.is_effect_free

# `getfield_effects` handles access to union object nicely
@test Core.Compiler.is_consistent(Core.Compiler.getfield_effects(Any[Some{String}, Core.Const(:value)], String))
@test Core.Compiler.is_consistent(Core.Compiler.getfield_effects(Any[Some{Symbol}, Core.Const(:value)], Symbol))
@test Core.Compiler.is_consistent(Core.Compiler.getfield_effects(Any[Union{Some{Symbol},Some{String}}, Core.Const(:value)], Union{Symbol,String}))
@test Base.infer_effects((Bool,)) do c
    obj = c ? Some{String}("foo") : Some{Symbol}(:bar)
    return getfield(obj, :value)
end |> Core.Compiler.is_consistent

@test Core.Compiler.is_consistent(Base.infer_effects(setindex!, (Base.RefValue{Int}, Int)))

# :inaccessiblememonly effect
const global constant_global::Int = 42
const global ConstantType = Ref
global nonconstant_global::Int = 42
const global constant_mutable_global = Ref(0)
const global constant_global_nonisbits = Some(:foo)
@test Base.infer_effects() do
    constant_global
end |> Core.Compiler.is_inaccessiblememonly
@test Base.infer_effects() do
    ConstantType
end |> Core.Compiler.is_inaccessiblememonly
@test Base.infer_effects() do
    ConstantType{Any}()
end |> Core.Compiler.is_inaccessiblememonly
@test_broken Base.infer_effects() do
    constant_global_nonisbits
end |> Core.Compiler.is_inaccessiblememonly
@test Base.infer_effects() do
    getglobal(@__MODULE__, :constant_global)
end |> Core.Compiler.is_inaccessiblememonly
@test Base.infer_effects() do
    nonconstant_global
end |> !Core.Compiler.is_inaccessiblememonly
@test Base.infer_effects() do
    getglobal(@__MODULE__, :nonconstant_global)
end |> !Core.Compiler.is_inaccessiblememonly
@test Base.infer_effects((Symbol,)) do name
    getglobal(@__MODULE__, name)
end |> !Core.Compiler.is_inaccessiblememonly
@test Base.infer_effects((Int,)) do v
    global nonconstant_global = v
end |> !Core.Compiler.is_inaccessiblememonly
@test Base.infer_effects((Int,)) do v
    setglobal!(@__MODULE__, :nonconstant_global, v)
end |> !Core.Compiler.is_inaccessiblememonly
@test Base.infer_effects((Int,)) do v
    constant_mutable_global[] = v
end |> !Core.Compiler.is_inaccessiblememonly
module ConsistentModule
const global constant_global::Int = 42
const global ConstantType = Ref
end # module
@test Base.infer_effects() do
    ConsistentModule.constant_global
end |> Core.Compiler.is_inaccessiblememonly
@test Base.infer_effects() do
    ConsistentModule.ConstantType
end |> Core.Compiler.is_inaccessiblememonly
@test Base.infer_effects() do
    ConsistentModule.ConstantType{Any}()
end |> Core.Compiler.is_inaccessiblememonly
@test Base.infer_effects() do
    getglobal(@__MODULE__, :ConsistentModule).constant_global
end |> Core.Compiler.is_inaccessiblememonly
@test Base.infer_effects() do
    getglobal(@__MODULE__, :ConsistentModule).ConstantType
end |> Core.Compiler.is_inaccessiblememonly
@test Base.infer_effects() do
    getglobal(@__MODULE__, :ConsistentModule).ConstantType{Any}()
end |> Core.Compiler.is_inaccessiblememonly
@test Base.infer_effects((Module,)) do M
    M.constant_global
end |> !Core.Compiler.is_inaccessiblememonly
@test Base.infer_effects((Module,)) do M
    M.ConstantType
end |> !Core.Compiler.is_inaccessiblememonly
@test Base.infer_effects() do M
    M.ConstantType{Any}()
end |> !Core.Compiler.is_inaccessiblememonly

# the `:inaccessiblememonly` helper effect allows us to prove `:consistent`-cy of frames
# including `getfield` accessing to local mutable object

mutable struct SafeRef{T}
    x::T
end
Base.getindex(x::SafeRef) = x.x;
Base.setindex!(x::SafeRef, v) = x.x = v;
Base.isassigned(x::SafeRef) = true;

function mutable_consistent(s)
    SafeRef(s)[]
end
@test Core.Compiler.is_inaccessiblememonly(Base.infer_effects(mutable_consistent, (Symbol,)))
@test fully_eliminated(; retval=QuoteNode(:foo)) do
    mutable_consistent(:foo)
end

function nested_mutable_consistent(s)
    SafeRef(SafeRef(SafeRef(SafeRef(SafeRef(s)))))[][][][][]
end
@test Core.Compiler.is_inaccessiblememonly(Base.infer_effects(nested_mutable_consistent, (Symbol,)))
@test fully_eliminated(; retval=QuoteNode(:foo)) do
    nested_mutable_consistent(:foo)
end

const consistent_global = Some(:foo)
@test Base.infer_effects() do
    consistent_global.value
end |> Core.Compiler.is_consistent

const inconsistent_global = SafeRef(:foo)
@test Base.infer_effects() do
    inconsistent_global[]
end |> !Core.Compiler.is_consistent

global inconsistent_condition_ref = Ref{Bool}(false)
@test Base.infer_effects() do
    if inconsistent_condition_ref[]
        return 0
    else
        return 1
    end
end |> !Core.Compiler.is_consistent

# the `:inaccessiblememonly` helper effect allows us to prove `:effect_free`-ness of frames
# including `setfield!` modifying local mutable object

const global_ref = Ref{Any}()
global const global_bit::Int = 42
makeref() = Ref{Any}()
setref!(ref, @nospecialize v) = ref[] = v

@noinline function removable_if_unused1()
    x = makeref()
    setref!(x, 42)
    x
end
@noinline function removable_if_unused2()
    x = makeref()
    setref!(x, global_bit)
    x
end
for f = Any[removable_if_unused1, removable_if_unused2]
    effects = Base.infer_effects(f)
    @test Core.Compiler.is_inaccessiblememonly(effects)
    @test Core.Compiler.is_effect_free(effects)
    @test Core.Compiler.is_removable_if_unused(effects)
    @test @eval fully_eliminated() do
        $f()
        nothing
    end
end
@noinline function removable_if_unused3(v)
    x = makeref()
    setref!(x, v)
    x
end
let effects = Base.infer_effects(removable_if_unused3, (Int,))
    @test Core.Compiler.is_inaccessiblememonly(effects)
    @test Core.Compiler.is_effect_free(effects)
    @test Core.Compiler.is_removable_if_unused(effects)
end
@test fully_eliminated((Int,)) do v
    removable_if_unused3(v)
    nothing
end

@noinline function unremovable_if_unused1!(x)
    setref!(x, 42)
end
@test !Core.Compiler.is_removable_if_unused(Base.infer_effects(unremovable_if_unused1!, (typeof(global_ref),)))
@test !Core.Compiler.is_removable_if_unused(Base.infer_effects(unremovable_if_unused1!, (Any,)))

@noinline function unremovable_if_unused2!()
    setref!(global_ref, 42)
end
@test !Core.Compiler.is_removable_if_unused(Base.infer_effects(unremovable_if_unused2!))

@noinline function unremovable_if_unused3!()
    getfield(@__MODULE__, :global_ref)[] = nothing
end
@test !Core.Compiler.is_removable_if_unused(Base.infer_effects(unremovable_if_unused3!))

@testset "effects analysis on array ops" begin

@testset "effects analysis on array construction" begin

@noinline construct_array(@nospecialize(T), args...) = Array{T}(undef, args...)

# should eliminate safe but dead allocations
let good_dims = @static Int === Int64 ? (1:10) : (1:8)
    Ns = @static Int === Int64 ? (1:10) : (1:8)
    for dim = good_dims, N = Ns
        dims = ntuple(i->dim, N)
        @test @eval Base.infer_effects() do
            $construct_array(Int, $(dims...))
        end |> Core.Compiler.is_removable_if_unused
        @test @eval fully_eliminated() do
            $construct_array(Int, $(dims...))
            nothing
        end
    end
end

# should analyze throwness correctly
let bad_dims = [-1, typemax(Int)]
    for dim in bad_dims, N in 1:10
        dims = ntuple(i->dim, N)
        @test @eval Base.infer_effects() do
            $construct_array(Int, $(dims...))
        end |> !Core.Compiler.is_removable_if_unused
        @test @eval !fully_eliminated() do
            $construct_array(Int, $(dims...))
            nothing
        end
        @test_throws "invalid Array" @eval $construct_array(Int, $(dims...))
    end
end

end # @testset "effects analysis on array construction" begin

end # @testset "effects analysis on array ops" begin
