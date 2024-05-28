using Test
include("irutils.jl")

# Test that the Core._apply_iterate bail path taints effects
function f_apply_bail(f)
    f(()...)
    return nothing
end
@test !Core.Compiler.is_removable_if_unused(Base.infer_effects(f_apply_bail))
@test !fully_eliminated((Function,)) do f
    f_apply_bail(f)
    nothing
end

# Test that effect modeling for return_type doesn't incorrectly pick
# up the effects of the function being analyzed
f_throws() = error()
@noinline function return_type_unused(x)
    Core.Compiler.return_type(f_throws, Tuple{})
    return x+1
end
@test Core.Compiler.is_removable_if_unused(Base.infer_effects(return_type_unused, (Int,)))
@test fully_eliminated((Int,)) do x
    return_type_unused(x)
    return nothing
end

# Test that ambiguous calls don't accidentally get nothrow effect
ambig_effects_test(a::Int, b) = 1
ambig_effects_test(a, b::Int) = 1
ambig_effects_test(a, b) = 1
@test !Core.Compiler.is_nothrow(Base.infer_effects(ambig_effects_test, (Int, Any)))
global ambig_unknown_type_global::Any = 1
@noinline function conditionally_call_ambig(b::Bool, a)
    if b
        ambig_effects_test(a, ambig_unknown_type_global)
    end
    return 0
end
@test !fully_eliminated((Bool,)) do b
    conditionally_call_ambig(b, 1)
    return nothing
end

# Test that a missing methtable identification gets tainted
# appropriately
struct FCallback; f::Union{Nothing, Function}; end
f_invoke_callback(fc) = let f=fc.f; (f !== nothing && f(); nothing); end
@test !Core.Compiler.is_removable_if_unused(Base.infer_effects(f_invoke_callback, (FCallback,)))
@test !fully_eliminated((FCallback,)) do fc
    f_invoke_callback(fc)
    return nothing
end

# @assume_effects override
const ___CONST_DICT___ = Dict{Any,Any}(Symbol(c) => i for (i, c) in enumerate('a':'z'))
Base.@assume_effects :foldable concrete_eval(
    f, args...; kwargs...) = f(args...; kwargs...)
@test fully_eliminated() do
    concrete_eval(getindex, ___CONST_DICT___, :a)
end

# :removable override
Base.@assume_effects :removable removable_call(
    f, args...; kwargs...) = f(args...; kwargs...)
@test fully_eliminated() do
    @noinline removable_call(getindex, ___CONST_DICT___, :a)
    nothing
end

# terminates_globally override
# https://github.com/JuliaLang/julia/issues/41694
Base.@assume_effects :terminates_globally function issue41694(x)
    res = 1
    0 ≤ x < 20 || error("bad fact")
    while x > 1
        res *= x
        x -= 1
    end
    return res
end
@test Core.Compiler.is_foldable(Base.infer_effects(issue41694, (Int,)))
@test fully_eliminated() do
    issue41694(2)
end

Base.@assume_effects :terminates_globally function recur_termination1(x)
    x == 0 && return 1
    0 ≤ x < 20 || error("bad fact")
    return x * recur_termination1(x-1)
end
@test_broken Core.Compiler.is_foldable(Base.infer_effects(recur_termination1, (Int,)))
@test Core.Compiler.is_terminates(Base.infer_effects(recur_termination1, (Int,)))
function recur_termination2()
    Base.@assume_effects :total !:terminates_globally
    recur_termination1(12)
end
@test_broken fully_eliminated(recur_termination2)
@test fully_eliminated() do; recur_termination2(); end

Base.@assume_effects :terminates_globally function recur_termination21(x)
    x == 0 && return 1
    0 ≤ x < 20 || error("bad fact")
    return recur_termination22(x)
end
recur_termination22(x) = x * recur_termination21(x-1)
@test_broken Core.Compiler.is_foldable(Base.infer_effects(recur_termination21, (Int,)))
@test_broken Core.Compiler.is_foldable(Base.infer_effects(recur_termination22, (Int,)))
@test Core.Compiler.is_terminates(Base.infer_effects(recur_termination21, (Int,)))
@test Core.Compiler.is_terminates(Base.infer_effects(recur_termination22, (Int,)))
function recur_termination2x()
    Base.@assume_effects :total !:terminates_globally
    recur_termination21(12) + recur_termination22(12)
end
@test_broken fully_eliminated(recur_termination2x)
@test fully_eliminated() do; recur_termination2x(); end

# anonymous function support for `@assume_effects`
@test fully_eliminated() do
    map((2,3,4)) do x
        # this :terminates_locally allows this anonymous function to be constant-folded
        Base.@assume_effects :terminates_locally
        res = 1
        0 ≤ x < 20 || error("bad fact")
        while x > 1
            res *= x
            x -= 1
        end
        return res
    end
end

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

# Effect modeling for Core.compilerbarrier
@test Base.infer_effects(Base.inferencebarrier, Tuple{Any}) |> Core.Compiler.is_removable_if_unused

# allocation/access of uninitialized fields should taint the :consistent-cy
struct Maybe{T}
    x::T
    Maybe{T}() where T = new{T}()
    Maybe{T}(x) where T = new{T}(x)
    Maybe(x::T) where T = new{T}(x)
end
Base.getindex(x::Maybe) = x.x

struct SyntacticallyDefined{T}
    x::T
end

import Core.Compiler: Const, getfield_notuninit
for T = (Base.RefValue, Maybe) # both mutable and immutable
    for name = (Const(1), Const(:x))
        @test getfield_notuninit(T{String}, name)
        @test getfield_notuninit(T{Integer}, name)
        @test getfield_notuninit(T{Union{String,Integer}}, name)
        @test getfield_notuninit(Union{T{String},T{Integer}}, name)
        @test !getfield_notuninit(T{Int}, name)
        @test !getfield_notuninit(T{<:Integer}, name)
        @test !getfield_notuninit(T{Union{Int32,Int64}}, name)
        @test !getfield_notuninit(T, name)
    end
    # throw doesn't account for undefined behavior
    for name = (Const(0), Const(2), Const(1.0), Const(:y), Const("x"),
                Float64, String, Nothing)
        @test getfield_notuninit(T{String}, name)
        @test getfield_notuninit(T{Int}, name)
        @test getfield_notuninit(T{Integer}, name)
        @test getfield_notuninit(T{<:Integer}, name)
        @test getfield_notuninit(T{Union{Int32,Int64}}, name)
        @test getfield_notuninit(T, name)
    end
    # should not be too conservative when field isn't known very well but object information is accurate
    @test getfield_notuninit(T{String}, Int)
    @test getfield_notuninit(T{String}, Symbol)
    @test getfield_notuninit(T{Integer}, Int)
    @test getfield_notuninit(T{Integer}, Symbol)
    @test !getfield_notuninit(T{Int}, Int)
    @test !getfield_notuninit(T{Int}, Symbol)
    @test !getfield_notuninit(T{<:Integer}, Int)
    @test !getfield_notuninit(T{<:Integer}, Symbol)
end
# should be conservative when object information isn't accurate
@test !getfield_notuninit(Any, Const(1))
@test !getfield_notuninit(Any, Const(:x))
# tuples and namedtuples should be okay if not given accurate information
for TupleType = Any[Tuple{Int,Int,Int}, Tuple{Int,Vararg{Int}}, Tuple{Any}, Tuple,
                    NamedTuple{(:a, :b), Tuple{Int,Int}}, NamedTuple{(:x,),Tuple{Any}}, NamedTuple],
    FieldType = Any[Int, Symbol, Any]
    @test getfield_notuninit(TupleType, FieldType)
end
# skip analysis on fields that are known to be defined syntactically
@test Core.Compiler.getfield_notuninit(SyntacticallyDefined{Float64}, Symbol)
@test Core.Compiler.getfield_notuninit(Const(Main), Const(:var))
@test Core.Compiler.getfield_notuninit(Const(Main), Const(42))
# high-level tests for `getfield_notuninit`
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
let f() = Maybe{String}()[]
    @test Base.return_types() do
        f() # this call should be concrete evaluated
    end |> only === Union{}
end
@test Base.infer_effects() do
    Ref{Int}()
end |> !Core.Compiler.is_consistent
@test Base.infer_effects() do
    Ref{Int}()[]
end |> !Core.Compiler.is_consistent
@test !fully_eliminated() do
    Ref{Int}()[]
end
@test Base.infer_effects() do
    Ref{String}()[]
end |> Core.Compiler.is_consistent
let f() = Ref{String}()[]
    @test Base.return_types() do
        f() # this call should be concrete evaluated
    end |> only === Union{}
end
@test Base.infer_effects((SyntacticallyDefined{Float64}, Symbol)) do w, s
    getfield(w, s)
end |> Core.Compiler.is_foldable

# effects propagation for `Core.invoke` calls
# https://github.com/JuliaLang/julia/issues/44763
global x44763::Int = 0
increase_x44763!(n) = (global x44763; x44763 += n)
invoke44763(x) = @invoke increase_x44763!(x)
@test Base.return_types() do
    invoke44763(42)
end |> only === Int
@test x44763 == 0

# `@inbounds`/`@boundscheck` expression should taint :consistent correctly
# https://github.com/JuliaLang/julia/issues/48099
function A1_inbounds()
    r = 0
    @inbounds begin
        @boundscheck r += 1
    end
    return r
end
@test !Core.Compiler.is_consistent(Base.infer_effects(A1_inbounds))

# Test that purity doesn't try to accidentally run unreachable code due to
# boundscheck elimination
function f_boundscheck_elim(n)
    # Inbounds here assumes that this is only ever called with `n==0`, but of
    # course the compiler has no way of knowing that, so it must not attempt
    # to run the `@inbounds getfield(sin, 1)` that `ntuple` generates.
    ntuple(x->(@inbounds ()[x]), n)
end
@test !Core.Compiler.is_noub(Base.infer_effects(f_boundscheck_elim, (Int,)))
@test Tuple{} <: only(Base.return_types(f_boundscheck_elim, (Int,)))

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

@test !Core.Compiler.builtin_nothrow(Core.Compiler.fallback_lattice, Core.get_binding_type, Any[Rational{Int}, Core.Const(:foo)], Any)

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
# since the inference currently doesn't track "world-age" of global variables
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
@test_throws Union{ErrorException,TypeError} setglobal!_nothrow_undefinedyet() # TODO: what kind of error should this be?

# Nothrow for setfield!
mutable struct SetfieldNothrow
    x::Int
end
f_setfield_nothrow() = SetfieldNothrow(0).x = 1
let effects = Base.infer_effects(f_setfield_nothrow, ())
    @test Core.Compiler.is_nothrow(effects)
    @test Core.Compiler.is_effect_free(effects) # see EFFECT_FREE_IF_INACCESSIBLEMEMONLY
end

# even if 2-arg `getfield` may throw, it should be still `:consistent`
@test Core.Compiler.is_consistent(Base.infer_effects(getfield, (NTuple{5, Float64}, Int)))

# SimpleVector allocation is consistent
@test Core.Compiler.is_consistent(Base.infer_effects(Core.svec))
@test Base.infer_effects() do
    Core.svec(nothing, 1, "foo")
end |> Core.Compiler.is_consistent

# fastmath operations are in-`:consistent`
@test !Core.Compiler.is_consistent(Base.infer_effects((a,b)->@fastmath(a+b), (Float64,Float64)))

# issue 46122: @assume_effects for @ccall
@test Base.infer_effects((Vector{Int},)) do a
    Base.@assume_effects :effect_free @ccall this_call_does_not_really_exist(a::Any)::Ptr{Int}
end |> Core.Compiler.is_effect_free

# `getfield_effects` handles access to union object nicely
let 𝕃 = Core.Compiler.fallback_lattice
    getfield_effects = Core.Compiler.getfield_effects
    @test Core.Compiler.is_consistent(getfield_effects(𝕃, Any[Some{String}, Core.Const(:value)], String))
    @test Core.Compiler.is_consistent(getfield_effects(𝕃, Any[Some{Symbol}, Core.Const(:value)], Symbol))
    @test Core.Compiler.is_consistent(getfield_effects(𝕃, Any[Union{Some{Symbol},Some{String}}, Core.Const(:value)], Union{Symbol,String}))
end
@test Base.infer_effects((Bool,)) do c
    obj = c ? Some{String}("foo") : Some{Symbol}(:bar)
    return getfield(obj, :value)
end |> Core.Compiler.is_consistent

# getfield is nothrow when bounds checking is turned off
@test Base.infer_effects((Tuple{Int,Int},Int)) do t, i
    getfield(t, i, false)
end |> Core.Compiler.is_nothrow
@test Base.infer_effects((Tuple{Int,Int},Symbol)) do t, i
    getfield(t, i, false)
end |> Core.Compiler.is_nothrow
@test Base.infer_effects((Tuple{Int,Int},String)) do t, i
    getfield(t, i, false) # invalid name type
end |> !Core.Compiler.is_nothrow

@test Base.infer_effects((Some{Any},)) do some
    getfield(some, 1, :not_atomic)
end |> Core.Compiler.is_nothrow
@test Base.infer_effects((Some{Any},)) do some
    getfield(some, 1, :invalid_atomic_spec)
end |> !Core.Compiler.is_nothrow
@test Base.infer_effects((Some{Any},Bool)) do some, boundscheck
    getfield(some, 1, boundscheck)
end |> Core.Compiler.is_nothrow
@test Base.infer_effects((Some{Any},Bool)) do some, boundscheck
    getfield(some, 1, :not_atomic, boundscheck)
end |> Core.Compiler.is_nothrow
@test Base.infer_effects((Some{Any},Bool)) do some, boundscheck
    getfield(some, 1, :invalid_atomic_spec, boundscheck)
end |> !Core.Compiler.is_nothrow
@test Base.infer_effects((Some{Any},Any)) do some, boundscheck
    getfield(some, 1, :not_atomic, boundscheck)
end |> !Core.Compiler.is_nothrow

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
@test Base.infer_effects() do
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
# including `getfield` / `isdefined` accessing to local mutable object

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
@test fully_eliminated(; retval=:foo) do
    mutable_consistent(:foo)
end

function nested_mutable_consistent(s)
    SafeRef(SafeRef(SafeRef(SafeRef(SafeRef(s)))))[][][][][]
end
@test Core.Compiler.is_inaccessiblememonly(Base.infer_effects(nested_mutable_consistent, (Symbol,)))
@test fully_eliminated(; retval=:foo) do
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
const inconsistent_condition_ref = Ref{Bool}(false)
@test Base.infer_effects() do
    if inconsistent_condition_ref[]
        return 0
    else
        return 1
    end
end |> !Core.Compiler.is_consistent

# should handle va-method properly
callgetfield1(xs...) = getfield(getfield(xs, 1), 1)
@test !Core.Compiler.is_inaccessiblememonly(Base.infer_effects(callgetfield1, (Base.RefValue{Symbol},)))
const GLOBAL_XS = Ref(:julia)
global_getfield() = callgetfield1(GLOBAL_XS)
@test let
    Base.Experimental.@force_compile
    global_getfield()
end === :julia
GLOBAL_XS[] = :julia2
@test let
    Base.Experimental.@force_compile
    global_getfield()
end === :julia2

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

# array ops
# =========

# allocation
# ----------

# low-level constructor
@noinline construct_array(@nospecialize(T), args...) = Array{T}(undef, args...)
# should eliminate safe but dead allocations
let good_dims = [1, 2, 3, 4, 10]
    Ns = [1, 2, 3, 4, 10]
    for dim = good_dims, N = Ns
        Int64(dim)^N > typemax(Int) && continue
        dims = ntuple(i->dim, N)
        @test @eval Base.infer_effects() do
            construct_array(Int, $(dims...))
        end |> Core.Compiler.is_removable_if_unused
        @test @eval fully_eliminated() do
            construct_array(Int, $(dims...))
            nothing
        end
    end
end
# should analyze throwness correctly
let bad_dims = [-1, typemax(Int)]
    for dim in bad_dims, N in [1, 2, 3, 4, 10]
        for T in Any[Int, Union{Missing,Nothing}, Missing, Any]
            dims = ntuple(i->dim, N)
            @test @eval Base.infer_effects() do
                construct_array($T, $(dims...))
            end |> !Core.Compiler.is_removable_if_unused
            @test @eval !fully_eliminated() do
                construct_array($T, $(dims...))
                nothing
            end
            @test_throws "invalid " @eval construct_array($T, $(dims...))
        end
    end
end

# high-level interfaces
# getindex
for safesig = Any[
        (Type{Int},)
        (Type{Int}, Int)
        (Type{Int}, Int, Int)
        (Type{Number},)
        (Type{Number}, Number)
        (Type{Number}, Int)
        (Type{Any},)
        (Type{Any}, Any,)
        (Type{Any}, Any, Any)
    ]
    let effects = Base.infer_effects(getindex, safesig)
        @test Core.Compiler.is_consistent_if_notreturned(effects)
        @test Core.Compiler.is_removable_if_unused(effects)
    end
end
for unsafesig = Any[
        (Type{Int}, String)
        (Type{Int}, Any)
        (Type{Number}, AbstractString)
        (Type{Number}, Any)
    ]
    let effects = Base.infer_effects(getindex, unsafesig)
        @test !Core.Compiler.is_nothrow(effects)
    end
end
# vect
for safesig = Any[
        ()
        (Int,)
        (Int, Int)
    ]
    let effects = Base.infer_effects(Base.vect, safesig)
        @test Core.Compiler.is_consistent_if_notreturned(effects)
        @test Core.Compiler.is_removable_if_unused(effects)
    end
end

# array getindex
let tt = (MemoryRef{Any},Symbol,Bool)
    @testset let effects = Base.infer_effects(Core.memoryrefget, tt)
        @test Core.Compiler.is_consistent_if_inaccessiblememonly(effects)
        @test Core.Compiler.is_effect_free(effects)
        @test !Core.Compiler.is_nothrow(effects)
        @test Core.Compiler.is_terminates(effects)
    end
end

# array setindex!
let tt = (MemoryRef{Any},Any,Symbol,Bool)
    @testset let effects = Base.infer_effects(Core.memoryrefset!, tt)
        @test Core.Compiler.is_consistent_if_inaccessiblememonly(effects)
        @test Core.Compiler.is_effect_free_if_inaccessiblememonly(effects)
        @test !Core.Compiler.is_nothrow(effects)
        @test Core.Compiler.is_terminates(effects)
    end
end
# nothrow for arrayset
@test Base.infer_effects((MemoryRef{Int},Int)) do a, v
    Core.memoryrefset!(a, v, :not_atomic, true)
end |> !Core.Compiler.is_nothrow
@test Base.infer_effects((MemoryRef{Int},Int)) do a, v
    a[] = v # may throw
end |> !Core.Compiler.is_nothrow
# when bounds checking is turned off, it should be safe
@test Base.infer_effects((MemoryRef{Int},Int)) do a, v
    Core.memoryrefset!(a, v, :not_atomic, false)
end |> Core.Compiler.is_nothrow
@test Base.infer_effects((MemoryRef{Number},Number)) do a, v
    Core.memoryrefset!(a, v, :not_atomic, false)
end |> Core.Compiler.is_nothrow

# arraysize
# ---------

let effects = Base.infer_effects(size, (Array,Int))
    @test Core.Compiler.is_consistent_if_inaccessiblememonly(effects)
    @test Core.Compiler.is_effect_free(effects)
    @test !Core.Compiler.is_nothrow(effects)
    @test Core.Compiler.is_terminates(effects)
end
# Test that arraysize has proper effect modeling
@test fully_eliminated(M->(size(M, 2); nothing), (Matrix{Float64},))

# arraylen
# --------

let effects = Base.infer_effects(length, (Vector{Any},))
    @test Core.Compiler.is_consistent_if_inaccessiblememonly(effects)
    @test Core.Compiler.is_effect_free(effects)
    @test Core.Compiler.is_nothrow(effects)
    @test Core.Compiler.is_terminates(effects)
end

# resize
# ------

#for op = Any[
#        Base._growbeg!,
#        Base._growend!,
#        Base._deletebeg!,
#        Base._deleteend!,
#    ]
#    let effects = Base.infer_effects(op, (Vector, Int))
#        @test Core.Compiler.is_effect_free_if_inaccessiblememonly(effects)
#        @test Core.Compiler.is_terminates(effects)
#        @test !Core.Compiler.is_nothrow(effects)
#    end
#end
#
# tuple indexing
# --------------

@test Core.Compiler.is_foldable(Base.infer_effects(iterate, Tuple{Tuple{Int, Int}, Int}))

# end to end
# ----------

#function simple_vec_ops(T, op!, op, xs...)
#    a = T[]
#    op!(a, xs...)
#    return op(a)
#end
#for T = Any[Int,Any], op! = Any[push!,pushfirst!], op = Any[length,size],
#    xs = Any[(Int,), (Int,Int,)]
#    let effects = Base.infer_effects(simple_vec_ops, (Type{T},typeof(op!),typeof(op),xs...))
#        @test Core.Compiler.is_foldable(effects)
#    end
#end

# Test that builtin_effects handles vararg correctly
@test !Core.Compiler.is_nothrow(Core.Compiler.builtin_effects(Core.Compiler.fallback_lattice, Core.isdefined,
    Any[String, Vararg{Any}], Bool))

# Test that :new can be eliminated even if an sparam is unknown
struct SparamUnused{T}
    x
    SparamUnused(x::T) where {T} = new{T}(x)
end
mksparamunused(x) = (SparamUnused(x); nothing)
let src = code_typed1(mksparamunused, (Any,))
    @test count(isnew, src.code) == 0
end

struct WrapperOneField{T}
    x::T
end

# Effects for getfield of type instance
@test Base.infer_effects(Tuple{Nothing}) do x
    WrapperOneField{typeof(x)}.instance
end |> Core.Compiler.is_foldable_nothrow
@test Base.infer_effects(Tuple{WrapperOneField{Float64}, Symbol}) do w, s
    getfield(w, s)
end |> Core.Compiler.is_foldable
@test Core.Compiler.getfield_notuninit(WrapperOneField{Float64}, Symbol)
@test Base.infer_effects(Tuple{WrapperOneField{Symbol}, Symbol}) do w, s
    getfield(w, s)
end |> Core.Compiler.is_foldable

# Flow-sensitive consistent for _typevar
@test Base.infer_effects() do
    return WrapperOneField == (WrapperOneField{T} where T)
end |> Core.Compiler.is_foldable_nothrow

# Test that dead `@inbounds` does not taint consistency
# https://github.com/JuliaLang/julia/issues/48243
@test Base.infer_effects(Tuple{Int64}) do i
    false && @inbounds (1,2,3)[i]
    return 1
end |> Core.Compiler.is_foldable_nothrow

@test Base.infer_effects(Tuple{Int64}) do i
    @inbounds (1,2,3)[i]
end |> !Core.Compiler.is_noub

@test Base.infer_effects(Tuple{Tuple{Int64}}) do x
    @inbounds x[1]
end |> Core.Compiler.is_foldable_nothrow

# Test that :new of non-concrete, but otherwise known type
# does not taint consistency.
@eval struct ImmutRef{T}
    x::T
    ImmutRef(x) = $(Expr(:new, :(ImmutRef{typeof(x)}), :x))
end
@test Core.Compiler.is_foldable(Base.infer_effects(ImmutRef, Tuple{Any}))

@test Core.Compiler.is_foldable_nothrow(Base.infer_effects(typejoin, ()))

# nothrow-ness of subtyping operations
# https://github.com/JuliaLang/julia/pull/48566
@test !Core.Compiler.is_nothrow(Base.infer_effects((A,B)->A<:B, (Any,Any)))
@test !Core.Compiler.is_nothrow(Base.infer_effects((A,B)->A>:B, (Any,Any)))

# GotoIfNot should properly mark itself as throwing when given a non-Bool
# https://github.com/JuliaLang/julia/pull/48583
gotoifnot_throw_check_48583(x) = x ? x : 0
@test !Core.Compiler.is_nothrow(Base.infer_effects(gotoifnot_throw_check_48583, (Missing,)))
@test !Core.Compiler.is_nothrow(Base.infer_effects(gotoifnot_throw_check_48583, (Any,)))
@test Core.Compiler.is_nothrow(Base.infer_effects(gotoifnot_throw_check_48583, (Bool,)))

# unknown :static_parameter should taint :nothrow
# https://github.com/JuliaLang/julia/issues/46771
unknown_sparam_throw(::Union{Nothing, Type{T}}) where T = (T; nothing)
unknown_sparam_nothrow1(x::Ref{T}) where T = (T; nothing)
unknown_sparam_nothrow2(x::Ref{Ref{T}}) where T = (T; nothing)
@test Core.Compiler.is_nothrow(Base.infer_effects(unknown_sparam_throw, (Type{Int},)))
@test Core.Compiler.is_nothrow(Base.infer_effects(unknown_sparam_throw, (Type{<:Integer},)))
@test !Core.Compiler.is_nothrow(Base.infer_effects(unknown_sparam_throw, (Type,)))
@test !Core.Compiler.is_nothrow(Base.infer_effects(unknown_sparam_throw, (Nothing,)))
@test !Core.Compiler.is_nothrow(Base.infer_effects(unknown_sparam_throw, (Union{Type{Int},Nothing},)))
@test !Core.Compiler.is_nothrow(Base.infer_effects(unknown_sparam_throw, (Any,)))
@test Core.Compiler.is_nothrow(Base.infer_effects(unknown_sparam_nothrow1, (Ref,)))
@test Core.Compiler.is_nothrow(Base.infer_effects(unknown_sparam_nothrow2, (Ref{Ref{T}} where T,)))

# purely abstract recursion should not taint :terminates
# https://github.com/JuliaLang/julia/issues/48983
abstractly_recursive1() = abstractly_recursive2()
abstractly_recursive2() = (Core.Compiler._return_type(abstractly_recursive1, Tuple{}); 1)
abstractly_recursive3() = abstractly_recursive2()
@test Core.Compiler.is_terminates(Base.infer_effects(abstractly_recursive3, ()))
actually_recursive1(x) = actually_recursive2(x)
actually_recursive2(x) = (x <= 0) ? 1 : actually_recursive1(x - 1)
actually_recursive3(x) = actually_recursive2(x)
@test !Core.Compiler.is_terminates(Base.infer_effects(actually_recursive3, (Int,)))

# `isdefined` effects
struct MaybeSome{T}
    value::T
    MaybeSome(x::T) where T = new{T}(x)
    MaybeSome{T}(x::T) where T = new{T}(x)
    MaybeSome{T}() where T = new{T}()
end
const undefined_ref = Ref{String}()
const defined_ref = Ref{String}("julia")
const undefined_some = MaybeSome{String}()
const defined_some = MaybeSome{String}("julia")
let effects = Base.infer_effects() do
        isdefined(undefined_ref, :x)
    end
    @test !Core.Compiler.is_consistent(effects)
    @test Core.Compiler.is_nothrow(effects)
end
let effects = Base.infer_effects() do
        isdefined(defined_ref, :x)
    end
    @test !Core.Compiler.is_consistent(effects)
    @test Core.Compiler.is_nothrow(effects)
end
let effects = Base.infer_effects() do
        isdefined(undefined_some, :value)
    end
    @test Core.Compiler.is_consistent(effects)
    @test Core.Compiler.is_nothrow(effects)
end
let effects = Base.infer_effects() do
        isdefined(defined_some, :value)
    end
    @test Core.Compiler.is_consistent(effects)
    @test Core.Compiler.is_nothrow(effects)
end
# high-level interface test
isassigned_effects(s) = isassigned(Ref(s))
@test Core.Compiler.is_consistent(Base.infer_effects(isassigned_effects, (Symbol,)))
@test fully_eliminated(; retval=true) do
    isassigned_effects(:foo)
end

# inference on throw block should be disabled only when the effects are already known to be
# concrete-eval ineligible:
function optimize_throw_block_for_effects(x)
    a = [x]
    if x < 0
        throw(ArgumentError(lazy"negative number given: $x"))
    end
    return a
end
let effects = Base.infer_effects(optimize_throw_block_for_effects, (Int,))
    @test Core.Compiler.is_consistent_if_notreturned(effects)
    @test Core.Compiler.is_effect_free(effects)
    @test !Core.Compiler.is_nothrow(effects)
    @test Core.Compiler.is_terminates(effects)
end

# :isdefined effects
@test @eval Base.infer_effects() do
    @isdefined($(gensym("some_undef_symbol")))
end |> !Core.Compiler.is_consistent

# Effects of Base.hasfield (#50198)
hf50198(s) = hasfield(typeof((;x=1, y=2)), s)
f50198() = (hf50198(Ref(:x)[]); nothing)
@test fully_eliminated(f50198)

# Effects properly applied to flags by irinterp (#50311)
f50311(x, s) = Symbol(s)
g50311(x) = Val{f50311((1.0, x), "foo")}()
@test fully_eliminated(g50311, Tuple{Float64})

# getglobal effects
const my_defined_var = 42
@test Base.infer_effects() do
    getglobal(@__MODULE__, :my_defined_var, :monotonic)
end |> Core.Compiler.is_foldable_nothrow
@test Base.infer_effects() do
    getglobal(@__MODULE__, :my_defined_var, :foo)
end |> !Core.Compiler.is_nothrow
@test Base.infer_effects() do
    getglobal(@__MODULE__, :my_defined_var, :foo, nothing)
end |> !Core.Compiler.is_nothrow

# irinterp should refine `:nothrow` information only if profitable
Base.@assume_effects :nothrow function irinterp_nothrow_override(x, y)
    z = sin(y)
    if x
        return "julia"
    end
    return z
end
@test Base.infer_effects((Float64,)) do y
    isinf(y) && return zero(y)
    irinterp_nothrow_override(true, y)
end |> Core.Compiler.is_nothrow

# Effects for :compilerbarrier
f1_compilerbarrier(b) = Base.compilerbarrier(:type, b)
f2_compilerbarrier(b) = Base.compilerbarrier(:conditional, b)

@test !Core.Compiler.is_consistent(Base.infer_effects(f1_compilerbarrier, (Bool,)))
@test Core.Compiler.is_consistent(Base.infer_effects(f2_compilerbarrier, (Bool,)))

# Optimizer-refined effects
function f1_optrefine(b)
    if Base.inferencebarrier(b)
        error()
    end
    return b
end
@test !Core.Compiler.is_consistent(Base.infer_effects(f1_optrefine, (Bool,)))

function f2_optrefine()
    if Ref(false)[]
        error()
    end
    return true
end
@test Core.Compiler.is_nothrow(Base.infer_effects(f2_optrefine))

function f3_optrefine(x)
    @fastmath sqrt(x)
    return x
end
@test Core.Compiler.is_consistent(Base.infer_effects(f3_optrefine, (Float64,)))

# Check that :consistent is properly modeled for throwing statements
const GLOBAL_MUTABLE_SWITCH = Ref{Bool}(false)

check_switch(switch::Base.RefValue{Bool}) = (switch[] && error(); return nothing)
check_switch2() = check_switch(GLOBAL_MUTABLE_SWITCH)

@test (Base.return_types(check_switch2) |> only) === Nothing
GLOBAL_MUTABLE_SWITCH[] = true
# Check that flipping the switch doesn't accidentally change the return type
@test (Base.return_types(check_switch2) |> only) === Nothing

@test !Core.Compiler.is_consistent(Base.infer_effects(check_switch, (Base.RefValue{Bool},)))

# post-opt IPO analysis refinement of `:effect_free`-ness
function post_opt_refine_effect_free(y, c=true)
    x = Ref(c)
    if x[]
        return true
    else
        r = y[] isa Number
        y[] = nothing
    end
    return r
end
@test Core.Compiler.is_effect_free(Base.infer_effects(post_opt_refine_effect_free, (Base.RefValue{Any},)))
@test Base.infer_effects((Base.RefValue{Any},)) do y
    post_opt_refine_effect_free(y, true)
end |> Core.Compiler.is_effect_free

# Check EA-based refinement of :effect_free
Base.@assume_effects :nothrow @noinline _noinline_set!(x) = (x[] = 1; nothing)

function set_ref_with_unused_arg_1(_)
    x = Ref(0)
    _noinline_set!(x)
    return nothing
end
function set_ref_with_unused_arg_2(_)
    x = @noinline Ref(0)
    _noinline_set!(x)
    return nothing
end
function set_arg_ref!(x)
    _noinline_set!(x)
    y = Ref(false)
    y[] && (Main.x = x)
    return nothing
end

function set_arr_with_unused_arg_1(_)
    x = Int[0]
    _noinline_set!(x)
    return nothing
end
function set_arr_with_unused_arg_2(_)
    x = @noinline Int[0]
    _noinline_set!(x)
    return nothing
end
function set_arg_arr!(x)
    _noinline_set!(x)
    y = Bool[false]
    y[] && (Main.x = x)
    return nothing
end

# This is inferable by type analysis only since the arguments have no mutable memory
@test Core.Compiler.is_effect_free_if_inaccessiblememonly(Base.infer_effects(_noinline_set!, (Base.RefValue{Int},)))
@test Core.Compiler.is_effect_free_if_inaccessiblememonly(Base.infer_effects(_noinline_set!, (Vector{Int},)))
for func in (set_ref_with_unused_arg_1, set_ref_with_unused_arg_2,
             set_arr_with_unused_arg_1, set_arr_with_unused_arg_2)
    effects = Base.infer_effects(func, (Nothing,))
    @test Core.Compiler.is_inaccessiblememonly(effects)
    @test Core.Compiler.is_effect_free(effects)
end

# These need EA
@test Core.Compiler.is_effect_free(Base.infer_effects(set_ref_with_unused_arg_1, (Base.RefValue{Int},)))
@test Core.Compiler.is_effect_free(Base.infer_effects(set_ref_with_unused_arg_2, (Base.RefValue{Int},)))
@test Core.Compiler.is_effect_free_if_inaccessiblememonly(Base.infer_effects(set_arg_ref!, (Base.RefValue{Int},)))
@test_broken Core.Compiler.is_effect_free(Base.infer_effects(set_arr_with_unused_arg_1, (Vector{Int},)))
@test_broken Core.Compiler.is_effect_free(Base.infer_effects(set_arr_with_unused_arg_2, (Vector{Int},)))
@test_broken Core.Compiler.is_effect_free_if_inaccessiblememonly(Base.infer_effects(set_arg_arr!, (Vector{Int},)))

function issue51837(; openquotechar::Char, newlinechar::Char)
    ncodeunits(openquotechar) == 1 || throw(ArgumentError("`openquotechar` must be a single-byte character"))
    if !isnothing(newlinechar)
        ncodeunits(newlinechar) > 1 && throw(ArgumentError("`newlinechar` must be a single-byte character."))
    end
    return nothing
end
@test Base.infer_effects() do openquotechar::Char, newlinechar::Char
    issue51837(; openquotechar, newlinechar)
end |> !Core.Compiler.is_nothrow
@test_throws ArgumentError issue51837(; openquotechar='α', newlinechar='\n')

# idempotency of effects derived by post-opt analysis
callgetfield(x, f) = getfield(x, f, Base.@_boundscheck)
@test Base.infer_effects(callgetfield, (Some{Any},Symbol)).noub === Core.Compiler.NOUB_IF_NOINBOUNDS
callgetfield1(x, f) = getfield(x, f, Base.@_boundscheck)
callgetfield_simple(x, f) = callgetfield1(x, f)
@test Base.infer_effects(callgetfield_simple, (Some{Any},Symbol)).noub ===
      Base.infer_effects(callgetfield_simple, (Some{Any},Symbol)).noub ===
      Core.Compiler.ALWAYS_TRUE
callgetfield2(x, f) = getfield(x, f, Base.@_boundscheck)
callgetfield_inbounds(x, f) = @inbounds callgetfield2(x, f)
@test Base.infer_effects(callgetfield_inbounds, (Some{Any},Symbol)).noub ===
      Base.infer_effects(callgetfield_inbounds, (Some{Any},Symbol)).noub ===
      Core.Compiler.ALWAYS_FALSE

# noub modeling for memory ops
let (memoryref, memoryrefget, memoryref_isassigned, memoryrefset!) =
        (Core.memoryref, Core.memoryrefget, Core.memoryref_isassigned, Core.memoryrefset!)
    function builtin_effects(@nospecialize xs...)
        interp = Core.Compiler.NativeInterpreter()
        𝕃 = Core.Compiler.typeinf_lattice(interp)
        rt = Core.Compiler.builtin_tfunction(interp, xs..., nothing)
        return Core.Compiler.builtin_effects(𝕃, xs..., rt)
    end
    @test Core.Compiler.is_noub(builtin_effects(memoryref, Any[Memory,]))
    @test Core.Compiler.is_noub(builtin_effects(memoryref, Any[MemoryRef,Int]))
    @test Core.Compiler.is_noub(builtin_effects(memoryref, Any[MemoryRef,Int,Core.Const(true)]))
    @test !Core.Compiler.is_noub(builtin_effects(memoryref, Any[MemoryRef,Int,Core.Const(false)]))
    @test !Core.Compiler.is_noub(builtin_effects(memoryref, Any[MemoryRef,Int,Bool]))
    @test Core.Compiler.is_noub(builtin_effects(memoryref, Any[MemoryRef,Int,Int]))
    @test !Core.Compiler.is_noub(builtin_effects(memoryref, Any[MemoryRef,Int,Vararg{Bool}]))
    @test !Core.Compiler.is_noub(builtin_effects(memoryref, Any[MemoryRef,Vararg{Any}]))
    @test Core.Compiler.is_noub(builtin_effects(memoryrefget, Any[MemoryRef,Symbol,Core.Const(true)]))
    @test !Core.Compiler.is_noub(builtin_effects(memoryrefget, Any[MemoryRef,Symbol,Core.Const(false)]))
    @test !Core.Compiler.is_noub(builtin_effects(memoryrefget, Any[MemoryRef,Symbol,Bool]))
    @test Core.Compiler.is_noub(builtin_effects(memoryrefget, Any[MemoryRef,Symbol,Int]))
    @test !Core.Compiler.is_noub(builtin_effects(memoryrefget, Any[MemoryRef,Symbol,Vararg{Bool}]))
    @test !Core.Compiler.is_noub(builtin_effects(memoryrefget, Any[MemoryRef,Vararg{Any}]))
    @test Core.Compiler.is_noub(builtin_effects(memoryref_isassigned, Any[MemoryRef,Symbol,Core.Const(true)]))
    @test !Core.Compiler.is_noub(builtin_effects(memoryref_isassigned, Any[MemoryRef,Symbol,Core.Const(false)]))
    @test !Core.Compiler.is_noub(builtin_effects(memoryref_isassigned, Any[MemoryRef,Symbol,Bool]))
    @test Core.Compiler.is_noub(builtin_effects(memoryref_isassigned, Any[MemoryRef,Symbol,Int]))
    @test !Core.Compiler.is_noub(builtin_effects(memoryref_isassigned, Any[MemoryRef,Symbol,Vararg{Bool}]))
    @test !Core.Compiler.is_noub(builtin_effects(memoryref_isassigned, Any[MemoryRef,Vararg{Any}]))
    @test Core.Compiler.is_noub(builtin_effects(memoryrefset!, Any[MemoryRef,Any,Symbol,Core.Const(true)]))
    @test !Core.Compiler.is_noub(builtin_effects(memoryrefset!, Any[MemoryRef,Any,Symbol,Core.Const(false)]))
    @test !Core.Compiler.is_noub(builtin_effects(memoryrefset!, Any[MemoryRef,Any,Symbol,Bool]))
    @test Core.Compiler.is_noub(builtin_effects(memoryrefset!, Any[MemoryRef,Any,Symbol,Int]))
    @test !Core.Compiler.is_noub(builtin_effects(memoryrefset!, Any[MemoryRef,Any,Symbol,Vararg{Bool}]))
    @test !Core.Compiler.is_noub(builtin_effects(memoryrefset!, Any[MemoryRef,Vararg{Any}]))
    # `:boundscheck` taint should be refined by post-opt analysis
    @test Base.infer_effects() do xs::Vector{Any}, i::Int
        memoryrefget(memoryref(getfield(xs, :ref), i, Base.@_boundscheck), :not_atomic, Base.@_boundscheck)
    end |> Core.Compiler.is_noub_if_noinbounds
end

# high level tests
@test Core.Compiler.is_noub_if_noinbounds(Base.infer_effects(getindex, (Vector{Int},Int)))
@test Core.Compiler.is_noub_if_noinbounds(Base.infer_effects(getindex, (Vector{Any},Int)))
@test Core.Compiler.is_noub_if_noinbounds(Base.infer_effects(setindex!, (Vector{Int},Int,Int)))
@test Core.Compiler.is_noub_if_noinbounds(Base.infer_effects(setindex!, (Vector{Any},Any,Int)))
@test Core.Compiler.is_noub_if_noinbounds(Base.infer_effects(isassigned, (Vector{Int},Int)))
@test Core.Compiler.is_noub_if_noinbounds(Base.infer_effects(isassigned, (Vector{Any},Int)))
@test Base.infer_effects((Vector{Int},Int)) do xs, i
    xs[i]
end |> Core.Compiler.is_noub
@test Base.infer_effects((Vector{Any},Int)) do xs, i
    xs[i]
end |> Core.Compiler.is_noub
@test Base.infer_effects((Vector{Int},Int,Int)) do xs, x, i
    xs[i] = x
end |> Core.Compiler.is_noub
@test Base.infer_effects((Vector{Any},Any,Int)) do xs, x, i
    xs[i] = x
end |> Core.Compiler.is_noub
@test Base.infer_effects((Vector{Int},Int)) do xs, i
    @inbounds xs[i]
end |> !Core.Compiler.is_noub
@test Base.infer_effects((Vector{Any},Int)) do xs, i
    @inbounds xs[i]
end |> !Core.Compiler.is_noub
Base.@propagate_inbounds getindex_propagate(xs, i) = xs[i]
getindex_dont_propagate(xs, i) = xs[i]
@test Core.Compiler.is_noub_if_noinbounds(Base.infer_effects(getindex_propagate, (Vector{Any},Int)))
@test Core.Compiler.is_noub(Base.infer_effects(getindex_dont_propagate, (Vector{Any},Int)))
@test Base.infer_effects((Vector{Any},Int)) do xs, i
    @inbounds getindex_propagate(xs, i)
end |> !Core.Compiler.is_noub
@test Base.infer_effects((Vector{Any},Int)) do xs, i
    @inbounds getindex_dont_propagate(xs, i)
end |> Core.Compiler.is_noub

# refine `:nothrow` when `exct` is known to be `Bottom`
@test Base.infer_exception_type(getindex, (Vector{Int},Int)) == BoundsError
function getindex_nothrow(xs::Vector{Int}, i::Int)
    try
        return xs[i]
    catch err
        err isa BoundsError && return nothing
        rethrow(err)
    end
end
@test Core.Compiler.is_nothrow(Base.infer_effects(getindex_nothrow, (Vector{Int}, Int)))

# callsite `@assume_effects` annotation
let ast = code_lowered((Int,)) do x
        Base.@assume_effects :total identity(x)
    end |> only
    ssaflag = ast.ssaflags[findfirst(!iszero, ast.ssaflags)::Int]
    override = Core.Compiler.decode_statement_effects_override(ssaflag)
    # if this gets broken, check if this is synced with expr.jl
    @test override.consistent && override.effect_free && override.nothrow &&
          override.terminates_globally && !override.terminates_locally &&
          override.notaskstate && override.inaccessiblememonly &&
          override.noub && !override.noub_if_noinbounds
end
@test Base.infer_effects((Float64,)) do x
    isinf(x) && return 0.0
    return Base.@assume_effects :nothrow sin(x)
end |> Core.Compiler.is_nothrow
let effects = Base.infer_effects((Vector{Float64},)) do xs
        isempty(xs) && return 0.0
        Base.@assume_effects :nothrow begin
            x = Base.@assume_effects :noub @inbounds xs[1]
            isinf(x) && return 0.0
            return sin(x)
        end
    end
    # all nested overrides should be applied
    @test Core.Compiler.is_nothrow(effects)
    @test Core.Compiler.is_noub(effects)
end
@test Base.infer_effects((Int,)) do x
    res = 1
    0 ≤ x < 20 || error("bad fact")
    Base.@assume_effects :terminates_locally while x > 1
        res *= x
        x -= 1
    end
    return res
end |> Core.Compiler.is_terminates

# https://github.com/JuliaLang/julia/issues/52531
const a52531 = Core.Ref(1)
@eval getref52531() = $(QuoteNode(a52531)).x
@test !Core.Compiler.is_consistent(Base.infer_effects(getref52531))
let
    global set_a52531!, get_a52531
    _a::Int             = -1
    set_a52531!(a::Int) = (_a = a; return get_a52531())
    get_a52531()        = _a
end
@test !Core.Compiler.is_consistent(Base.infer_effects(set_a52531!, (Int,)))
@test !Core.Compiler.is_consistent(Base.infer_effects(get_a52531, ()))
@test get_a52531() == -1
@test set_a52531!(1) == 1
@test get_a52531() == 1

let
    global is_initialized52531, set_initialized52531!
    _is_initialized                   = false
    set_initialized52531!(flag::Bool) = (_is_initialized = flag)
    is_initialized52531()             = _is_initialized
end
top_52531(_) = (set_initialized52531!(true); nothing)
@test !Core.Compiler.is_consistent(Base.infer_effects(is_initialized52531))
@test !Core.Compiler.is_removable_if_unused(Base.infer_effects(set_initialized52531!, (Bool,)))
@test !is_initialized52531()
top_52531(0)
@test is_initialized52531()

const ref52843 = Ref{Int}()
@eval func52843() = ($ref52843[] = 1; nothing)
@test !Core.Compiler.is_foldable(Base.infer_effects(func52843))
let; Base.Experimental.@force_compile; func52843(); end
@test ref52843[] == 1

@test Core.Compiler.is_inaccessiblememonly(Base.infer_effects(identity∘identity, Tuple{Any}))
@test Core.Compiler.is_inaccessiblememonly(Base.infer_effects(()->Vararg, Tuple{}))

# pointerref nothrow for invalid pointer
@test !Core.Compiler.intrinsic_nothrow(Core.Intrinsics.pointerref, Any[Type{Ptr{Vector{Int64}}}, Int, Int])
@test !Core.Compiler.intrinsic_nothrow(Core.Intrinsics.pointerref, Any[Type{Ptr{T}} where T, Int, Int])

# post-opt :consistent-cy analysis correctness
# https://github.com/JuliaLang/julia/issues/53508
@test !Core.Compiler.is_consistent(Base.infer_effects(getindex, (UnitRange{Int},Int)))
@test !Core.Compiler.is_consistent(Base.infer_effects(getindex, (Base.OneTo{Int},Int)))

@noinline f53613() = @assert isdefined(@__MODULE__, :v53613)
g53613() = f53613()
h53613() = g53613()
@test !Core.Compiler.is_consistent(Base.infer_effects(f53613))
@test !Core.Compiler.is_consistent(Base.infer_effects(g53613))
@test_throws AssertionError f53613()
@test_throws AssertionError g53613()
@test_throws AssertionError h53613()
global v53613 = nothing
@test f53613() === nothing
@test g53613() === nothing
@test h53613() === nothing
