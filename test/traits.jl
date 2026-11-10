# This file is a part of Julia. License is MIT: https://julialang.org/license

# Traits and guarded methods (doc/src/devdocs/traits.md). Definitions are at top level, so
# that each testset runs in a world that sees them. Guarded methods on Base functions would
# change dispatch for other tests in this process, so those run in subprocesses.

using Test
using Base.Experimental: @guarded, @trait, @overlay

abstract type Backend end
struct CPU <: Backend end
abstract type GPU <: Backend end
struct Dev <: GPU end
struct OtherDev <: GPU end

# a device array, and a trait that follows the parents of wrappers
struct DevArray{T,N} <: AbstractArray{T,N}
    data::Array{T,N}
end
Base.size(x::DevArray) = size(x.data)
Base.getindex(x::DevArray, i::Int...) = x.data[i...]

@trait backend::Backend = CPU
@trait backend(DevArray) = Dev
@trait backend(SubArray{T,N,P}) = backend(P)
@trait backend(Base.ReshapedArray{T,N,P}) = backend(P)
@trait backend(PermutedDimsArray{T,N,perm,iperm,P}) = backend(P)

const x0 = [1.0 2.0; 3.0 4.0]
const d0 = DevArray(x0)

kind(a::AbstractArray) = :cpu
@guarded kind(a::AbstractArray) where {backend(a)::GPU} = :gpu
kind_static(a) = kind(a)
kind_dynamic(a) = kind(Base.inferencebarrier(a))

# the labels of the values of type `T`
region(trait, @nospecialize(T)) =
    ccall(:jl_trait_region, Any, (Any, Any, Int, UInt), trait, Tuple{Nothing,T}, 1, Base.get_world_counter())
# whether the code compiled for `f(::T)` is still valid
isvalid_code(f, @nospecialize(T)) = Base.method_instance(f, (T,)).cache.max_world == typemax(UInt)

@testset "nested wrappers" begin
    for a in Any[d0, view(d0, :, 1), vec(PermutedDimsArray(d0, (2, 1))), view(vec(d0), 2:3)]
        @test kind_static(a) === kind_dynamic(a) === :gpu
    end
    for a in Any[x0, view(x0, :, 1), vec(PermutedDimsArray(x0, (2, 1)))]
        @test kind_static(a) === kind_dynamic(a) === :cpu
    end
    @test occursin("kind(a::AbstractArray) where {backend(a)::$GPU}", string(methods(kind)))
end

@testset "labels" begin
    V{P} = SubArray{Float64,1,P,Tuple{Base.Slice{Base.OneTo{Int}},Int},true}
    @test backend(Matrix{Float64}) === backend(Int) === CPU
    @test backend(DevArray{Float64,2}) === backend(DevArray) === Dev
    @test backend(V{DevArray{Float64,2}}) === Dev
    # a type parameter has the label all its values share
    @test backend(V{DevArray}) === backend(V{<:DevArray}) === Dev
    @test backend(V{AbstractMatrix{Float64}}) === backend(V{Any}) === CPU
    @test backend(SubArray) === CPU
    # for a type argument `X`, the label of `X`
    @test region(backend, Type{DevArray{Int,1}}) === Dev
    @test region(backend, Type{Vector{Int}}) === region(backend, Type{Union{}}) === CPU
    @test region(backend, DataType) === Backend
    # abstract types can have subtypes with any label
    @test region(backend, AbstractMatrix{Float64}) === region(backend, Any) === Backend
    @test region(backend, Union{DevArray{Int,1},Vector{Int}}) == Union{CPU,Dev}
end

# inference resolves a call when the argument type determines the label, even if the parent
# of a wrapper is only bounded
@testset "inference" begin
    V{P} = SubArray{Float64,1,P,Tuple{Base.Slice{Base.OneTo{Int}},Int},true}
    for (T, r) in Any[typeof(view(d0, :, 1)) => :gpu, V{<:DevArray} => :gpu,
                      Matrix{Float64} => :cpu, V{Matrix{Float64}} => :cpu]
        @test only(code_typed(kind_static, (T,)))[1].code[end] == Core.ReturnNode(QuoteNode(r))
    end
    for T in Any[AbstractMatrix{Float64}, V{<:AbstractMatrix{Float64}}]
        @test !Base.infer_effects(kind_static, (T,)).nothrow
        @test Base.return_types(kind_static, (T,)) == Any[Symbol]
    end
end

# a type variable may be `Union{}`, which has the default label, unless it is the type of a
# field: no wrapper holds a value of type `Union{}`
struct Box{T} end
@trait backend(Box{T}) = backend(T)
bk(x) = :cpu
@guarded bk(x) where {backend(x)::GPU} = :gpu
@testset "bottom parameters" begin
    @test backend(Box{Union{}}) === CPU
    @test region(backend, Box{T} where T<:DevArray) == Union{CPU,Dev}
    @test region(backend, SubArray{Float64,1,P} where P<:DevArray) === Dev
    @test region(backend, Type{T} where T<:DevArray) == Union{CPU,Dev}
    @test backend(Box{T} where {L<:DevArray, L<:T<:DevArray}) === CPU # not normalized, unlike in a Tuple
    @test bk(Box{Union{}}()) === :cpu
    @test invoke(bk, Tuple{Box{T} where T<:DevArray}, Box{Union{}}()) === :cpu
    D = DevArray{Float64,1}
    for _ = 1:40
        D = Box{D}
    end
    @test backend(D) === Dev
    @test region(backend, Union{D,Int8,Int16,Int32,Float32}) == Union{CPU,Dev}
end

# a method for part of an abstract type is still called for it
abstract type AbstractDev1 <: AbstractVector{Int} end
struct Dev1A <: AbstractDev1 end
struct Dev1B <: AbstractDev1 end
@trait backend(AbstractDev1) = Dev
@guarded partial(x::AbstractArray) where {backend(x)::GPU} = 1
partial(x::Dev1A) = "special"
partial_static(x) = partial(x)
@testset "partly covering methods" begin
    @test partial(Dev1A()) == "special" && partial(Dev1B()) == 1
    @test only(Base.return_types(partial_static, (AbstractDev1,))) >: Union{Int,String}
    @test only(Base.return_types(partial_static, (Dev1B,))) === Int
end

targ(::Type) = :cpu
@guarded targ(t::Type) where {backend(t)::GPU} = :gpu
@testset "type arguments" begin
    @test targ(DevArray{Float64,2}) === targ(typeof(view(d0, :, 1))) === :gpu
    @test targ(DevArray) === :gpu
    @test targ(Matrix{Float64}) === targ(AbstractArray) === :cpu
    @test targ(Base.inferencebarrier(DevArray{Int,1})) === :gpu
end

# --- specificity ------------------------------------------------------------------------

# Labels come before signatures: a method that requires narrower labels is more specific.
# (AbstractArray; GPU) requires a GPU label and (SubArray; -) none, so the former is called
# for a view of a device array. (DevArray; -) requires the label of `DevArray`, which is
# narrower still. Signatures order methods that require the same labels.
op(a::AbstractArray) = :generic
op(a::SubArray) = :view
op(a::DevArray) = :dev
op(a::SubArray{<:Any,<:Any,<:DevArray{Int}}) = :intview
@guarded op(a::AbstractArray) where {backend(a)::GPU} = :gpu
op_static(a) = op(a)

@testset "specificity" begin
    @test op(x0) === op_static(x0) === :generic
    @test op(view(x0, :, 1)) === :view
    @test op(vec(d0)) === op_static(vec(d0)) === :gpu
    @test op(d0) === op_static(d0) === :dev
    @test op(view(DevArray([1 2; 3 4]), :, 1)) === :intview
    @test only(code_typed(op_static, (typeof(d0),)))[1].code[end] == Core.ReturnNode(QuoteNode(:dev))
    @test op(view(d0, :, 1)) === op_static(view(d0, :, 1)) === :gpu
end

@guarded op(a::SubArray) where {backend(a)::GPU} = :gpu_view

@testset "same labels, narrower signature" begin
    @test op(view(d0, :, 1)) === op_static(view(d0, :, 1)) === :gpu_view
    @test op(view(x0, :, 1)) === :view
    @test op(view(DevArray([1 2; 3 4]), :, 1)) === :intview
end

# methods are ambiguous if each requires narrower labels for some guard, or if they
# require the same labels and their signatures are ambiguous
@trait speed::Backend = CPU
@trait speed(DevArray) = OtherDev
amb(a, b) = :neither
@guarded amb(a, b) where {backend(a)::GPU} = :first
@guarded amb(a, b) where {backend(b)::GPU} = :second
@guarded sigamb(a::AbstractArray, b) where {backend(a)::GPU} = 1
@guarded sigamb(a, b::AbstractArray) where {backend(a)::GPU} = 2
@guarded traitamb(a) where {backend(a)::GPU} = 1
@guarded traitamb(a) where {speed(a)::GPU} = 2
@testset "ambiguities" begin
    @test amb(x0, x0) === :neither
    @test amb(x0, d0) === :second
    @test amb(d0, x0) === :first
    for f in (() -> amb(d0, d0), () -> sigamb(d0, d0), () -> traitamb(d0))
        err = try f(); catch e; e; end
        @test err isa MethodError && occursin("ambiguous", sprint(showerror, err))
    end
    @test sigamb(d0, 1) === 1
    @test Base.isambiguous(methods(sigamb)...) && Base.isambiguous(methods(traitamb)...)
end
@guarded amb(a, b) where {backend(a)::GPU, backend(b)::GPU} = :both
@guarded traitamb(a) where {backend(a)::GPU, speed(a)::GPU} = 3
@testset "resolved ambiguities" begin
    @test amb(d0, d0) === :both
    @test traitamb(d0) === 3
end

# an explicit guard beats the one a signature implies; narrower bounds beat wider ones
@trait backend(Int) = OtherDev
gb(x) = :any
@guarded gb(x) where {backend(x)::GPU} = :gpu
@guarded gb(x) where {backend(x)::Dev} = :dev
gb(x::DevArray{Int}) = :sig
@guarded gb(x::DevArray{Float64}) where {backend(x)::GPU} = :sig_gpu
# a method beats the one it replaces, and still any guarded method its signature beat;
# reflection finds the selected method before any call has
rd(x::Int) = :first
@guarded rd(x) where {backend(x)::GPU} = :guarded
@test rd(1) === :first
rd(x::Int) = :replacement
cold(x::Int) = :int
@guarded cold(x) where {backend(x)::GPU} = :guarded
@testset "guard bounds" begin
    @test rd(1) === :replacement
    @test only(code_typed(cold, (Int,)))[1].code[end] == Core.ReturnNode(QuoteNode(:int))
    @test only(methods(cold, (Int,))).guards === nothing
    @test gb(1.0) === :any
    @test gb(1) === :gpu
    @test gb(vec(d0)) === :dev
    @test gb(DevArray([1])) === :sig
    @test gb(d0) === :sig_gpu
    @test Base.morespecific(which(gb, (DevArray{Int,1},)), which(gb, (typeof(vec(d0)),)))
    @test !Base.morespecific(which(gb, (typeof(vec(d0)),)), which(gb, (DevArray{Int,1},)))
end

# --- method definitions -----------------------------------------------------------------

# keyword and optional arguments, argument macros, return types, several guards
kw(a::AbstractArray; scale=1) = (:cpu, scale)
@guarded kw(a::AbstractArray; scale=1) where {backend(a)::GPU} = (:gpu, scale)
kw_static(a) = kw(a; scale=5)
opt(a::AbstractArray, n=1) = (:cpu, n)
@guarded opt(a::AbstractArray, n=1) where {backend(a)::GPU} = (:gpu, n)
nsp(@nospecialize(a)) = :cpu
@guarded nsp(@nospecialize(a::AbstractArray)) where {backend(a)::GPU} = :gpu
@guarded (rett(a::AbstractArray{T})::Tuple{Symbol,DataType}) where {T, backend(a)::GPU} = (:gpu, T)
two(a, b) = :neither
@guarded two(a, b) where {backend(a)::GPU} = :first
@guarded two(a, b) where {backend(b)::GPU} = :second
@guarded two(a, b) where {backend(a)::GPU, backend(b)::GPU} = :both
struct Callable end
(::Callable)(a) = :cpu
@guarded (::Callable)(a) where {backend(a)::GPU} = :gpu
abstract type Shape end
struct Square <: Shape end
struct Circle <: Shape end
@trait backend(Circle) = Dev
(T::Type{<:Shape})(::Val{:made}) = :cpu
@guarded (T::Type{<:Shape})(::Val{:made}; n=1) where {backend(T)::GPU} = (:gpu, n)
@guarded @generated gen(a::AbstractArray{T}) where {T, backend(a)::GPU} = :(($T, :gpu))
"a guarded method"
@guarded documented(a::AbstractArray) where {backend(a)::GPU} = :gpu

@testset "@guarded" begin
    for (a, r) in Any[x0 => :cpu, d0 => :gpu, view(d0, :, 1) => :gpu]
        @test kw(a) === (r, 1)
        @test kw(a; scale=2) === (r, 2)
        @test kw_static(a) === (r, 5)
        @test opt(a) === (r, 1)
        @test opt(a, 2) === (r, 2)
        @test nsp(a) === nsp(Base.inferencebarrier(a)) === r
        @test Callable()(a) === r
    end
    @test Square(Val(:made)) === :cpu
    @test Circle(Val(:made)) === (:gpu, 1)
    @test Circle(Val(:made); n=2) === (:gpu, 2)
    @test rett(vec(d0)) === (:gpu, Float64)
    @test_throws MethodError rett(x0)
    @test gen(d0) === (Float64, :gpu)
    @test_throws MethodError gen(x0)
    @test occursin("a guarded method", string(@doc documented))
    @test occursin("documented(::AbstractArray) where {backend(a)::$GPU}",
                   sprint(showerror, try documented(x0); catch e; e; end))
    @test two(x0, x0) === :neither
    @test two(d0, x0) === :first
    @test two(x0, d0) === :second
    @test two(d0, vec(d0)) === :both
    @test only(methods(kw, (typeof(d0),))).guards == Core.svec(1, backend, GPU)
    @test only(methods(Core.kwcall, (NamedTuple, typeof(kw), typeof(d0)))).guards == Core.svec(3, backend, GPU)

    notatrait(::Type) = Val(false)
    @test_throws MethodError @eval @guarded nt(x) where {$notatrait(x)::Val{true}} = 1
    @test_throws TypeError @eval @guarded nb(x) where {backend(x)::1} = 1
    @test_throws "not an argument" @eval @guarded na(x::Type{T}) where {T, backend(T)::GPU} = 1
    @test_throws "one guard per argument and trait" @eval @guarded dup(x) where {backend(x)::GPU, backend(x)::Dev} = 1
    @test_throws "Vararg" @eval @guarded va(x...) where {backend(x)::GPU} = 1
    @test_throws "Vararg" @eval @guarded va(x::Vararg{Int}) where {backend(x)::GPU} = 1
    @test_throws "requires a guard" @eval @guarded ng(x) where {T} = 1
    @test_throws UndefVarError @eval @guarded failing(x::NoSuchType) where {backend(x)::GPU} = 1
end

# --- worlds and invalidation ------------------------------------------------------------

wf(x) = :cpu
const W_BEFORE = Base.get_world_counter()
@trait wtrait::Backend = CPU
@trait wtrait(Int) = Dev
@guarded wf(x) where {wtrait(x)::GPU} = :gpu

@testset "old worlds" begin
    @test wf(1) === :gpu
    @test Base.invoke_in_world(W_BEFORE, wf, 1) === :cpu
    @test wf(1.0) === :cpu
end

# a guarded method only invalidates the callers it can apply to
pf(x) = :cpu
callpf(x) = pf(x)
@test callpf(1) === callpf(1.0) === callpf("a") === :cpu
@guarded pf(x::Number) where {wtrait(x)::GPU} = :gpu

@testset "first guarded method" begin
    @test callpf(1) === :gpu
    @test callpf(1.0) === callpf("a") === :cpu
    @test isvalid_code(callpf, Float64) && isvalid_code(callpf, String)
end

# a method with the same signature and guards replaces the old one; as for any method,
# deleting it brings back the one it replaced
@guarded pf(x::Number) where {wtrait(x)::GPU} = :gpu2
@testset "replacing a guarded method" begin
    @test callpf(1) === :gpu2
    @test length(methods(pf)) == 2
end
Base.delete_method(only(m for m in methods(pf) if m.guards !== nothing))
@testset "deleting a guarded method" begin
    @test callpf(1) === pf(Base.inferencebarrier(1)) === :gpu
end
Base.delete_method(only(m for m in methods(pf) if m.guards !== nothing))
@testset "deleting the guarded methods" begin
    @test callpf(1) === pf(Base.inferencebarrier(1)) === :cpu
end
@guarded pf(x::Number) where {wtrait(x)::GPU} = :gpu

# as does an unguarded method of a function with guarded methods
pf(x::String) = :string
@testset "adding an unguarded method" begin
    @test callpf("a") === :string
    @test callpf(1) === :gpu
    @test isvalid_code(callpf, Float64) && isvalid_code(callpf, Int)
end
Base.delete_method(which(pf, (String,)))
@testset "deleting an unguarded method" begin
    @test callpf("a") === :cpu
end

# Selections do not outlive a change of the table. An entry for a type family that no
# lookup has seen yet invalidates nothing.
ef(x) = :cpu
@trait etrait::Backend = CPU
@guarded ef(x) where {etrait(x)::GPU} = :gpu
callef(x) = ef(x)
struct Seen end
struct Unseen end
struct EBox{T} end
@test callef(1) === callef(Seen()) === :cpu
@test ef(Base.inferencebarrier(1)) === :cpu
const W_E = Base.get_world_counter()
@trait etrait(Unseen) = Dev

@testset "entry for an unseen type family" begin
    @test isvalid_code(callef, Int) && isvalid_code(callef, Seen)
    @test callef(Unseen()) === :gpu
end

@trait etrait(Int) = Dev

@testset "adding an entry" begin
    @test callef(1) === :gpu
    @test callef(Seen()) === :cpu
    @test Base.invoke_in_world(W_E, callef, 1) === :cpu
    @test Base.invoke_in_world(W_E, ef, 1) === :cpu
    @test ef(Base.inferencebarrier(1)) === :gpu
end

@trait etrait(Int) = CPU
@trait etrait(EBox{T}) = etrait(T)
const W_E2 = Base.get_world_counter()

@testset "replacing an entry" begin
    @test callef(1) === :cpu
    @test callef(EBox{Int}()) === :cpu
    @test Base.invoke_in_world(W_E, callef, 1) === :cpu
end

@trait etrait(Int) = Dev

@testset "replacing a delegated entry" begin
    @test callef(1) === :gpu
    @test callef(EBox{Int}()) === ef(Base.inferencebarrier(EBox{Int}())) === :gpu
    @test Base.invoke_in_world(W_E2, callef, EBox{Int}()) === :cpu
end

Base.Experimental.delete_trait_entry(@__MODULE__, etrait, Int)

@testset "deleting an entry" begin
    @test callef(1) === :cpu
    @test ef(Base.inferencebarrier(1)) === :cpu
    @test callef(EBox{Int}()) === :cpu
end

# an entry can change which of two methods is more specific
sf(x) = :any
@guarded sf(x) where {etrait(x)::GPU} = :gpu
sf(x::Seen) = :seen
callsf(x) = sf(x)
@test callsf(Seen()) === :seen
@trait etrait(Seen) = Dev
@testset "entries and specificity" begin
    @test callsf(Seen()) === sf(Base.inferencebarrier(Seen())) === :seen
    @test Base.morespecific(which(sf, (Seen,)), which(sf, (Int,)))
end

# call methods are inherited, so a change of the table also affects the callers of the
# unguarded method of a supertype
abstract type AbstractCallable end
struct SubCallable{N} <: AbstractCallable end
(::AbstractCallable)(x) = :cpu
@trait ctrait::Backend = CPU
@guarded (::SubCallable)(x) where {ctrait(x)::GPU} = :gpu
callit(f, x) = f(x)
@test callit(SubCallable{0}(), 1) === :cpu
@trait ctrait(Int) = Dev
@testset "inherited call methods" begin
    @test callit(SubCallable{0}(), 1) === callit(SubCallable{1}(), 1) === :gpu
end

# entries on an abstract type family apply to its subtypes
abstract type AbstractDev{T,N} <: AbstractArray{T,N} end
struct Dev2{T,N} <: AbstractDev{T,N} end
Base.size(::Dev2) = (0,)
@trait backend(AbstractDev) = Dev

@testset "inherited entries" begin
    @test backend(Dev2{Int,1}) === region(backend, AbstractDev) === Dev
    @test kind(Dev2{Int,1}()) === kind(view(Dev2{Int,1}(), 1:0)) === :gpu
    @test only(code_typed(kind_static, (SubArray{Int,1,P,Tuple{UnitRange{Int}},true} where P<:AbstractDev,)))[1].code[end] ==
          Core.ReturnNode(QuoteNode(:gpu))
end

# --- dispatch details -------------------------------------------------------------------

# A dispatch cache entry is only widened where no guard distinguishes the calls it covers.
@trait vtrait::Backend = CPU
@trait vtrait(Val{X}) = vtrait(X)
@trait vtrait(DevArray) = Dev
wid(@nospecialize(x)) = :generic
wid(@nospecialize(x::Int)) = :int
@guarded wid(@nospecialize(x::Val)) where {vtrait(x)::GPU} = :gpu

@testset "dispatch cache" begin
    for _ = 1:2, (v, r) in Any[(1.0, :generic), (1, :int), (Val(1), :generic), (Val(DevArray), :gpu),
                               (Val(2), :generic), (Val(DevArray{Int,1}), :gpu), (Val(Array), :generic), ("a", :generic)]
        @test wid(Base.inferencebarrier(v)) === r
    end
end

# a static parameter that the compilable signature leaves undetermined is bound per call
struct SparamTag end
@trait sparam_trait::Backend = CPU
sp(::SparamTag, ::Union{Int, NTuple{N,Int}}...) where {N} = N
@guarded sp(t::SparamTag, ::Union{Int, NTuple{N,Int}}...) where {N, sparam_trait(t)::GPU} = -N

@testset "static parameters of widened signatures" begin
    @test sp(SparamTag(), 1, 2, 3, 4, 5, 6, (7, 8)) === 2
    @test_throws UndefVarError(:N, :static_parameter) sp(SparamTag(), 1, 2, 3, 4, 5, 6, 7)
end

# --- declarations -----------------------------------------------------------------------

module OtherOwner
using Base.Experimental: @trait
abstract type Kind end
struct K1 <: Kind end
struct K2 <: Kind end
@trait okind::Kind = K1
struct Own{T} end
@trait okind(Own{T}) = okind(T)
end
struct NotSealed{T} end

@testset "declarations" begin
    M = OtherOwner
    @test M.okind(M.Own{Int}) === M.K1
    # any module can declare an entry, and declaring one again does nothing,
    @eval @trait OtherOwner.okind(Some) = OtherOwner.K2
    @eval M @trait okind(Some) = K2
    @eval @trait OtherOwner.okind(OtherOwner.Own{T}) = OtherOwner.okind(T)
    @test M.okind(Some{Int}) === M.K2
    # but only the module that declared an entry replaces or deletes it
    @test_throws "another module" @eval @trait OtherOwner.okind(OtherOwner.Own) = OtherOwner.K2
    @test_throws "another module" @eval OtherOwner @trait okind(Some) = K1
    @test_throws "another module" Base.Experimental.delete_trait_entry(@__MODULE__, M.okind, M.Own)
    @test_throws "no entry" Base.Experimental.delete_trait_entry(@__MODULE__, backend, NotSealed)
    Base.Experimental.delete_trait_entry(@__MODULE__, M.okind, Some)
    @test M.okind(Some{Int}) === M.K1
    # an entry is for a whole type family, and gives a concrete label below the top
    @test_throws "whole type family" @eval @trait backend(Vector) = Dev
    @test_throws "whole type family" @eval @trait backend(Base.StridedReinterpretArray{T,N,A}) = backend(A)
    @test_throws "concrete subtype" @eval @trait backend(NotSealed) = GPU
    @test_throws "concrete subtype" @eval @trait backend(NotSealed) = OtherOwner.K1
    @test_throws "abstract type cannot delegate" @eval @trait backend(AbstractArray{T,N}) = backend(T)
    @test_throws "exactly one parameter" @eval @trait backend(NotSealed{T}) = backend(S)
    @test_throws ErrorException @eval @trait backend(Type) = Dev
    @test_throws "cannot have trait entries" @eval @trait backend(DataType) = Dev
    @test_throws "cannot have trait entries" @eval @trait backend(Tuple) = Dev
    # an entry on an abstract family seals its subtypes
    @test_throws "supertype has an entry" @eval @trait backend(Dev2) = CPU
    @test_throws "subtype has an entry" @eval @trait backend(AbstractArray) = CPU
    # declaring a trait
    @test_throws "not concrete" @eval @trait badtop::Dev = Dev
    @test_throws "concrete subtype" @eval @trait baddefault::Backend = Int
    @test_throws "concrete subtype" @eval @trait baddefault::Backend = GPU
end

# --- reflection -------------------------------------------------------------------------

module GuardReflection
using Base.Experimental: @guarded
import ..backend, ..GPU, ..CPU, ..DevArray
rf(a::AbstractArray) = :generic
@guarded rf(a::AbstractArray) where {backend(a)::GPU} = :gpu
rf(a::SubArray) = :view
rf(a::DevArray) = :dev
rf(a::Vector) = :vector
rg(a::AbstractArray) = :generic
@guarded rg(a::AbstractArray) where {backend(a)::GPU} = :gpu
rg(a::SubArray) = :view
@guarded rg(a::SubArray) where {backend(a)::GPU} = :gpu_view
@guarded rh(a::AbstractArray) where {backend(a)::GPU} = :gpu
@guarded rh(a::AbstractArray) where {backend(a)::CPU} = :cpu
@guarded ra(a::AbstractArray, b) where {backend(a)::GPU} = 1
@guarded ra(a, b::AbstractArray) where {backend(a)::GPU} = 2
cyc(::T, ::Vararg{T}) where {T<:Integer} = 1
cyc(::Integer, ::Vararg{String}) = 2
cyc(::Integer, ::Vararg{Union{Int,String}}) = 3
@guarded cyc(a::AbstractFloat) where {backend(a)::GPU} = 4
@guarded gcyc(x, y::DevArray) where {backend(y)::GPU} = :a
@guarded gcyc(x::Union{DevArray,String}, y::DevArray) where {backend(x)::GPU} = :b
gcyc(x::DevArray, y::Union{DevArray,String}) = :c
end

function guardedmethod(f, argtype, bound)
    return only(m for m in methods(f)
                if m.sig == Tuple{typeof(f), argtype} &&
                   (bound === nothing ? m.guards === nothing : (m.guards !== nothing && m.guards[3] === bound)))
end

@testset "isambiguous and detect_ambiguities" begin
    M = GuardReflection
    rf_gen = guardedmethod(M.rf, AbstractArray, nothing)
    rf_gpu = guardedmethod(M.rf, AbstractArray, GPU)
    rf_view = guardedmethod(M.rf, SubArray, nothing)
    @test !Base.isambiguous(rf_gpu, rf_gen)
    @test !Base.isambiguous(rf_gpu, rf_view)
    @test Base.morespecific(rf_gpu, rf_view) && !Base.morespecific(rf_view, rf_gpu)
    # every `DevArray` is on a device, no `Vector` is
    @test Base.morespecific(guardedmethod(M.rf, DevArray, nothing), rf_gpu)
    @test !Base.isambiguous(rf_gpu, guardedmethod(M.rf, DevArray, nothing))
    @test !Base.isambiguous(rf_gpu, guardedmethod(M.rf, Vector, nothing))
    @test !Base.isambiguous(guardedmethod(M.rh, AbstractArray, GPU), guardedmethod(M.rh, AbstractArray, CPU))
    @test Base.isambiguous(methods(M.ra)...)
    # a specificity cycle among unguarded methods of a function with guarded methods
    @test all(Base.isambiguous(m1, m2) for m1 in methods(M.cyc), m2 in methods(M.cyc)
              if m1 !== m2 && m1.guards === m2.guards === nothing)
    @test_throws MethodError M.cyc(3)
    # and one among guarded methods: gcyc's second method beats the first, which beats
    # the third, which beats the second
    @test_throws MethodError M.gcyc(d0, d0)
    @test any(p -> p[1].name === :gcyc, detect_ambiguities(M))
    @test Set(methods(M.ra)) in Set.(detect_ambiguities(M))
    @test all(p -> p[1].name in (:ra, :cyc, :gcyc), detect_ambiguities(M))
end

@testset "which, hasmethod and invoke" begin
    M = GuardReflection
    gv = view(d0, :, 1)
    @test which(M.rf, Tuple{typeof(vec(d0))}) === guardedmethod(M.rf, AbstractArray, GPU)
    @test which(M.rf, Tuple{typeof(d0)}) === guardedmethod(M.rf, DevArray, nothing)
    @test which(M.rg, Tuple{typeof(gv)}) === guardedmethod(M.rg, SubArray, GPU)
    @test which(M.rf, Tuple{typeof(gv)}) === guardedmethod(M.rf, AbstractArray, GPU)
    @test hasmethod(M.rh, Tuple{typeof(x0)})
    # abstract argument types: a guard does not hold for all of them
    @test which(M.rf, Tuple{AbstractArray}) === guardedmethod(M.rf, AbstractArray, nothing)
    @test !hasmethod(M.rh, Tuple{AbstractArray})
    # so `invoke` reaches the unguarded method of a wrapper, which a guarded one can forward to
    @test invoke(M.rg, Tuple{SubArray}, gv) === :view
    @test invoke(M.rf, Tuple{typeof(vec(d0))}, vec(d0)) === :gpu
    invoke_self(x) = invoke(M.rf, Tuple{typeof(x)}, x)
    @test invoke_self(vec(d0)) === :gpu
    @test invoke_self(x0) === :generic
end

# constructors: a missing method depends on the table too
module Ctors
struct C end
end
@trait ckind::Backend = CPU
@guarded Ctors.C(x::Int) where {ckind(x)::GPU} = Ctors.C()
hasctor() = hasmethod(Ctors.C, Tuple{Int})
callctor() = Ctors.C(1)
@test !hasctor()
@test_throws MethodError callctor()
@trait ckind(Int) = Dev
@testset "constructors" begin
    @test hasctor()
    @test callctor() === Ctors.C()
end

# a lookup folded by inference depends on the table
@trait tkind::Backend = CPU
@guarded tk(x::Int) where {tkind(x)::GPU} = :gpu
tk_hasmethod() = hasmethod(tk, Tuple{Int})
tk_invoke(x::Int) = invoke(tk, Tuple{Int}, x)
@test !tk_hasmethod()
@test_throws MethodError tk_invoke(1)
const W_TK = Base.get_world_counter()
@trait tkind(Int) = Dev

@testset "lookups after an entry is added" begin
    @test tk_hasmethod()
    @test tk_invoke(1) === :gpu
    @test !Base.invoke_in_world(W_TK, tk_hasmethod)
end

# --- overlay method tables --------------------------------------------------------------

Base.Experimental.@MethodTable GuardOverlay
@trait okind2::Backend = CPU
@trait okind2(Box{T}) = okind2(T)
@trait okind2(DevArray) = Dev
ovf(x) = "cpu"
@overlay GuardOverlay @guarded ovf(x) where {okind2(x)::GPU} = 1

@testset "overlay method tables" begin
    m = only(Base.MethodList(GuardOverlay).ms)
    @test m.guards == Core.svec(1, okind2, GPU)
    # an overlay method hides a native one only where its guards hold
    world = Base.get_world_counter()
    mt = Base.Compiler.OverlayMethodTable(world, GuardOverlay)
    for (T, native) in Any[Box{DevArray{Int,1}} => false, (Box{T} where T<:DevArray) => true, Box{Int} => true]
        ms = Base.Compiler.findall(Tuple{typeof(ovf), T}, mt).matches
        @test any(m -> m.method.guards === nothing, ms) == native
    end
    # and its callers are invalidated when the table changes
    mi = Base.specialize_method(m, Tuple{typeof(ovf), Box{Int}}, Core.svec())
    log = ccall(:jl_debug_method_invalidation, Any, (Cint,), 1)
    try
        @trait okind2(Int) = Dev
    finally
        ccall(:jl_debug_method_invalidation, Any, (Cint,), 0)
    end
    @test m in log
end

# --- packages ---------------------------------------------------------------------------

# Traits, their entries and guarded methods are saved in package images, including the
# entries and methods a package adds to the traits and functions of another one.
const package_test = raw"""
using Test
using TraitUser, TraitOwner
using TraitOwner: kind, scaled, backend, GPU, OwnerArray
using TraitUser: UserArray, UserWrapper
@test Base.JLOptions().use_compiled_modules == 1
o, u = OwnerArray([1.0, 2.0]), UserArray()
@testset "packages" begin
    @test kind(o) === kind(view(o, 1:1)) === kind(u) === :gpu
    @test kind(UserWrapper(o)) === kind(Base.inferencebarrier(UserWrapper(view(u, 1:0)))) === :gpu
    @test kind([1.0]) === kind(UserWrapper([1.0])) === :cpu
    @test scaled(o) === scaled(o; by=1) === (:gpu, 1)
    @test scaled(view(o, 1:1); by=3) === (:gpu, 3)
    @test scaled([1.0]; by=3) === (:cpu, 3)
    @test TraitUser.callkind(UserWrapper(o)) === :gpu
    @test length(u) == 0 && length(view(o, 1:2)) == -1 && length([1.0]) == 1
    @test backend(typeof(UserWrapper(u))) === GPU
end
# a package that repeats an entry does not declare it when it is loaded
@eval TraitOwner @trait backend(OwnerArray) = CPU
using TraitRepeat
@test backend(typeof(o)) === TraitOwner.CPU
"""

@testset "packages" begin
    mktempdir() do dir
        write(joinpath(dir, "TraitOwner.jl"), """
            module TraitOwner
            using Base.Experimental: @trait, @guarded
            abstract type Backend end
            struct CPU <: Backend end
            struct GPU <: Backend end
            struct OwnerArray{T,N} <: AbstractArray{T,N}
                data::Array{T,N}
            end
            Base.size(x::OwnerArray) = size(x.data)
            Base.getindex(x::OwnerArray, i::Int...) = x.data[i...]
            @trait backend::Backend = CPU
            @trait backend(OwnerArray) = GPU
            @trait backend(SubArray{T,N,P}) = backend(P)
            kind(x) = :cpu
            @guarded kind(x) where {backend(x)::GPU} = :gpu
            scaled(x; by=1) = (:cpu, by)
            @guarded scaled(x; by=1) where {backend(x)::GPU} = (:gpu, by)
            @guarded Base.length(x::SubArray) where {backend(x)::GPU} = -1
            kind(view(OwnerArray([1.0]), 1:1)); scaled(OwnerArray([1.0]); by=2)
            end
            """)
        write(joinpath(dir, "TraitUser.jl"), """
            module TraitUser
            using Base.Experimental: @trait, @guarded
            using TraitOwner: TraitOwner, backend, GPU, kind
            struct UserArray <: AbstractVector{Float64} end
            Base.size(::UserArray) = (0,)
            struct UserWrapper{P} <: AbstractVector{Float64}
                parent::P
            end
            Base.size(x::UserWrapper) = size(x.parent)
            @trait backend(UserArray) = GPU
            @trait backend(UserWrapper{P}) = backend(P)
            callkind(x) = kind(x)
            callkind(UserWrapper(UserArray())); callkind(UserWrapper([1.0]))
            end
            """)
        write(joinpath(dir, "TraitRepeat.jl"), """
            module TraitRepeat
            using Base.Experimental: @trait
            using TraitOwner: backend, GPU, OwnerArray
            @trait backend(OwnerArray) = GPU
            end
            """)
        cmd = `$(Base.julia_cmd()) --startup-file=no -e $package_test`
        cmd = addenv(cmd, "JULIA_LOAD_PATH" => dir * (Sys.iswindows() ? ";" : ":") * "@stdlib",
                     "JULIA_DEPOT_PATH" => dir * (Sys.iswindows() ? ";" : ":"))
        @test success(pipeline(cmd; stdout, stderr))
    end
end

# --- Base integration -------------------------------------------------------------------

# A mock device array that disallows scalar indexing: guarded methods on Base's broadcast
# style and `copyto!` route any nesting of wrappers to it.
const gpu_example = raw"""
using Test, LinearAlgebra
using Base.Broadcast: Broadcasted
using Base.Experimental: @guarded, @trait

abstract type Backend end
struct CPU <: Backend end
struct GPU <: Backend end
struct MockGPUArray{T,N} <: AbstractArray{T,N}
    data::Array{T,N}
end
Base.size(x::MockGPUArray) = size(x.data)
Base.getindex(::MockGPUArray, ::Int...) = error("scalar indexing")
Base.setindex!(::MockGPUArray, v, ::Int...) = error("scalar indexing")

@trait backend::Backend = CPU
@trait backend(MockGPUArray) = GPU
@trait backend(SubArray{T,N,P}) = backend(P)
@trait backend(Base.ReshapedArray{T,N,P}) = backend(P)
@trait backend(Transpose{T,P}) = backend(P)

# what a kernel would see: the wrappers around the device buffer
dev(x) = x
dev(x::MockGPUArray) = x.data
dev(x::SubArray) = view(dev(parent(x)), x.indices...)
dev(x::Base.ReshapedArray) = reshape(dev(parent(x)), size(x))
dev(x::Transpose) = transpose(dev(parent(x)))
dev(bc::Broadcasted) = Broadcast.broadcasted(bc.f, map(dev, bc.args)...)

struct MockStyle{N} <: Broadcast.AbstractArrayStyle{N} end
MockStyle{M}(::Val{N}) where {M,N} = MockStyle{N}()
@guarded Base.Broadcast.BroadcastStyle(a::Type{A}) where {T, N, A<:AbstractArray{T,N}, backend(a)::GPU} = MockStyle{N}()
Base.similar(::Broadcasted{<:MockStyle}, ::Type{T}, dims) where {T} = MockGPUArray(Array{T}(undef, length.(dims)))
const KERNELS = Ref(0)
function Base.copyto!(dest::AbstractArray, bc::Broadcasted{<:MockStyle})
    KERNELS[] += 1
    copyto!(dev(dest), Broadcast.instantiate(dev(bc)))
    return dest
end
@guarded function Base.copyto!(dst::AbstractArray, src::AbstractArray) where {backend(dst)::GPU}
    KERNELS[] += 1
    copyto!(dev(dst), dev(src))
    return dst
end
@guarded function Base.sum(a::AbstractArray; dims=:) where {backend(a)::GPU}
    KERNELS[] += 1
    return sum(dev(a); dims)
end

x0 = [1.0 2.0; 3.0 4.0]
x = MockGPUArray(copy(x0))
@testset "broadcast over nested wrappers" begin
    for f in (a -> vec(transpose(a)) .+ 1, a -> view(a, 2:2, :) .* 2,
              a -> view(reshape(transpose(a), 4), 2:3) .+ view(vec(a), 1:2))
        k = KERNELS[]
        r = f(x)
        @test r isa MockGPUArray && KERNELS[] == k + 1
        @test r.data == f(x0)
    end
    k = KERNELS[]
    @test sum(transpose(view(x, :, 1))) == 4.0 && sum(vec(x); dims=1) == [10.0]
    @test KERNELS[] == k + 2
    copyto!(view(transpose(x), :, 1), [10.0, 20.0])
    @test x.data == [10.0 20.0; 3.0 4.0]
    @test view(x0, :, 1) .+ 1 isa Vector{Float64}
    @test sum(x0) == 10.0 && sum(x0; dims=1) == [4.0 6.0]
end
"""

@testset "Base integration" begin
    cmd = `$(Base.julia_cmd()) --startup-file=no -e $gpu_example`
    @test success(pipeline(cmd; stdout, stderr))
end
