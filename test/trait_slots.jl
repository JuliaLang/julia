# This file is a part of Julia. License is MIT: https://julialang.org/license

# Trait slots (doc/src/devdocs/trait-slots.md): a mock device array routes Base
# fallbacks to its own methods for any nesting of wrappers, through guarded
# methods on a `backend` trait.
#
# Definitions are at top level, so that each testset runs in a world that sees them.

using Test, LinearAlgebra
using Base.Broadcast: Broadcasted, BroadcastStyle, DefaultArrayStyle
using Base.Experimental: @guarded

# --- the trait -----------------------------------------------------------

abstract type AbstractBackend end
struct CPUBackend <: AbstractBackend end
struct MockGPUBackend <: AbstractBackend end

# a mock device array: scalar indexing is disallowed (like JLArray with allowscalar(false))
struct MockGPUArray{T,N} <: AbstractArray{T,N}
    data::Array{T,N}
end
const SCALAR_ACCESSES = Ref(0)
const KERNELS = Ref(0)
Base.size(x::MockGPUArray) = size(x.data)
function Base.getindex(::MockGPUArray, ::Int...)
    SCALAR_ACCESSES[] += 1
    error("scalar getindex of a MockGPUArray is disallowed")
end
function Base.setindex!(::MockGPUArray, v, ::Int...)
    SCALAR_ACCESSES[] += 1
    error("scalar setindex! of a MockGPUArray is disallowed")
end
Base.Array(x::MockGPUArray) = copy(x.data)
Base.show(io::IO, x::MockGPUArray) = print(io, "MockGPUArray(", x.data, ")")
Base.show(io::IO, ::MIME"text/plain", x::MockGPUArray) = show(io, x)

backend(::Type) = CPUBackend()
backend(::Type{Type{T}}) where {T} = backend(T)
backend(::Type{<:MockGPUArray}) = MockGPUBackend()
backend(::Type{<:SubArray{<:Any,<:Any,P}}) where {P} = backend(P)
backend(::Type{<:Base.ReshapedArray{<:Any,<:Any,P}}) where {P} = backend(P)
backend(::Type{<:Adjoint{<:Any,P}}) where {P} = backend(P)
backend(::Type{<:Transpose{<:Any,P}}) where {P} = backend(P)
backend(::Type{<:PermutedDimsArray{<:Any,<:Any,<:Any,<:Any,P}}) where {P} = backend(P)
backend(::Type{<:Base.ReinterpretArray{<:Any,<:Any,<:Any,P}}) where {P} = backend(P)

# rebuild a (nested) wrapper around the host buffer: what a real backend's
# kernel would see as device pointers
tohost(x) = x
tohost(x::MockGPUArray) = x.data
tohost(x::SubArray) = view(tohost(parent(x)), x.indices...)
tohost(x::Base.ReshapedArray) = reshape(tohost(parent(x)), size(x))
tohost(x::Adjoint) = adjoint(tohost(parent(x)))
tohost(x::Transpose) = transpose(tohost(parent(x)))
tohost(x::PermutedDimsArray{<:Any,<:Any,perm}) where {perm} = PermutedDimsArray(tohost(parent(x)), perm)
tohost(x::Base.ReinterpretArray{T}) where {T} = reinterpret(T, tohost(parent(x)))
tohost(bc::Broadcasted) = Broadcast.broadcasted(bc.f, map(tohost, bc.args)...)

# --- the broadcast style: ONE guarded method covers all wrapper nestings ----

struct MockStyle{N} <: Broadcast.AbstractArrayStyle{N} end
MockStyle{M}(::Val{N}) where {M,N} = MockStyle{N}()

@guarded Broadcast.BroadcastStyle(::Type{A}) where {T,N,A<:AbstractArray{T,N},backend(A)::MockGPUBackend} =
    MockStyle{N}()

Base.similar(bc::Broadcasted{MockStyle{N}}, ::Type{T}, dims) where {N,T} =
    MockGPUArray(Array{T}(undef, map(length, dims)))
function Base.copyto!(dest::AbstractArray, bc::Broadcasted{MockStyle{N}}) where {N}
    # the mock "kernel": a host loop over the parent buffers
    KERNELS[] += 1
    copyto!(tohost(dest), Broadcast.instantiate(tohost(bc)))
    return dest
end

const x0 = [1.0 2.0; 3.0 4.0]
const v0 = [1.0, 2.0, 3.0, 4.0]
gpu(a) = MockGPUArray(copy(a))

function launches(f)
    k = KERNELS[]
    r = f()
    return r, KERNELS[] - k
end

@testset "trait slots: broadcast over nested wrappers" begin
    x = gpu(x0)
    v = gpu(v0)
    SCALAR_ACCESSES[] = 0
    @test backend(typeof(vec(transpose(x)))) === MockGPUBackend()
    @test BroadcastStyle(typeof(x)) === MockStyle{2}()
    @test BroadcastStyle(typeof(vec(transpose(x)))) === MockStyle{1}()
    @test BroadcastStyle(typeof(view(x, 2:2, :))) === MockStyle{2}()
    @test BroadcastStyle(typeof(x0)) === DefaultArrayStyle{2}()
    @test BroadcastStyle(typeof(view(x0, :, 1))) === DefaultArrayStyle{1}()

    for (f, expected) in Any[
            (() -> vec(transpose(x)) .+ 1, vec(transpose(x0)) .+ 1),
            (() -> view(x, 2:2, :) .* 2, view(x0, 2:2, :) .* 2),
            (() -> view(x, 1:2, 1) .* 2, view(x0, 1:2, 1) .* 2),
            (() -> reshape(x', 1, 4) .+ reshape(x, 1, 4), reshape(x0', 1, 4) .+ reshape(x0, 1, 4)),
            (() -> reshape(v', 1, 4) .+ v', reshape(v0', 1, 4) .+ v0'),
            (() -> x' .+ x, x0' .+ x0),
            (() -> PermutedDimsArray(x, (2, 1)) .- x, PermutedDimsArray(x0, (2, 1)) .- x0),
            (() -> reinterpret(UInt64, x) .+ 0x1, reinterpret(UInt64, x0) .+ 0x1),
            (() -> view(reshape(transpose(x), 4), 2:3) .+ 1, view(reshape(transpose(x0), 4), 2:3) .+ 1),
            (() -> sin.(view(x', :, 2)) .+ view(v, 1:2)', sin.(view(x0', :, 2)) .+ view(v0, 1:2)'),
        ]
        r, n = launches(f)
        @test r isa MockGPUArray
        @test n == 1
        @test Array(r) == expected
    end
    # in-place broadcast into a wrapper of device memory goes through the
    # MockStyle path as well (needs `copyto!(::SubArray, ::Broadcasted)` to
    # be routed, which it is via the generic `copyto!(dest, bc)`)
    @test SCALAR_ACCESSES[] == 0
end

# --- more guarded fallbacks ---------------------------------------------------

const COPIES = Ref(0)
@guarded function Base.copyto!(dst::AbstractArray, src::AbstractArray) where {backend(dst)::MockGPUBackend}
    COPIES[] += 1
    copyto!(tohost(dst), tohost(src))
    return dst
end
@guarded Base.similar(a::AbstractArray, ::Type{T}, dims::Dims) where {T,backend(a)::MockGPUBackend} =
    MockGPUArray(Array{T}(undef, dims))

@testset "trait slots: guarded copyto! and similar" begin
    x = gpu(x0)
    SCALAR_ACCESSES[] = 0
    c = COPIES[]
    copyto!(view(x, :, 1), [10.0, 30.0])
    @test COPIES[] == c + 1
    @test x.data == [10.0 2.0; 30.0 4.0]
    copyto!(transpose(x), [1.0 3.0; 2.0 4.0])
    @test x.data == x0
    copyto!(x, x0 .* 2)
    @test x.data == x0 .* 2
    # CPU arrays and wrappers of them still take the ordinary path
    y = copy(x0)
    copyto!(view(y, :, 1), [5.0, 6.0])
    @test COPIES[] == c + 3
    @test y == [5.0 2.0; 6.0 4.0]

    s = similar(x, Float32, (3,))
    @test s isa MockGPUArray{Float32,1}
    @test similar(x) isa MockGPUArray{Float64,2}
    @test similar(x0, Float32, (3,)) isa Vector{Float32}
    @test SCALAR_ACCESSES[] == 0
end

# --- dynamic and static dispatch agree ----------------------------------------

style_dyn(@nospecialize(T)) = BroadcastStyle(T)
style_static(::Type{T}) where {T} = BroadcastStyle(T)
@noinline add1(a) = a .+ 1

@testset "trait slots: opaque calls select like typed calls" begin
    x = gpu(x0)
    for a in Any[x, view(x, :, 1), x', vec(transpose(x)), PermutedDimsArray(x, (2, 1)), x0, view(x0, 1, :)]
        T = typeof(a)
        @test style_dyn(Base.inferencebarrier(T)) === style_static(T) === BroadcastStyle(T)
        r1 = add1(Base.inferencebarrier(a))
        r2 = add1(a)
        @test typeof(r1) === typeof(r2)
        @test Array(r1) == Array(r2)
    end
    # with an unknown slot the guarded method stays a candidate: no static choice
    rts = Base.return_types(style_static, (Type{<:AbstractMatrix},))
    @test only(rts) != DefaultArrayStyle{2}
end

# --- adding a method to the trait changes the selection ------------------------

struct MyWrapper{T,N,P<:AbstractArray{T,N}} <: AbstractArray{T,N}
    parent::P
end
Base.size(w::MyWrapper) = size(w.parent)
Base.parent(w::MyWrapper) = w.parent
Base.getindex(w::MyWrapper, i::Int...) = w.parent[i...]
tohost(w::MyWrapper) = MyWrapper(tohost(w.parent))

const W = MyWrapper(gpu(x0))

@testset "trait slots: before teaching backend about MyWrapper" begin
    @test style_static(typeof(W)) === DefaultArrayStyle{2}()  # warms the compiled code
    @test style_dyn(typeof(W)) === DefaultArrayStyle{2}()
    # the default broadcast path falls back to scalar indexing
    @test_throws ErrorException add1(W)
end

backend(::Type{<:MyWrapper{<:Any,<:Any,P}}) where {P} = backend(P)

@testset "trait slots: after teaching backend about MyWrapper" begin
    @test style_static(typeof(W)) === MockStyle{2}()
    @test style_dyn(typeof(W)) === MockStyle{2}()
    r, n = launches(() -> add1(W))
    @test r isa MockGPUArray && n == 1
    @test Array(r) == x0 .+ 1
    @test similar(W, Int, (2,)) isa MockGPUArray{Int,1}
end

# --- first guarded method after an ordinary call was compiled ------------------

kind(a::AbstractArray) = :generic
callkind(a) = kind(a)

@testset "trait slots: before the first guarded method of `kind`" begin
    @test callkind(gpu(x0)) === :generic
    @test callkind(view(gpu(x0), :, 1)) === :generic
    @test callkind(x0) === :generic
end

@guarded kind(a::AbstractArray) where {backend(a)::MockGPUBackend} = :gpu

@testset "trait slots: after the first guarded method of `kind`" begin
    @test callkind(gpu(x0)) === :gpu
    @test callkind(view(gpu(x0), :, 1)) === :gpu
    @test callkind(Base.inferencebarrier(gpu(x0)')) === :gpu
    @test callkind(x0) === :generic
    @test length(methods(kind)) == 2
    @test occursin("guarded by {backend(a)::$(MockGPUBackend)}", string(methods(kind)))
end

# --- ambiguity between a wrapper method and a guarded fallback -----------------

# Base has `similar(V::SubArray, T::Type, dims::Dims)`: unguarded on a specific
# wrapper, so it is ambiguous with the guarded `similar(::AbstractArray, ...)`
# (product ordering: (SubArray, Any) vs (AbstractArray, MockGPUBackend)).
@testset "trait slots: ambiguity" begin
    xv = view(gpu(x0), :, 1)
    err = try
        similar(xv, Float64, (2,))
        nothing
    catch e
        e
    end
    @test err isa MethodError
    msg = sprint(showerror, err)
    @test occursin("ambiguous", msg)
    @test occursin("SubArray", msg)
    @test occursin("guarded by {backend(a)::$(MockGPUBackend)}", msg)
    # CPU views are unaffected
    @test similar(view(x0, :, 1), Float64, (2,)) isa Vector{Float64}
end

# resolved by a guarded method on the specific wrapper
@guarded Base.similar(a::SubArray, ::Type{T}, dims::Dims) where {T,backend(a)::MockGPUBackend} =
    similar(parent(a), T, dims)

@testset "trait slots: ambiguity resolved" begin
    xv = view(gpu(x0), :, 1)
    @test similar(xv, Float64, (2,)) isa MockGPUArray{Float64,1}
    @test similar(view(x0, :, 1), Float64, (2,)) isa Vector{Float64}
end

# --- worlds: calls in a world before the trait and the guarded method existed ---

wf(x) = :cpu
const W_BEFORE_TRAIT = Base.get_world_counter()
wtrait(::Type) = CPUBackend()
wtrait(::Type{Int}) = MockGPUBackend()
@guarded wf(x) where {wtrait(x)::MockGPUBackend} = :gpu

@testset "trait slots: old-world calls ignore later slots" begin
    @test wf(1) === :gpu
    @test wf(1.0) === :cpu
    @test Base.invoke_in_world(W_BEFORE_TRAIT, wf, 1) === :cpu
    @test Base.invoke_in_world(W_BEFORE_TRAIT, wf, 1.0) === :cpu
    @test wf(1) === :gpu
end

# --- worlds: selections do not outlive a change of the trait --------------------

# the trait gains a method after the world was saved; call the latest world first
ef(x) = :cpu
etrait(::Type) = CPUBackend()
@guarded ef(x) where {etrait(x)::MockGPUBackend} = :gpu
callef(x) = ef(x)
const W_E = Base.get_world_counter()
etrait(::Type{Int}) = MockGPUBackend()

@testset "trait slots: latest world first, then an older world" begin
    @test ef(1) === :gpu
    @test callef(1) === :gpu
    @test Base.invoke_in_world(W_E, ef, 1) === :cpu
    @test Base.invoke_in_world(W_E, callef, 1) === :cpu
    @test ef(1) === :gpu
    @test ef(Base.inferencebarrier(1)) === :gpu
end

# same, with a fresh function, calling the older world first
gfx(x) = :cpu
gtrait(::Type) = CPUBackend()
@guarded gfx(x) where {gtrait(x)::MockGPUBackend} = :gpu
callgfx(x) = gfx(x)
const W_G = Base.get_world_counter()
gtrait(::Type{Int}) = MockGPUBackend()

@testset "trait slots: older world first, then the latest world" begin
    @test Base.invoke_in_world(W_G, gfx, 1) === :cpu
    @test Base.invoke_in_world(W_G, callgfx, 1) === :cpu
    @test gfx(1) === :gpu
    @test callgfx(1) === :gpu
    @test gfx(Base.inferencebarrier(1)) === :gpu
end

# deleting the trait method routes back to the fallback
Base.delete_method(which(gtrait, (Type{Int},)))

@testset "trait slots: deleting a trait method" begin
    @test gfx(1) === :cpu
    @test callgfx(1) === :cpu
    @test gfx(Base.inferencebarrier(1)) === :cpu
end

# --- unknown slots never let the unguarded method cover -------------------------

@testset "trait slots: matching with unknown slots ignores the dispatch cache" begin
    xg = gpu(x0)
    @test kind(xg) === :gpu     # warm the dispatch cache for this signature
    tt = Tuple{typeof(kind), typeof(xg)}
    w = Base.get_world_counter()
    function matches_with(slots)
        mn, mx, amb = Ref(typemin(UInt)), Ref(typemax(UInt)), Ref{Int32}(0)
        ccall(:jl_matching_methods_with_slots, Any,
              (Any, Any, Cint, Cint, UInt, Ptr{UInt}, Ptr{UInt}, Ptr{Int32}, Any),
              tt, nothing, -1, 0, w, mn, mx, amb, slots)::Vector{Any}
    end
    for slots in (nothing, Core.svec(1, backend, Any))
        ms = matches_with(slots)
        guarded_ms = filter(m -> m.method.guards !== nothing, ms)
        @test length(guarded_ms) == 1
        @test !only(guarded_ms).fully_covers
    end
    ms = matches_with(Core.svec(1, backend, MockGPUBackend))
    @test length(ms) == 1 && only(ms).method.guards !== nothing && only(ms).fully_covers
    ms = matches_with(Core.svec(1, backend, CPUBackend))
    @test length(ms) == 1 && only(ms).method.guards === nothing
end

# --- cache entries never cover a guarded method's region -----------------------

vtrait(::Type) = CPUBackend()
vtrait(::Type{Val{:gpu}}) = MockGPUBackend()
wid(@nospecialize(x)) = :generic
wid(@nospecialize(x::Int)) = :int
@guarded wid(@nospecialize(x::Val)) where {vtrait(x)::MockGPUBackend} = :gpu

@testset "trait slots: dispatch cache" begin
    for v in Any[1.0, "a", (1, 2), 1.0f0, :s]
        @test wid(Base.inferencebarrier(v)) === :generic
    end
    @test wid(Base.inferencebarrier(1)) === :int
    @test wid(Base.inferencebarrier(Val(1))) === :generic
    @test wid(Base.inferencebarrier(Val(:gpu))) === :gpu
    @test wid(Base.inferencebarrier(Val(2))) === :generic
    @test wid(Base.inferencebarrier(Val(:gpu))) === :gpu
    @test wid(Base.inferencebarrier(2.0)) === :generic
end

# --- a backend's own concrete method vs. a guarded fallback ----------------------

# (MockGPUArray, Any) and (AbstractArray, MockGPUBackend) are ambiguous: a
# signature never implies a guard.
bf(dst::AbstractArray, src::Array) = :generic
bf(dst::MockGPUArray, src::Array) = :backend
@guarded bf(dst::AbstractArray, src::Array) where {backend(dst)::MockGPUBackend} = :guarded

@testset "trait slots: backend method vs. guarded fallback is ambiguous" begin
    err = try
        bf(gpu(x0), x0)
        nothing
    catch e
        e
    end
    @test err isa MethodError
    msg = sprint(showerror, err)
    @test occursin("ambiguous", msg)
    @test occursin("bf(dst::$(MockGPUArray), src::Array)", msg)
    @test occursin("guarded by {backend(dst)::$(MockGPUBackend)}", msg)
    @test bf(view(gpu(x0), :, 1), x0) === :guarded
    @test bf(x0, x0) === :generic
end

# a static parameter that the compilable signature leaves undetermined (`N`,
# widened away with the `Vararg`) is still bound for each call
struct SparamTag end
sparam_trait(::Type) = nothing
sp47476(::SparamTag, ::Union{Int, NTuple{N,Int}}...) where {N} = N
@guarded sp47476(t::SparamTag, ::Union{Int, NTuple{N,Int}}...) where {N, sparam_trait(t)::SparamTag} = -N

@testset "trait slots: static parameters of widened signatures" begin
    @test sp47476(SparamTag(), 1, 2, 3, 4, 5, 6, (7, 8)) === 2
    @test sp47476(SparamTag(), 1, 2, 3, 4, 5, 6, (7, 8)) === 2
    @test_throws UndefVarError(:N, :static_parameter) sp47476(SparamTag(), 1, 2, 3, 4, 5, 6, 7)
end

# --- `@guarded` itself -------------------------------------------------------------

dtrait(::Type) = CPUBackend()
@testset "trait slots: @guarded errors" begin
    @test_throws "only once" @eval @guarded dup(x) where {dtrait(x)::CPUBackend, dtrait(x)::AbstractBackend} = 1
    @test_throws "Vararg" @eval @guarded va(x...) where {dtrait(x)::CPUBackend} = 1
    # a definition that fails leaves no guards behind for the next one
    @test_throws UndefVarError @eval @guarded failing(x::NoSuchType) where {dtrait(x)::CPUBackend} = 1
    @eval failing(x) = :plain
    @test only(methods(failing)).guards === nothing
    @test failing(1) === :plain
    # a definition nested in another's signature cannot take the outer guards
    @test_throws "still pending" @eval @guarded outer(x::(@eval(@guarded inner(y) where {dtrait(y)::CPUBackend} = 1); Int)) where {dtrait(x)::CPUBackend} = 1
    @test !isdefined(@__MODULE__, :outer) || isempty(methods(outer))
    # nor clear them when it fails
    @eval @guarded outer2(x::(try; @eval(@guarded inner2(y) where {dtrait(y)::CPUBackend} = 1); catch; end; Int)) where {dtrait(x)::MockGPUBackend} = 1
    @test only(methods(outer2)).guards !== nothing
    @test_throws MethodError outer2(1)
end

# --- inference ----------------------------------------------------------------------

@testset "trait slots: inference resolves guarded calls" begin
    x = gpu(x0)
    # inference resolves the guarded method statically when the slot is known
    T = typeof(view(x, :, 1))
    @test Base.return_types(style_static, (Type{T},)) == Any[MockStyle{1}]
    @test Base.return_types(style_static, (Type{typeof(view(x0, :, 1))},)) == Any[DefaultArrayStyle{1}]
    @test Base.return_types(add1, (T,)) == Any[MockGPUArray{Float64,1}]
end

# --- trait dependencies: helpers and precise invalidation ------------------------

# a trait whose value comes from a helper function
residency_helper(::Type) = CPUBackend()
residency(::Type{T}) where {T} = residency_helper(T)

rk(a::AbstractArray) = :host
@guarded rk(a::AbstractArray) where {residency(a)::MockGPUBackend} = :device
rk_static(a::Matrix{Float64}) = rk(a)
rk_static(a::Vector{Int}) = rk(a)
rk_dynamic(a) = rk(Base.inferencebarrier(a))

struct UnrelatedArray <: AbstractArray{Int,1} end
Base.size(::UnrelatedArray) = (0,)

function codeinstance(f, argtype)
    mi = only(Base.specializations(which(f, Tuple{argtype})))
    return mi.cache
end

@testset "trait slots: before changing the trait's helper" begin
    @test rk_static(x0) === :host
    @test rk_dynamic(x0) === :host
    @test rk_static([1, 2]) === :host
end

# changes the value of `residency` without touching `residency` itself
residency_helper(::Type{Matrix{Float64}}) = MockGPUBackend()

@testset "trait slots: after changing the trait's helper" begin
    @test rk_static(x0) === :device
    @test rk_dynamic(x0) === :device
    @test rk_static([1, 2]) === :host
end

const rk_int_ci = codeinstance(rk_static, Vector{Int})

# a new trait method that cannot change the value for the callers above
residency(::Type{UnrelatedArray}) = CPUBackend()

@testset "trait slots: a new trait method invalidates only its dependents" begin
    # compiled code that relied on another value of the trait stays valid
    @test rk_int_ci.max_world == typemax(UInt)
    @test rk_static([1, 2]) === :host
    @test rk_dynamic([1, 2]) === :host
    @test rk_static(x0) === :device
end

# a trait that is not inferred: no edge records what its value depends on, so
# calls stay dynamic
module UninferredTrait
import ..CPUBackend
Base.Experimental.@compiler_options compile=min infer=false
uninferred_helper(::Type) = CPUBackend()
uninferred_residency(::Type{T}) where {T} = uninferred_helper(T)
end
using .UninferredTrait: uninferred_residency

uk(a::AbstractArray) = :host
@guarded uk(a::AbstractArray) where {uninferred_residency(a)::MockGPUBackend} = :device
uk_static(a::Matrix{Float64}) = uk(a)
uk_dynamic(a) = uk(Base.inferencebarrier(a))

@testset "trait slots: before changing an uninferred trait's helper" begin
    @test uk_static(x0) === :host
    @test uk_dynamic(x0) === :host
end

UninferredTrait.uninferred_helper(::Type{Matrix{Float64}}) = MockGPUBackend()

@testset "trait slots: after changing an uninferred trait's helper" begin
    @test uk_static(x0) === :device
    @test uk_dynamic(x0) === :device
end

# an untracked trait value is not reused for an older world
const uk_world = Base.get_world_counter()
UninferredTrait.uninferred_helper(::Type{Vector{Float64}}) = MockGPUBackend()

@testset "trait slots: untracked trait values stay in their world" begin
    @test uk(v0) === :device
    @test Base.invoke_in_world(uk_world, uk, v0) === :host
end

# code inferred in an older world does not carry its trait values forward
residency2_helper(::Type) = CPUBackend()
residency2(::Type{T}) where {T} = residency2_helper(T)
ck(a::AbstractArray) = :host
@guarded ck(a::AbstractArray) where {residency2(a)::MockGPUBackend} = :device
ck_static(a::Vector{Float64}) = ck(a)
const ck_world = Base.get_world_counter()
residency2_helper(::Type{Vector{Float64}}) = MockGPUBackend()

@testset "trait slots: inference in an older world" begin
    @test Base.invoke_in_world(ck_world, ck_static, v0) === :host
    @test ck_static(v0) === :device
end


# --- reflection follows the guards ----------------------------------------------

module GuardReflection
using Base.Experimental: @guarded
import ..backend, ..MockGPUBackend, ..CPUBackend
# a wrapper method ambiguous with the guarded fallback
rf(a::AbstractArray) = :generic
@guarded rf(a::AbstractArray) where {backend(a)::MockGPUBackend} = :gpu
rf(a::SubArray) = :view
# the same, resolved by a tie-breaker
rg(a::AbstractArray) = :generic
@guarded rg(a::AbstractArray) where {backend(a)::MockGPUBackend} = :gpu
rg(a::SubArray) = :view
@guarded rg(a::SubArray) where {backend(a)::MockGPUBackend} = :gpu_view
# disjoint guards never apply to the same call
@guarded rh(a::AbstractArray) where {backend(a)::MockGPUBackend} = :gpu
@guarded rh(a::AbstractArray) where {backend(a)::CPUBackend} = :cpu
end

function guardedmethod(f, argtype, bound)
    return only(m for m in methods(f)
                if m.sig == Tuple{typeof(f), argtype} &&
                   (bound === nothing ? m.guards === nothing : (m.guards !== nothing && m.guards[3] === bound)))
end

@testset "trait slots: isambiguous and detect_ambiguities" begin
    M = GuardReflection
    rf_gen = guardedmethod(M.rf, AbstractArray, nothing)
    rf_gpu = guardedmethod(M.rf, AbstractArray, MockGPUBackend)
    rf_view = guardedmethod(M.rf, SubArray, nothing)
    @test !Base.isambiguous(rf_gpu, rf_gen)
    @test !Base.isambiguous(rf_view, rf_gen)
    @test Base.isambiguous(rf_gpu, rf_view)
    @test !Base.isambiguous(guardedmethod(M.rg, AbstractArray, MockGPUBackend), guardedmethod(M.rg, SubArray, nothing))
    @test !Base.isambiguous(guardedmethod(M.rg, SubArray, MockGPUBackend), guardedmethod(M.rg, AbstractArray, MockGPUBackend))
    @test !Base.isambiguous(guardedmethod(M.rh, AbstractArray, MockGPUBackend), guardedmethod(M.rh, AbstractArray, CPUBackend))
    ambs = detect_ambiguities(M)
    @test length(ambs) == 1
    @test Set(only(ambs)) == Set((rf_gpu, rf_view))
end

@testset "trait slots: which, hasmethod and invoke" begin
    M = GuardReflection
    g = gpu(x0)
    gv = view(g, :, 1)
    # concrete argument types: the trait is evaluated, as for a call
    @test which(M.rf, Tuple{typeof(g)}) === guardedmethod(M.rf, AbstractArray, MockGPUBackend)
    @test which(M.rf, Tuple{typeof(x0)}) === guardedmethod(M.rf, AbstractArray, nothing)
    @test which(M.rg, Tuple{typeof(gv)}) === guardedmethod(M.rg, SubArray, MockGPUBackend)
    @test_throws ErrorException which(M.rf, Tuple{typeof(gv)})
    @test hasmethod(M.rh, Tuple{typeof(x0)})
    # abstract argument types: a guard is not proven over all of them
    @test which(M.rf, Tuple{AbstractArray}) === guardedmethod(M.rf, AbstractArray, nothing)
    @test !hasmethod(M.rh, Tuple{AbstractArray})
    # `invoke` selects among the methods covering the given types, so it reaches
    # the unguarded method of a wrapper (as a tie-breaker may want to)
    @test invoke(M.rg, Tuple{SubArray}, gv) === :view
    @test invoke(M.rf, Tuple{AbstractArray}, g) === :generic
    @test invoke(M.rf, Tuple{typeof(g)}, g) === :gpu
    @test invoke(M.rg, guardedmethod(M.rg, SubArray, nothing), gv) === :view
    # the same when the `invoke` is resolved by inference
    invoke_self(x) = invoke(M.rf, Tuple{typeof(x)}, x)
    @test invoke_self(g) === :gpu
    @test invoke_self(x0) === :generic
end

# a tie-breaker whose guard covers only part of the ambiguous region does not
# resolve the ambiguity
module PartialTieBreaker
using Base.Experimental: @guarded
import ..AbstractBackend, ..CPUBackend
abstract type DeviceBackend <: AbstractBackend end
struct DeviceA <: DeviceBackend end
struct DeviceB <: DeviceBackend end
devkind(::Type) = CPUBackend()
devkind(::Type{Int}) = DeviceB()
@guarded pf(x::Number) where {devkind(x)::DeviceBackend} = :device
pf(x::Int) = :int
@guarded pf(x::Int) where {devkind(x)::DeviceA} = :int_device_a
end

@testset "trait slots: a partial tie-breaker leaves the ambiguity" begin
    M = PartialTieBreaker
    @test_throws MethodError M.pf(1)
    ambs = detect_ambiguities(M)
    @test length(ambs) == 1
    @test Set(only(ambs)) == Set((guardedmethod(M.pf, Number, M.DeviceBackend), guardedmethod(M.pf, Int, nothing)))
end

# tie-breakers for each member of a `Union` guard resolve every call, but
# `isambiguous` does not split the Union and conservatively reports the pair
module UnionTieBreakers
using Base.Experimental: @guarded
import ..CPUBackend
import ..PartialTieBreaker: DeviceA, DeviceB
devkind(::Type) = CPUBackend()
devkind(::Type{Int}) = DeviceB()
@guarded uf(x::Number) where {devkind(x)::Union{DeviceA,DeviceB}} = :device
uf(x::Int) = :int
@guarded uf(x::Int) where {devkind(x)::DeviceA} = :int_a
@guarded uf(x::Int) where {devkind(x)::DeviceB} = :int_b
end

@testset "trait slots: tie-breakers per member of a Union guard" begin
    @test UnionTieBreakers.uf(1) === :int_b
    @test length(detect_ambiguities(UnionTieBreakers)) == 1
end

# a lookup folded by inference depends on the trait even when it threw
throwing_kind(::Type) = error("no kind yet")
@guarded tk(x::Int) where {throwing_kind(x)::MockGPUBackend} = :gpu
tk_hasmethod() = hasmethod(tk, Tuple{Int})
tk_invoke(x::Int) = invoke(tk, Tuple{Int}, x)

@testset "trait slots: lookups before the trait is defined" begin
    @test !hasmethod(tk, Tuple{Int})
    @test !tk_hasmethod()
    @test_throws MethodError tk_invoke(1)
end

throwing_kind(::Type{Int}) = MockGPUBackend()

@testset "trait slots: lookups after the trait is defined" begin
    @test hasmethod(tk, Tuple{Int})
    @test tk_hasmethod()
    @test tk_invoke(1) === :gpu
end
