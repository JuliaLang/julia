__precompile__()

module Compat

using Base.Meta

"""Get just the function part of a function declaration."""
withincurly(ex) = isexpr(ex, :curly) ? ex.args[1] : ex

if VERSION < v"0.6.0-dev.2043"
    Base.take!(t::Task) = consume(t)
end

function rewrite_show(ex)
    if isexpr(ex, :call)
        Expr(:call, rewrite_show(ex.args[1]), ex.args[2:end]...)
    elseif isexpr(ex, :curly)
        Expr(:curly, rewrite_show(ex.args[1]), ex.args[2:end]...)
    else
        :(Base.writemime)
    end
end

function rewrite_dict(ex)
    length(ex.args) == 1 && return ex

    f = ex.args[1]
    if isexpr(f, :curly)
        newex = Expr(:typed_dict, :($(f.args[2])=>$(f.args[3])))
    else
        newex = Expr(:dict)
    end

    for i = 2:length(ex.args)
        pair = ex.args[i]
        !isexpr(pair, :(=>)) && return ex
        push!(newex.args, pair)
    end
    newex
end

function rewrite_ordereddict(ex)
    length(ex.args) == 1 && return ex

    f = ex.args[1]
    newex = Expr(:call, f, :[])

    for i = 2:length(ex.args)
        pair = ex.args[i]
        !isexpr(pair, :(=>)) && return ex
        push!(newex.args[2].args, Expr(:tuple, pair.args...))
    end

    newex
end

# rewrites all subexpressions of the form `a => b` to `(a, b)`
function rewrite_pairs_to_tuples!(expr::Expr)
    if expr.head == :(=>)
        expr.head = :tuple
    end
    for subexpr in expr.args
        isa(subexpr, Expr) && rewrite_pairs_to_tuples!(subexpr)
    end
    return expr
end

function is_quote_symbol(ex::ANY, val::Symbol)
    if isa(ex, QuoteNode)
        return (ex::QuoteNode).value === val
    elseif isa(ex, Expr)
        ex = ex::Expr
        return ex.head === :quote && length(ex.args) == 1 && ex.args[1] === val
    end
    return false
end

is_index_style(ex::Expr) = ex == :(Compat.IndexStyle) || ex == :(Base.IndexStyle) ||
    (ex.head == :(.) && (ex.args[1] == :Compat || ex.args[1] == :Base) &&
         ex.args[2] == Expr(:quote, :IndexStyle))

is_index_style(arg) = false

# rewrites accesses to IOContext dicts
function rewrite_iocontext!(expr::Expr)
    args = expr.args
    nargs = length(args)
    if nargs == 4 && expr.head === :call && args[1] === :get && args[4] === false
        key = args[3]
        if is_quote_symbol(key, :limit) || is_quote_symbol(key, :compact)
            if VERSION >= v"0.5.0-dev+1936" && VERSION < v"0.5.0-dev+4305"
                args[1] = :(Base.limit_output)
                deleteat!(args, 3:4)
            elseif VERSION < v"0.5.0-dev+1936"
                expr.head = :quote
                args[1] = false
                deleteat!(args, 3:4)
            end
        elseif is_quote_symbol(key, :multiline)
            if VERSION < v"0.5.0-dev+4305"
                expr.head = :quote
                args[1] = false
                deleteat!(args, 3:4)
            end
        end
    end
end

# JuliaLang/julia#10543
if !isdefined(Base, :tryparse)
    function tryparse{T}(::Type{T}, args...)
        try
            Nullable(Base.parse(T, args...))
        catch
            Nullable{T}()
        end
    end
end

import Base.unsafe_convert

function new_style_call_overload(ex::Expr)
    Base.depwarn("new_style_call_overload is deprecated.", :new_style_call_overload)
    false
end

istopsymbol(ex, mod, sym) = ex in (sym, Expr(:(.), mod, Expr(:quote, sym)))

if VERSION < v"0.6.0-dev.2782"
    function new_style_typealias(ex::ANY)
        isexpr(ex, :(=)) || return false
        ex = ex::Expr
        return length(ex.args) == 2 && isexpr(ex.args[1], :curly)
    end
else
    new_style_typealias(ex) = false
end

function _compat(ex::Expr)
    if ex.head === :call
        f = ex.args[1]
        if VERSION < v"0.6.0-dev.826" && length(ex.args) == 3 && # julia#18510
                istopsymbol(withincurly(ex.args[1]), :Base, :Nullable)
            ex = Expr(:call, f, ex.args[2], Expr(:call, :(Compat._Nullable_field2), ex.args[3]))
        end
    elseif ex.head === :curly
        f = ex.args[1]
        if ex == :(Ptr{Void})
            # Do not change Ptr{Void} to Ptr{Nothing}: 0.4.0-dev+768
            return ex
        elseif VERSION < v"0.6.0-dev.2575" #20414
            ex = Expr(:curly, map(a -> isexpr(a, :call, 2) && a.args[1] == :(<:) ?
                                  :($TypeVar($(QuoteNode(gensym(:T))), $(a.args[2]), false)) :
                                  isexpr(a, :call, 2) && a.args[1] == :(>:) ?
                                  :($TypeVar($(QuoteNode(gensym(:T))), $(a.args[2]), $Any, false)) : a,
                                  ex.args)...)
        end
    elseif ex.head === :macrocall
        f = ex.args[1]
    elseif ex.head === :quote && isa(ex.args[1], Symbol)
        # Passthrough
        return ex
    elseif new_style_typealias(ex)
        ex.head = :typealias
    elseif ex.head === :const && length(ex.args) == 1 && new_style_typealias(ex.args[1])
        ex = ex.args[1]::Expr
        ex.head = :typealias
    end
    if VERSION < v"0.6.0-dev.2840"
        if ex.head == :(=) && isa(ex.args[1], Expr) && ex.args[1].head == :call
            a = ex.args[1].args[1]
            if is_index_style(a)
                ex.args[1].args[1] = :(Base.linearindexing)
            elseif isa(a, Expr) && a.head == :curly
                if is_index_style(a.args[1])
                    ex.args[1].args[1].args[1] = :(Base.linearindexing)
                end
            end
        end
    end
    if VERSION < v"0.7.0-DEV.880"
        if ex.head == :curly && ex.args[1] == :CartesianRange && length(ex.args) >= 2
            a = ex.args[2]
            if a != :CartesianIndex && !(isa(a, Expr) && a.head == :curly && a.args[1] == :CartesianIndex)
                return Expr(:curly, :CartesianRange, Expr(:curly, :CartesianIndex, ex.args[2]))
            end
        end
    end
    return Expr(ex.head, map(_compat, ex.args)...)
end

_compat(ex) = ex

function _get_typebody(ex::Expr)
    args = ex.args
    if ex.head !== :type || length(args) != 3 || args[1] !== true
        throw(ArgumentError("Invalid usage of @compat: $ex"))
    end
    name = args[2]
    if !isexpr(args[3], :block)
        throw(ArgumentError("Invalid type declaration: $ex"))
    end
    body = (args[3]::Expr).args
    filter!(body) do e
        if isa(e, LineNumberNode) || isexpr(e, :line)
            return false
        end
        return true
    end
    return name, body
end

function _compat_primitive(typedecl)
    name, body = _get_typebody(typedecl)
    if length(body) != 1
        throw(ArgumentError("Invalid primitive type declaration: $typedecl"))
    end
    return Expr(:bitstype, body[1], name)
end

function _compat_abstract(typedecl)
    name, body = _get_typebody(typedecl)
    if length(body) != 0
        throw(ArgumentError("Invalid abstract type declaration: $typedecl"))
    end
    return Expr(:abstract, name)
end

macro compat(ex...)
    if VERSION < v"0.6.0-dev.2746" && length(ex) == 2 && ex[1] === :primitive
        return esc(_compat_primitive(ex[2]))
    elseif length(ex) != 1
        throw(ArgumentError("@compat called with wrong number of arguments: $ex"))
    elseif (VERSION < v"0.6.0-dev.2746" && isexpr(ex[1], :abstract) &&
            length(ex[1].args) == 1 && isexpr(ex[1].args[1], :type))
        # This can in principle be handled in nested case but we do not
        # do that to be consistent with primitive types.
        return esc(_compat_abstract(ex[1].args[1]))
    end
    esc(_compat(ex[1]))
end

export @compat, @inline, @noinline

import Base.@irrational

import Base: remotecall, remotecall_fetch, remotecall_wait, remote_do

import Base.Filesystem

if !isdefined(Base, :istextmime)
    export istextmime
    istextmime(m::@compat(Union{MIME,AbstractString})) = istext(m)
end

function primarytype(t::ANY)
    tn = t.name
    if isdefined(tn, :primary)
        return tn.primary
    else
        return tn.wrapper
    end
end

if !isdefined(Base, :Threads)
    @eval module Threads
        macro threads(expr)
            return esc(expr)
        end
        threadid() = 1
        nthreads() = 1
        export @threads, threadid, nthreads
    end
    export Threads
end

if !isdefined(Base, :normalize)
    function normalize!(v::AbstractVector, p::Real=2)
        nrm = norm(v, p)
        __normalize!(v, nrm)
    end

    @inline function __normalize!(v::AbstractVector, nrm::AbstractFloat)
        #The largest positive floating point number whose inverse is less than
        #infinity
        δ = inv(prevfloat(typemax(nrm)))
        if nrm ≥ δ #Safe to multiply with inverse
            invnrm = inv(nrm)
            scale!(v, invnrm)
        else # scale elements to avoid overflow
            εδ = eps(one(nrm))/δ
            scale!(v, εδ)
            scale!(v, inv(nrm*εδ))
        end
        v
    end

    copy_oftype{T,N}(A::AbstractArray{T,N}, ::Type{T}) = copy(A)
    copy_oftype{T,N,S}(A::AbstractArray{T,N}, ::Type{S}) = convert(AbstractArray{S,N}, A)

    function normalize(v::AbstractVector, p::Real = 2)
        nrm = norm(v, p)
        if !isempty(v)
            vv = copy_oftype(v, typeof(v[1]/nrm))
            return __normalize!(vv, nrm)
        else
            T = typeof(zero(eltype(v))/nrm)
            return T[]
        end
    end

    export normalize, normalize!
end

import Base.AsyncCondition
import Base: srand, rand, rand!


if !isdefined(Base, :pointer_to_string)

    function pointer_to_string(p::Ptr{UInt8}, len::Integer, own::Bool=false)
        a = ccall(:jl_ptr_to_array_1d, Vector{UInt8},
                  (Any, Ptr{UInt8}, Csize_t, Cint), Vector{UInt8}, p, len, own)
        ccall(:jl_array_to_string, Ref{String}, (Any,), a)
    end

    pointer_to_string(p::Ptr{UInt8}, own::Bool=false) =
        pointer_to_string(p, ccall(:strlen, Csize_t, (Cstring,), p), own)

end

import Base.promote_eltype_op

import Base.LinAlg.BLAS.@blasfunc

import Base: redirect_stdin, redirect_stdout, redirect_stderr
if VERSION < v"0.6.0-dev.374"
    for (F,S) in ((:redirect_stdin, :STDIN), (:redirect_stdout, :STDOUT), (:redirect_stderr, :STDERR))
        @eval function $F(f::Function, stream)
            STDOLD = $S
            $F(stream)
            try f() finally $F(STDOLD) end
        end
    end
end

if VERSION < v"0.6.0-dev.528"
    macro __DIR__()
        Base.source_dir()
    end
    export @__DIR__
end

# PR #17302
# Provide a non-deprecated version of `@vectorize_(1|2)arg` macro which defines
# deprecated version of the function so that the depwarns can be fixed without
# breaking users.
# Packages are expected to use this to maintain the old API until all users
# of the deprecated vectorized function have migrated.
# These macros should raise a depwarn when the `0.5` support is dropped from
# `Compat` and be dropped when the support for `0.6` is dropped from `Compat`.
# Modified based on the version copied from 0.6 Base.
macro dep_vectorize_1arg(S, f)
    S = esc(S)
    f = esc(f)
    T = esc(:T)
    x = esc(:x)
    AbsArr = esc(:AbstractArray)
    ## Depwarn to be enabled when 0.5 support is dropped.
    # depwarn("Implicit vectorized function is deprecated in favor of compact broadcast syntax.",
    #         Symbol("@dep_vectorize_1arg"))
    :(@deprecate $f{$T<:$S}($x::$AbsArr{$T}) @compat($f.($x)))
end

macro dep_vectorize_2arg(S, f)
    S = esc(S)
    f = esc(f)
    T1 = esc(:T1)
    T2 = esc(:T2)
    x = esc(:x)
    y = esc(:y)
    AbsArr = esc(:AbstractArray)
    ## Depwarn to be enabled when 0.5 support is dropped.
    # depwarn("Implicit vectorized function is deprecated in favor of compact broadcast syntax.",
    #         Symbol("@dep_vectorize_2arg"))
    quote
        @deprecate $f{$T1<:$S}($x::$S, $y::$AbsArr{$T1}) @compat($f.($x,$y))
        @deprecate $f{$T1<:$S}($x::$AbsArr{$T1}, $y::$S) @compat($f.($x,$y))
        @deprecate $f{$T1<:$S,$T2<:$S}($x::$AbsArr{$T1}, $y::$AbsArr{$T2}) @compat($f.($x,$y))
    end
end

# broadcast over same length tuples, from julia#16986
if VERSION < v"0.6.0-dev.693"
    Base.Broadcast.broadcast{N}(f, t::NTuple{N}, ts::Vararg{NTuple{N}}) = map(f, t, ts...)
end

# julia#18510
if VERSION < v"0.6.0-dev.826"
    _Nullable_field2(x) = !x
else
    _Nullable_field2(x) = x
end

# julia#18484
if VERSION < v"0.6.0-dev.848"
    unsafe_get(x::Nullable) = x.value
    unsafe_get(x) = x
    export unsafe_get
    Base.isnull(x) = false
end

# julia#18977
if !isdefined(Base, :xor)
    const xor = $
    const ⊻ = xor
    export xor, ⊻
end

# julia#19246
if !isdefined(Base, :numerator)
    const numerator = num
    const denominator = den
    export numerator, denominator
end

# julia #19950
if !isdefined(Base, :iszero)
    iszero(x) = x == zero(x)
    iszero(x::Number) = x == 0
    iszero(x::AbstractArray) = all(iszero, x)
    export iszero
end

# julia　#20407
if !isdefined(Base, :(>:))
    const >: = let
        _issupertype(a::ANY, b::ANY) = issubtype(b, a)
    end
    export >:
end

# julia#19088
if VERSION < v"0.6.0-dev.1256"
    Base.take!(io::Base.AbstractIOBuffer) = takebuf_array(io)
end

# julia #17155 function composition and negation
if VERSION < v"0.6.0-dev.1883"
    export ∘
    ∘(f, g) = (x...)->f(g(x...))
    @compat Base.:!(f::Function) = (x...)->!f(x...)
end


if VERSION < v"0.6.0-dev.1632"
    # To work around unsupported syntax on Julia 0.4
    include_string("export .&, .|")
    include_string(".&(xs...) = broadcast(&, xs...)")
    include_string(".|(xs...) = broadcast(|, xs...)")
end

if VERSION < v"0.6.0-dev.2093" # Compat.isapprox to allow for NaNs
    using Base.rtoldefault
    function isapprox(x::Number, y::Number; rtol::Real=rtoldefault(x,y), atol::Real=0, nans::Bool=false)
        x == y || (isfinite(x) && isfinite(y) && abs(x-y) <= atol + rtol*max(abs(x), abs(y))) || (nans && isnan(x) && isnan(y))
    end
else
    import Base.isapprox
end

module TypeUtils
    @static if isdefined(Base, :isabstract)
        using Base: isabstract, parameter_upper_bound, typename
    else
        isabstract(t::DataType) = t.abstract
        if isdefined(Base, :TypeConstructor)
            isabstract(t::TypeConstructor) = isabstract(t.body)
        end
        isabstract(t::ANY) = false
        parameter_upper_bound(t::DataType, idx) = t.parameters[idx].ub
        typename(t::DataType) = t.name
    end
    export isabstract, parameter_upper_bound, typename
end # module TypeUtils

# @view, @views, @__dot__
include("arraymacros.jl")

# julia #18839
if VERSION < v"0.6.0-dev.1024"
    @eval module Iterators
        export countfrom, cycle, drop, enumerate, flatten, product, repeated,
               rest, take, zip, partition

        import Base: eltype, start, next, done, length, size, ndims
        using Base: tuple_type_cons
        using Base: countfrom, cycle, drop, enumerate, repeated, rest, take,
                    zip
        using Compat

        using Base: flatten
        using Base: product
        using Base: partition
    end
else
    using Base: Iterators
end

if VERSION < v"0.6.0-dev.2840"
    export IndexStyle, IndexLinear, IndexCartesian
    eval(Expr(:typealias, :IndexStyle, :(Base.LinearIndexing)))
    eval(Expr(:typealias, :IndexLinear, :(Base.LinearFast)))
    eval(Expr(:typealias, :IndexCartesian, :(Base.LinearSlow)))
    IndexStyle{T}(::Type{T}) = Base.linearindexing(T)
    IndexStyle(args...) = Base.linearindexing(args...)
end

if VERSION < v"0.6.0-dev.1653"
    for (fname, felt) in ((:zeros,:zero), (:ones,:one))
        @eval begin
            # allow signature of similar
            Base.$fname(a::AbstractArray, T::Type, dims::Tuple) = fill!(similar(a, T, dims), $felt(T))
            Base.$fname(a::AbstractArray, T::Type, dims...) = fill!(similar(a,T,dims...), $felt(T))
            Base.$fname(a::AbstractArray, T::Type=eltype(a)) = fill!(similar(a,T), $felt(T))
        end
    end
end

# https://github.com/JuliaLang/julia/pull/20203
if VERSION < v"0.6.0-dev.2283"
    # not exported
    function readline(s::IO=STDIN; chomp::Bool=true)
        if chomp
            Base.chomp!(Base.readline(s))
        else
            Base.readline(s)
        end
    end
end

# https://github.com/JuliaLang/julia/pull/18727
if VERSION < v"0.6.0-dev.838"
    Base.convert{T}(::Type{Set{T}}, s::Set{T}) = s
    Base.convert{T}(::Type{Set{T}}, s::Set) = Set{T}(s)
end

# https://github.com/JuliaLang/julia/pull/18082
if VERSION < v"0.6.0-dev.2347"
    Base.isassigned(x::Base.RefValue) = isdefined(x, :x)
end

if VERSION < v"0.6.0-dev.735"
    Base.unsafe_trunc{T<:Integer}(::Type{T}, x::Integer) = rem(x, T)
end

# https://github.com/JuliaLang/julia/pull/21346
if VERSION < v"0.6.0-pre.beta.102"
    Base.bswap(z::Complex) = Complex(bswap(real(z)), bswap(imag(z)))
end

# https://github.com/JuliaLang/julia/pull/19449
if VERSION < v"0.6.0-dev.1988"
    StringVector(n::Integer) = Vector{UInt8}(n)
else
    using Base: StringVector
end

# https://github.com/JuliaLang/julia/pull/22064
if !isdefined(Base, Symbol("@__MODULE__"))
    export @__MODULE__
    macro __MODULE__()
        return current_module()
    end
    Base.expand(mod::Module, x::ANY) = eval(mod, :(expand($(QuoteNode(x)))))
    Base.macroexpand(mod::Module, x::ANY) = eval(mod, :(macroexpand($(QuoteNode(x)))))
    Base.include_string(mod::Module, code::String, fname::String) =
        eval(mod, :(include_string($code, $fname)))
    Base.include_string(mod::Module, code::AbstractString, fname::AbstractString="string") =
        eval(mod, :(include_string($code, $fname)))
end

# https://github.com/JuliaLang/julia/pull/19784
if isdefined(Base, :invokelatest)
    import Base.invokelatest
else
    invokelatest(f, args...) = eval(current_module(), Expr(:call, f, map(QuoteNode, args)...))
end

# https://github.com/JuliaLang/julia/pull/21257
if VERSION < v"0.6.0-pre.beta.28"
    collect(A) = collect_indices(indices(A), A)
    collect_indices(::Tuple{}, A) = copy!(Array{eltype(A)}(), A)
    collect_indices(indsA::Tuple{Vararg{Base.OneTo}}, A) =
        copy!(Array{eltype(A)}(map(length, indsA)), A)
    function collect_indices(indsA, A)
        B = Array{eltype(A)}(map(length, indsA))
        copy!(B, CartesianRange(indices(B)), A, CartesianRange(indsA))
    end
else
    const collect = Base.collect
end

# https://github.com/JuliaLang/julia/pull/21197
if VERSION < v"0.7.0-DEV.257"
    # allow the elements of the Cmd to be accessed as an array or iterator
    for f in (:length, :endof, :start, :eachindex, :eltype, :first, :last)
        @eval Base.$f(cmd::Cmd) = $f(cmd.exec)
    end
    for f in (:next, :done, :getindex)
        @eval Base.$f(cmd::Cmd, i) = $f(cmd.exec, i)
    end
end

# https://github.com/JuliaLang/julia/pull/21378
if VERSION < v"0.6.0-pre.beta.455"
    import Base: ==, isless

    ==(x::Dates.Period, y::Dates.Period) = (==)(promote(x, y)...)
    isless(x::Dates.Period, y::Dates.Period) = isless(promote(x,y)...)

    # disallow comparing fixed to other periods
    ==(x::Dates.FixedPeriod, y::Dates.OtherPeriod) = throw(MethodError(==, (x, y)))
    ==(x::Dates.OtherPeriod, y::Dates.FixedPeriod) = throw(MethodError(==, (x, y)))
    isless(x::Dates.FixedPeriod, y::Dates.OtherPeriod) = throw(MethodError(isless, (x, y)))
    isless(x::Dates.OtherPeriod, y::Dates.FixedPeriod) = throw(MethodError(isless, (x, y)))
end

# https://github.com/JuliaLang/julia/pull/22475
if VERSION < v"0.7.0-DEV.843"
    import Base: Val
    (::Type{Val})(x) = (Base.@_pure_meta; Val{x}())
end

# https://github.com/JuliaLang/julia/pull/22629
if VERSION < v"0.7.0-DEV.848"
    import Base: logdet
    logdet(A) = log(det(A))
end

# https://github.com/JuliaLang/julia/pull/22633
if VERSION < v"0.7.0-DEV.1041"
    import Base.LinAlg: chol, chol!
    chol!(J::UniformScaling, uplo) = UniformScaling(chol!(J.λ, uplo))
    chol(J::UniformScaling, args...) = UniformScaling(chol(J.λ, args...))
end

include("deprecated.jl")

# https://github.com/JuliaLang/julia/pull/21746
const macros_have_sourceloc = VERSION >= v"0.7-" && length(:(@test).args) == 2

# https://github.com/JuliaLang/julia/pull/22182
module Sys
    if VERSION < v"0.7.0-DEV.914"
        isapple(k::Symbol=Base.Sys.KERNEL)   = k in (:Darwin, :Apple)
        isbsd(k::Symbol=Base.Sys.KERNEL)     = isapple(k) || k in (:FreeBSD, :OpenBSD, :NetBSD, :DragonFly)
        islinux(k::Symbol=Base.Sys.KERNEL)   = k == :Linux
        isunix(k::Symbol=Base.Sys.KERNEL)    = isbsd(k) || islinux(k)
        iswindows(k::Symbol=Base.Sys.KERNEL) = k in (:Windows, :NT)
    else
        import Base.Sys: isapple, isbsd, islinux, isunix, iswindows
    end
end

end # module Compat
