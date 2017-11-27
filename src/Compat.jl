__precompile__()

module Compat

using Base.Meta

@static if !isdefined(Base, Symbol("@nospecialize"))
    # 0.7
    macro nospecialize(arg)
        if isa(arg, Symbol)
            # @nospecialize(arg)
            return :($(esc(arg))::ANY)
        elseif isa(arg, Expr) && arg.head == :(::)
            # @nospecialize(arg::typ)
            # unsupported: needs ::ANY which would change dispatch as determined by ::typ
        elseif isa(arg, Expr) && arg.head == :(=)
            # @nospecialize(arg=val)
            arg, val = arg.args
            if isa(arg, Expr) && arg.head == :(::)
                # @nospecialize(arg::typ=val)
                # unsupported (see above), but generate a kw arg
                arg, typ = arg.args
                return Expr(:kw, :($(esc(arg))::$(esc(typ))), esc(val))
            else
                return Expr(:kw, :($(esc(arg))::ANY), esc(val))
            end
        end
        return esc(arg)
    end
    export @nospecialize
end

"""Get just the function part of a function declaration."""
withincurly(ex) = isexpr(ex, :curly) ? ex.args[1] : ex

if VERSION < v"0.6.0-dev.2043"
    Base.take!(t::Task) = consume(t)
end

is_index_style(ex::Expr) = ex == :(Compat.IndexStyle) || ex == :(Base.IndexStyle) ||
    (ex.head == :(.) && (ex.args[1] == :Compat || ex.args[1] == :Base) &&
         ex.args[2] == Expr(:quote, :IndexStyle))

is_index_style(arg) = false

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
        if VERSION < v"0.6.0-dev.2575" #20414
            ex = Expr(:curly, map(a -> isexpr(a, :call, 2) && a.args[1] == :(<:) ?
                                  :($TypeVar($(QuoteNode(gensym(:T))), $(a.args[2]), false)) :
                                  isexpr(a, :call, 2) && a.args[1] == :(>:) ?
                                  :($TypeVar($(QuoteNode(gensym(:T))), $(a.args[2]), $Any, false)) : a,
                                  ex.args)...)
        end
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


export @compat

if VERSION < v"0.6.0-dev.2042"
    immutable ExponentialBackOff
        n::Int
        first_delay::Float64
        max_delay::Float64
        factor::Float64
        jitter::Float64

        function ExponentialBackOff(n, first_delay, max_delay, factor, jitter)
            all(x->x>=0, (n, first_delay, max_delay, factor, jitter)) || error("all inputs must be non-negative")
            new(n, first_delay, max_delay, factor, jitter)
        end
    end

    """
        ExponentialBackOff(; n=1, first_delay=0.05, max_delay=10.0, factor=5.0, jitter=0.1)

    A [`Float64`](@ref) iterator of length `n` whose elements exponentially increase at a
    rate in the interval `factor` * (1 ± `jitter`).  The first element is
    `first_delay` and all elements are clamped to `max_delay`.
    """
    ExponentialBackOff(; n=1, first_delay=0.05, max_delay=10.0, factor=5.0, jitter=0.1) =
        ExponentialBackOff(n, first_delay, max_delay, factor, jitter)
    Base.start(ebo::ExponentialBackOff) = (ebo.n, min(ebo.first_delay, ebo.max_delay))
    function Base.next(ebo::ExponentialBackOff, state)
        next_n = state[1]-1
        curr_delay = state[2]
        next_delay = min(ebo.max_delay, state[2] * ebo.factor * (1.0 - ebo.jitter + (rand() * 2.0 * ebo.jitter)))
        (curr_delay, (next_n, next_delay))
    end
    Base.done(ebo::ExponentialBackOff, state) = state[1]<1
    Base.length(ebo::ExponentialBackOff) = ebo.n

    function retry(f::Function;  delays=ExponentialBackOff(), check=nothing)
        (args...; kwargs...) -> begin
            state = start(delays)
            while true
                try
                    return f(args...; kwargs...)
                catch e
                    done(delays, state) && rethrow(e)
                    if check !== nothing
                        state, retry_or_not = check(state, e)
                        retry_or_not || rethrow(e)
                    end
                end
                (delay, state) = next(delays, state)
                sleep(delay)
            end
        end
    end
else
    import Base.ExponentialBackOff
    import Base.retry
end

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

@static if VERSION < v"0.6.0-dev.528"
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
if VERSION < v"0.7.0-DEV.1211"
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
else
    macro dep_vectorize_1arg(S, f)
        AbstractArray = GlobalRef(Base, :AbstractArray)
        return esc(:(@deprecate $f(x::$AbstractArray{T}) where {T<:$S} $f.(x)))
    end

    macro dep_vectorize_2arg(S, f)
        AbstractArray = GlobalRef(Base, :AbstractArray)
        return esc(quote
            @deprecate $f(x::$S, y::$AbstractArray{T1}) where {T1<:$S} $f.(x, y)
            @deprecate $f(x::$AbstractArray{T1}, y::$S) where {T1<:$S} $f.(x, y)
            @deprecate $f(x::$AbstractArray{T1}, y::$AbstractArray{T2}) where {T1<:$S, T2<:$S} $f.(x, y)
        end)
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
@static if VERSION < v"0.6.0-dev.848"
    unsafe_get(x::Nullable) = x.value
    unsafe_get(x) = x
    export unsafe_get
    Base.isnull(x) = false
end

# julia#18977
@static if !isdefined(Base, :xor)
    # 0.6
    const xor = $
    const ⊻ = xor
    export xor, ⊻
end

# julia#19246
@static if !isdefined(Base, :numerator)
    # 0.6
    const numerator = num
    const denominator = den
    export numerator, denominator
end

# julia #19950
@static if !isdefined(Base, :iszero)
    # 0.6
    iszero(x) = x == zero(x)
    iszero(x::Number) = x == 0
    iszero(x::AbstractArray) = all(iszero, x)
    export iszero
end

# julia　#20407
@static if !isdefined(Base, :(>:))
    # 0.6
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
@static if VERSION < v"0.6.0-dev.1883"
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

@static if VERSION < v"0.6.0-dev.2093" # Compat.isapprox to allow for NaNs
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

@static if VERSION < v"0.6.0-dev.2840"
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
@static if VERSION < v"0.6.0-dev.2283"
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
@static if VERSION < v"0.6.0-dev.1988"
    StringVector(n::Integer) = Vector{UInt8}(n)
else
    using Base: StringVector
end

# https://github.com/JuliaLang/julia/pull/22064
@static if !isdefined(Base, Symbol("@__MODULE__"))
    # 0.7
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
@static if isdefined(Base, :invokelatest)
    # 0.6
    import Base.invokelatest
else
    invokelatest(f, args...) = eval(current_module(), Expr(:call, f, map(QuoteNode, args)...))
end

# https://github.com/JuliaLang/julia/pull/21257
@static if VERSION < v"0.6.0-pre.beta.28"
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
    # Also add methods for Val(x) that were previously Val{x}
    import Base: reshape
    reshape{N}(parent::AbstractArray, ndims::Val{N}) = reshape(parent, Val{N})
    import Base: ntuple
    ntuple{F,N}(f::F, ::Val{N}) = ntuple(f, Val{N})
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

# https://github.com/JuliaLang/julia/pull/21746
const macros_have_sourceloc = VERSION >= v"0.7-" && length(:(@test).args) == 2

# https://github.com/JuliaLang/julia/pull/22182
module Sys
    const KERNEL = Base.Sys.KERNEL
    @static if VERSION < v"0.7.0-DEV.914"
        isapple(k::Symbol=KERNEL)   = k in (:Darwin, :Apple)
        isbsd(k::Symbol=KERNEL)     = isapple(k) || k in (:FreeBSD, :OpenBSD, :NetBSD, :DragonFly)
        islinux(k::Symbol=KERNEL)   = k == :Linux
        isunix(k::Symbol=KERNEL)    = isbsd(k) || islinux(k)
        iswindows(k::Symbol=KERNEL) = k in (:Windows, :NT)
    else
        import Base.Sys: isapple, isbsd, islinux, isunix, iswindows
    end
end

@static if VERSION < v"0.7.0-DEV.892"
    fieldcount(t) = nfields(t)
    export fieldcount
end

if VERSION < v"0.7.0-DEV.1053"
    Base.read(obj::IO, ::Type{String}) = readstring(obj)
    Base.read(obj::AbstractString, ::Type{String}) = readstring(obj)
    Base.read(obj::Cmd, ::Type{String}) = readstring(obj)
end

# https://github.com/JuliaLang/julia/pull/20005
if VERSION < v"0.7.0-DEV.896"
    Base.InexactError(name::Symbol, T, val) = InexactError()
end

# https://github.com/JuliaLang/julia/pull/22751
if VERSION < v"0.7.0-DEV.924"
    Base.DomainError(val) = DomainError()
    Base.DomainError(val, msg) = DomainError()
end

# https://github.com/JuliaLang/julia/pull/22761
if VERSION < v"0.7.0-DEV.1285"
    Base.OverflowError(msg) = OverflowError()
end

if VERSION < v"0.7.0-DEV.755"
    # This is a hack to only add keyword signature that won't work on all julia versions.
    # However, since we really only need to support a few (0.5, 0.6 and early 0.7) versions
    # this should be good enough.
    let Tf = typeof(cov), Tkw = Core.Core.kwftype(Tf)
        @eval begin
            @inline function _get_corrected(kws)
                corrected = true
                nkw = length(kws) >> 1
                for i in 1:nkw
                    if kws[i * 2 - 1] !== :corrected
                        Base.kwerr(kws)
                    end
                    corrected = kws[i * 2]
                end
                return corrected::Bool
            end
            if VERSION >= v"0.6"
                (::$Tkw)(kws::Vector{Any}, ::$Tf, x::AbstractVector) = cov(x, _get_corrected(kws))
                (::$Tkw)(kws::Vector{Any}, ::$Tf, X::AbstractVector, Y::AbstractVector) =
                    cov(X, Y, _get_corrected(kws))
            end
            (::$Tkw)(kws::Vector{Any}, ::$Tf, x::AbstractMatrix, vardim::Int) =
                cov(x, vardim, _get_corrected(kws))
            (::$Tkw)(kws::Vector{Any}, ::$Tf, X::AbstractVecOrMat, Y::AbstractVecOrMat,
                     vardim::Int) = cov(X, Y, vardim, _get_corrected(kws))
        end
    end
end

# 0.7.0-DEV.1415
@static if !isdefined(Base, :adjoint)
    const adjoint = ctranspose
    const adjoint! = ctranspose!
    export adjoint, adjoint!
end

# 0.7.0-DEV.1592
@static if !isdefined(Base, :MathConstants)
    @eval module MathConstants
    # All other ones are already exported by Base (so should be already in the users namespace)
    # and will be automatically be in this module.
    export ℯ
    const ℯ = e
    end
    const ℯ = e
    export ℯ
else
    import Base.MathConstants
end

# 0.7.0-DEV.1535
@static if !isdefined(Base, :partialsort)
    const partialsort = select
    const partialsort! = select!
    const partialsortperm = selectperm
    const partialsortperm! = selectperm!
    export partialsort, partialsort!, partialsortperm, partialsortperm!
end

# 0.7.0-DEV.1721
@static if !isdefined(Base, :AbstractRange)
    const AbstractRange = Range
    export AbstractRange
end

if VERSION < v"0.7.0-DEV.1325"
    function Base.rtoldefault(x, y, atol::Real)
        T = isa(x, Type) ? x : typeof(x)
        S = isa(y, Type) ? y : typeof(y)
        rtol = max(Base.rtoldefault(real(T)), Base.rtoldefault(real(S)))
        return atol > 0 ? zero(rtol) : rtol
    end
end

# 0.7.0-DEV.1775
@static if !isdefined(Base, :isconcrete)
    const isconcrete = isleaftype
    export isconcrete
end

# 0.7.0-DEV.2005
if VERSION < v"0.7.0-DEV.2005"
    const Mmap = Base.Mmap
    const Test = Base.Test
    @eval module SharedArrays
        if isdefined(Base, :Distributed)
            using Base.Distributed.procs
        else
            using Base.procs
        end
        export SharedArray, SharedMatrix, SharedVector, indexpids, localindexes, sdata,
               procs
    end
    const DelimitedFiles = Base.DataFmt
else
    import Test, SharedArrays, Mmap, DelimitedFiles
end

if VERSION < v"0.7.0-DEV.2575"
    const Dates = Base.Dates
else
    import Dates
end

# 0.7.0-DEV.1993
@static if !isdefined(Base, :EqualTo)
    if VERSION >= v"0.6.0"
        include_string(@__MODULE__, """
            struct EqualTo{T} <: Function
                x::T

                EqualTo(x::T) where {T} = new{T}(x)
            end
        """)
    else
        include_string(@__MODULE__, """
            immutable EqualTo{T} <: Function
                x::T
            end
        """)
    end
    (f::EqualTo)(y) = isequal(f.x, y)
    const equalto = EqualTo
    export equalto
end

# 0.7.0-DEV.912
if VERSION < v"0.7.0-DEV.912"
    import Base.*
    (*)(s1::Union{Char,AbstractString}, ss::Union{Char,AbstractString}...) = string(s1, ss...)
end

# 0.7.0-DEV.2318
@static if !isdefined(Base, :BitSet)
    const BitSet = IntSet
    export BitSet
end

# 0.7.0-DEV.1930
@static if !isdefined(Base, :textwidth)
    textwidth(c::Char) = charwidth(c)
    textwidth(c::AbstractString) = strwidth(c)
    export textwidth
end

# 0.7.0-DEV.2116
@static if VERSION < v"0.7.0-DEV.2116"
    import Base: spdiagm
    function spdiagm(kv::Pair...)
        I, J, V = Base.SparseArrays.spdiagm_internal(last.(kv), first.(kv))
        m = max(Base.SparseArrays.dimlub(I), Base.SparseArrays.dimlub(J))
        return sparse(I, J, V, m, m)
    end
end

# 0.7.0-DEV.2161
@static if VERSION < v"0.7.0-DEV.2161"
    import Base: diagm
    function diagm(kv::Pair...)
        T = promote_type(map(x -> eltype(x.second), kv)...)
        n = mapreduce(x -> length(x.second) + abs(x.first), max, kv)
        A = zeros(T, n, n)
        for p in kv
            inds = diagind(A, p.first)
            for (i, val) in enumerate(p.second)
                A[inds[i]] += val
            end
        end
        return A
    end
end

if VERSION < v"0.7.0-DEV.2377"
    (::Type{Matrix{T}}){T}(s::UniformScaling, dims::Dims{2}) = setindex!(zeros(T, dims), T(s.λ), diagind(dims...))
    (::Type{Matrix{T}}){T}(s::UniformScaling, m::Integer, n::Integer) = Matrix{T}(s, Dims((m, n)))

    (::Type{SparseMatrixCSC{Tv,Ti}}){Tv,Ti}(s::UniformScaling, m::Integer, n::Integer) = SparseMatrixCSC{Tv,Ti}(s, Dims((m, n)))
    (::Type{SparseMatrixCSC{Tv}}){Tv}(s::UniformScaling, m::Integer, n::Integer) = SparseMatrixCSC{Tv}(s, Dims((m, n)))
    (::Type{SparseMatrixCSC{Tv}}){Tv}(s::UniformScaling, dims::Dims{2}) = SparseMatrixCSC{Tv,Int}(s, dims)
    function (::Type{SparseMatrixCSC{Tv,Ti}}){Tv,Ti}(s::UniformScaling, dims::Dims{2})
        @boundscheck first(dims) < 0 && throw(ArgumentError("first dimension invalid ($(first(dims)) < 0)"))
        @boundscheck last(dims) < 0 && throw(ArgumentError("second dimension invalid ($(last(dims)) < 0)"))
        iszero(s.λ) && return spzeros(Tv, Ti, dims...)
        m, n, k = dims..., min(dims...)
        nzval = fill!(Vector{Tv}(k), Tv(s.λ))
        rowval = copy!(Vector{Ti}(k), 1:k)
        colptr = copy!(Vector{Ti}(n + 1), 1:(k + 1))
        for i in (k + 2):(n + 1) colptr[i] = (k + 1) end
        SparseMatrixCSC{Tv,Ti}(dims..., colptr, rowval, nzval)
    end
end
if VERSION < v"0.7.0-DEV.2543"
    (::Type{Array{T}}){T}(s::UniformScaling, dims::Dims{2}) = Matrix{T}(s, dims)
    (::Type{Array{T}}){T}(s::UniformScaling, m::Integer, n::Integer) = Matrix{T}(s, m, n)
end

# 0.7.0-DEV.2581
@static if !isdefined(Base, :Uninitialized)
    if VERSION >= v"0.6.0"
        include_string(@__MODULE__, """
            struct Uninitialized end
            Array{T}(::Uninitialized, args...) where {T} = Array{T}(args...)
            Array{T,N}(::Uninitialized, args...) where {T,N} = Array{T,N}(args...)
            Vector(::Uninitialized, args...) = Vector(args...)
            Matrix(::Uninitialized, args...) = Matrix(args...)
        """)
    else
        include_string(@__MODULE__, """
            immutable Uninitialized end
            (::Type{Array{T}}){T}(::Uninitialized, args...) = Array{T}(args...)
            (::Type{Array{T,N}}){T,N}(::Uninitialized, args...) = Array{T,N}(args...)
            (::Type{Vector})(::Uninitialized, args...) = Vector(args...)
            (::Type{Matrix})(::Uninitialized, args...) = Matrix(args...)
        """)
    end
    const uninitialized = Uninitialized()
    export Uninitialized, uninitialized
end

include("deprecated.jl")

end # module Compat
