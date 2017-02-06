# This file is a part of Julia. License is MIT: http://julialang.org/license

# Deprecated functions and objects
#
# Please add new deprecations at the bottom of the file.
# A function deprecated in a release will be removed in the next one.
# Please also add a reference to the pull request which introduced the
# deprecation.
#
# For simple cases where a direct replacement is available, use @deprecate:
# the first argument is the signature of the deprecated method, the second one
# is the call which replaces it. Remove the definition of the deprecated method
# and unexport it, as @deprecate takes care of calling the replacement
# and of exporting the function.
#
# For more complex cases, move the body of the deprecated method in this file,
# and call depwarn() directly from inside it. The symbol depwarn() expects is
# the name of the function, which is used to ensure that the deprecation warning
# is only printed the first time for each call place.

macro deprecate(old,new,ex=true)
    meta = Expr(:meta, :noinline)
    if isa(old,Symbol)
        oldname = Expr(:quote,old)
        newname = Expr(:quote,new)
        Expr(:toplevel,
            ex ? Expr(:export,esc(old)) : nothing,
            :(function $(esc(old))(args...)
                  $meta
                  depwarn(string($oldname," is deprecated, use ",$newname," instead."),
                          $oldname)
                  $(esc(new))(args...)
              end))
    elseif isa(old,Expr) && old.head == :call
        remove_linenums!(new)
        oldcall = sprint(io->show_unquoted(io,old))
        newcall = sprint(io->show_unquoted(io,new))
        oldsym = if isa(old.args[1],Symbol)
            old.args[1]
        elseif isa(old.args[1],Expr) && old.args[1].head == :curly
            old.args[1].args[1]
        else
            error("invalid usage of @deprecate")
        end
        oldname = Expr(:quote, oldsym)
        Expr(:toplevel,
            Expr(:export,esc(oldsym)),
            :($(esc(old)) = begin
                  $meta
                  depwarn(string($oldcall," is deprecated, use ",$newcall," instead."),
                          $oldname)
                  $(esc(new))
              end))
    else
        error("invalid usage of @deprecate")
    end
end

function depwarn(msg, funcsym)
    opts = JLOptions()
    if opts.depwarn > 0
        bt = backtrace()
        _depwarn(msg, opts, bt, firstcaller(bt, funcsym))
    end
    nothing
end
function _depwarn(msg, opts, bt, caller)
    ln = Int(unsafe_load(cglobal(:jl_lineno, Cint)))
    fn = unsafe_string(unsafe_load(cglobal(:jl_filename, Ptr{Cchar})))
    if opts.depwarn == 1 # raise a warning
        warn(msg, once=(caller != StackTraces.UNKNOWN), key=(caller,fn,ln), bt=bt,
             filename=fn, lineno=ln)
    elseif opts.depwarn == 2 # raise an error
        throw(ErrorException(msg))
    end
end

firstcaller(bt::Array{Ptr{Void},1}, funcsym::Symbol) = firstcaller(bt, (funcsym,))
function firstcaller(bt::Array{Ptr{Void},1}, funcsyms)
    # Identify the calling line
    found = false
    lkup = StackTraces.UNKNOWN
    for frame in bt
        lkups = StackTraces.lookup(frame)
        for lkup in lkups
            if lkup === StackTraces.UNKNOWN
                continue
            end
            found && @goto found
            found = lkup.func in funcsyms
        end
    end
    return StackTraces.UNKNOWN
    @label found
    return lkup
end

deprecate(s::Symbol) = deprecate(current_module(), s)
deprecate(m::Module, s::Symbol) = ccall(:jl_deprecate_binding, Void, (Any, Any), m, s)

macro deprecate_binding(old, new)
    Expr(:toplevel,
         Expr(:export, esc(old)),
         Expr(:const, Expr(:(=), esc(old), esc(new))),
         Expr(:call, :deprecate, Expr(:quote, old)))
end

# BEGIN 0.6 deprecations

const _oldstyle_array_vcat_ = false

@deprecate write(x) write(STDOUT::IO, x)

function delete!(::EnvHash, k::AbstractString, def)
    depwarn("`delete!(ENV, k, def)` should be replaced with `pop!(ENV, k, def)`. Be aware that `pop!` returns `k` or `def`, while `delete!` returns `ENV` or `def`.", :delete!)
    haskey(ENV,k) ? delete!(ENV,k) : def
end

@deprecate (+)(J::UniformScaling, x::Number) J.λ + x
@deprecate (+)(x::Number, J::UniformScaling) x + J.λ
@deprecate (-)(J::UniformScaling, x::Number) J.λ - x
@deprecate (-)(x::Number, J::UniformScaling) x - J.λ

# Deprecate methods that convert Diagonal and Bidiagonal to <:AbstractTriangular.
function convert(::Type{UpperTriangular}, A::Diagonal)
    depwarn(string("`convert(::Type{UpperTriangular}, A::Diagonal)` and other methods ",
        "that convert `Diagonal`/`Bidiagonal` to `<:AbstractTriangular` are deprecated. ",
        "Consider calling the `UpperTriangular` constructor directly ",
        "(`UpperTriangular(A)`) instead."), :convert)
    UpperTriangular(A)
end
function convert(::Type{LowerTriangular}, A::Diagonal)
    depwarn(string("`convert(::Type{LowerTriangular}, A::Diagonal)` and other methods ",
        "that convert `Diagonal`/`Bidiagonal` to `<:AbstractTriangular` are deprecated. ",
        "Consider calling the `LowerTriangular` constructor directly ",
        "(`LowerTriangular(A)`) instead."), :convert)
    LowerTriangular(A)
end
function convert(::Type{Base.LinAlg.UnitUpperTriangular}, A::Diagonal)
    depwarn(string("`convert(::Type{UnitUpperTriangular}, A::Diagonal)` and other methods ",
        "that convert `Diagonal`/`Bidiagonal` to `<:AbstractTriangular` are deprecated. ",
        "Consider calling the `UnitUpperTriangular` constructor directly ",
        "(`Base.LinAlg.UnitUpperTriangular(A)`) instead."), :convert)
    if !all(x -> x == oneunit(x), A.diag)
        throw(ArgumentError("matrix cannot be represented as UnitUpperTriangular"))
    end
    Base.LinAlg.UnitUpperTriangular(Array(A))
end
function convert(::Type{Base.LinAlg.UnitLowerTriangular}, A::Diagonal)
    depwarn(string("`convert(::Type{UnitLowerTriangular}, A::Diagonal)` and other methods ",
        "that convert `Diagonal`/`Bidiagonal` to `<:AbstractTriangular` are deprecated. ",
        "Consider calling the `UnitLowerTriangular` constructor directly ",
        "(`Base.LinAlg.UnitLowerTriangular(A)`) instead."), :convert)
    if !all(x -> x == oneunit(x), A.diag)
        throw(ArgumentError("matrix cannot be represented as UnitLowerTriangular"))
    end
    Base.LinAlg.UnitLowerTriangular(Array(A))
end
function convert(::Type{LowerTriangular}, A::Bidiagonal)
    depwarn(string("`convert(::Type{LowerTriangular}, A::Bidiagonal)` and other methods ",
        "that convert `Diagonal`/`Bidiagonal` to `<:AbstractTriangular` are deprecated. ",
        "Consider calling the `LowerTriangular` constructor directly (`LowerTriangular(A)`) ",
        "instead."), :convert)
    if !A.isupper
        LowerTriangular(Array(A))
    else
        throw(ArgumentError("Bidiagonal matrix must have lower off diagonal to be converted to LowerTriangular"))
    end
end
function convert(::Type{UpperTriangular}, A::Bidiagonal)
    depwarn(string("`convert(::Type{UpperTriangular}, A::Bidiagonal)` and other methods ",
        "that convert `Diagoinal`/`Bidiagonal` to `<:AbstractTriangular` are deprecated. ",
        "Consider calling the `UpperTriangular` constructor directly (`UpperTriangular(A)`) ",
        "instead."), :convert)
    if A.isupper
        UpperTriangular(Array(A))
    else
        throw(ArgumentError("Bidiagonal matrix must have upper off diagonal to be converted to UpperTriangular"))
    end
end

# Deprecate three-arg SubArray since the constructor doesn't need the dims tuple
@deprecate SubArray(parent::AbstractArray, indexes::Tuple, dims::Tuple) SubArray(parent, indexes)

# Deprecate vectorized unary functions over sparse matrices in favor of compact broadcast syntax (#17265).
for f in (:sin, :sinh, :sind, :asin, :asinh, :asind,
        :tan, :tanh, :tand, :atan, :atanh, :atand,
        :sinpi, :cosc, :ceil, :floor, :trunc, :round, :real, :imag,
        :log1p, :expm1, :abs, :abs2,
        :log, :log2, :log10, :exp, :exp2, :exp10, :sinc, :cospi,
        :cos, :cosh, :cosd, :acos, :acosd,
        :cot, :coth, :cotd, :acot, :acotd,
        :sec, :sech, :secd, :asech,
        :csc, :csch, :cscd, :acsch)
    @eval @deprecate $f(A::SparseMatrixCSC) $f.(A)
end

# For deprecating vectorized functions in favor of compact broadcast syntax
macro dep_vectorize_1arg(S, f)
    S = esc(S)
    f = esc(f)
    T = esc(:T)
    x = esc(:x)
    AbsArr = esc(:AbstractArray)
    :( @deprecate $f{$T<:$S}($x::$AbsArr{$T}) $f.($x) )
end
macro dep_vectorize_2arg(S, f)
    S = esc(S)
    f = esc(f)
    T1 = esc(:T1)
    T2 = esc(:T2)
    x = esc(:x)
    y = esc(:y)
    AbsArr = esc(:AbstractArray)
    quote
        @deprecate $f{$T1<:$S}($x::$S, $y::$AbsArr{$T1}) $f.($x,$y)
        @deprecate $f{$T1<:$S}($x::$AbsArr{$T1}, $y::$S) $f.($x,$y)
        @deprecate $f{$T1<:$S,$T2<:$S}($x::$AbsArr{$T1}, $y::$AbsArr{$T2}) $f.($x,$y)
    end
end

# Deprecate @vectorize_1arg-vectorized functions from...
for f in (
        # base/special/trig.jl
        :sinpi, :cospi, :sinc, :cosc,
        # base/special/log.jl
        :log, :log1p,
        # base/special/gamma.jl
        :gamma, :lfact, :digamma, :trigamma, :zeta, :eta,
        # base/special/erf.jl
        :erfcx, :erfi, :dawson,
        # base/special/bessel.jl
        :airyai, :airyaiprime, :airybi, :airybiprime,
        :besselj0, :besselj1, :bessely0, :bessely1,
        # base/math.jl
        :cbrt, :sinh, :cosh, :tanh, :atan, :asinh, :exp, :erf, :erfc, :exp2,
        :expm1, :exp10, :sin, :cos, :tan, :asin, :acos, :acosh, :atanh,
        #=:log,=# :log2, :log10, :lgamma, #=:log1p,=# :sqrt,
        # base/floatfuncs.jl
        :abs, :abs2, :angle, :isnan, :isinf, :isfinite,
        # base/complex.jl
        :cis,
        )
    @eval @dep_vectorize_1arg Number $f
end
# base/fastmath.jl
for f in ( :acos_fast, :acosh_fast, :angle_fast, :asin_fast, :asinh_fast,
            :atan_fast, :atanh_fast, :cbrt_fast, :cis_fast, :cos_fast,
            :cosh_fast, :exp10_fast, :exp2_fast, :exp_fast, :expm1_fast,
            :lgamma_fast, :log10_fast, :log1p_fast, :log2_fast, :log_fast,
            :sin_fast, :sinh_fast, :sqrt_fast, :tan_fast, :tanh_fast )
    @eval FastMath Base.@dep_vectorize_1arg Number $f
end
for f in (
        :invdigamma, # base/special/gamma.jl
        :erfinc, :erfcinv, # base/special/erf.jl
        :trunc, :floor, :ceil, :round, # base/floatfuncs.jl
        :rad2deg, :deg2rad, :exponent, :significand, # base/math.jl
        :sind, :cosd, :tand, :asind, :acosd, :atand, :asecd, :acscd, :acotd, # base/special/trig.jl
        )
    @eval @dep_vectorize_1arg Real $f
end
# base/complex.jl
@dep_vectorize_1arg Complex round
@dep_vectorize_1arg Complex float
# base/dates/*.jl
for f in (:unix2datetime, :rata2datetime, :julian2datetime)  # base/dates/conversions.jl
    @eval Dates Base.@dep_vectorize_1arg Real $f
end
for f in (
        # base/dates/accessors.jl
        :year, :month, :day, :week, :dayofmonth, :yearmonth, :monthday, :yearmonthday,
        # base/dates/adjusters.jl
        :firstdayofweek, :lastdayofweek, :firstdayofmonth,
        :lastdayofmonth, :firstdayofyear, :lastdayofyear,
        :firstdayofquarter, :lastdayofquarter,
        # base/dates/query.jl
        :dayname, :dayabbr, :dayofweek, :dayofweekofmonth,
        :daysofweekinmonth, :monthname, :monthabbr, :daysinmonth,
        :isleapyear, :dayofyear, :daysinyear, :quarterofyear, :dayofquarter,
    )
    @eval Dates Base.@dep_vectorize_1arg Dates.TimeType $f
end
for f in (
    :hour, :minute, :second, :millisecond, # base/dates/accessors.jl
    :Date, :datetime2unix, :datetime2rata, :datetime2julian, # base/dates/conversions.jl
    )
    @eval Dates Base.@dep_vectorize_1arg Dates.DateTime $f
end
@eval Dates Base.@dep_vectorize_1arg Dates.Date Datetime # base/dates/conversions.jl

# Deprecate @vectorize_2arg-vectorized functions from...
for f in (
        # base/special/gamma.jl
        :polygamma, :zeta, :beta, :lbeta,
        # base/special/bessel.jl
        :besseli, :besselix, :besselj, :besseljx,
        :besselk, :besselkx, :bessely, :besselyx, :besselh,
        :besselhx, :hankelh1, :hankelh2, :hankelh1x, :hankelh2x,
        # base/math.jl
        :log, :hypot, :atan2,
    )
    @eval @dep_vectorize_2arg Number $f
end
# base/fastmath.jl
for f in (:pow_fast, :atan2_fast, :hypot_fast, :max_fast, :min_fast, :minmax_fast)
    @eval FastMath Base.@dep_vectorize_2arg Number $f
end
for f in (
        :max, :min, # base/math.jl
        :copysign, :flipsign, # base/floatfuncs.jl
    )
    @eval @dep_vectorize_2arg Real $f
end

# Deprecate @vectorize_1arg and @vectorize_2arg themselves
macro vectorize_1arg(S,f)
    depwarn(string("`@vectorize_1arg` is deprecated in favor of compact broadcast syntax. ",
        "Instead of `@vectorize_1arg`'ing function `f` and calling `f(arg)`, call `f.(arg)`."),
        :vectorize_1arg)
    quote
        @dep_vectorize_1arg($(esc(S)),$(esc(f)))
    end
end
macro vectorize_2arg(S,f)
    depwarn(string("`@vectorize_2arg` is deprecated in favor of compact broadcast syntax. ",
        "Instead of `@vectorize_2arg`'ing function `f` and calling `f(arg1, arg2)`, call ",
        "`f.(arg1,arg2)`. "), :vectorize_2arg)
    quote
        @dep_vectorize_2arg($(esc(S)),$(esc(f)))
    end
end
export @vectorize_1arg, @vectorize_2arg

# deprecations for uses of old dot operators (.* etc) as objects, rather than
# just calling them infix.
for op in (:(!=), :≠, :+, :-, :*, :/, :÷, :%, :<, :(<=), :≤, :(==), :>, :>=, :≥, :\, :^, ://, :>>, :<<)
    dotop = Symbol('.', op)
    # define as const dotop = (a,b) -> ...
    # to work around syntax deprecation for dotop(a,b) = ...
    @eval const $dotop = (a,b) -> begin
        depwarn(string($(string(dotop)), " is no longer a function object; use broadcast(",$op,", ...) instead"),
                $(QuoteNode(dotop)))
        broadcast($op, a, b)
    end
    @eval export $dotop
end

# Devectorize manually vectorized abs methods in favor of compact broadcast syntax
@deprecate abs(f::Base.Pkg.Resolve.MaxSum.Field) abs.(f)
@deprecate abs(B::BitArray) abs.(B)
@deprecate abs(M::Bidiagonal) abs.(M)
@deprecate abs(D::Diagonal) abs.(D)
@deprecate abs(M::Tridiagonal) abs.(M)
@deprecate abs(M::SymTridiagonal) abs.(M)
@deprecate abs(x::AbstractSparseVector) abs.(x)

# Deprecate @textmime into the Multimedia module, #18441
@eval Multimedia macro textmime(mime)
    Base.depwarn(string("`@textmime \"mime\"` is deprecated; use ",
        "`Base.Multimedia.istextmime(::MIME\"mime\") = true` instead"
        ), :textmime)
    quote
        Base.Multimedia.istextmime(::MIME{$(Meta.quot(Symbol(mime)))}) = true
    end
end

@deprecate ipermutedims(A::AbstractArray,p) permutedims(A, invperm(p))

# 18696
function ($)(x, y)
    depwarn("`x \$ y` is deprecated.  use `xor(x, y)` or `x ⊻ y` instead.", :$)
    xor(x, y)
end
export $

@deprecate is (===)

# midpoints of intervals
@deprecate midpoints(r::Range) r[1:length(r)-1] + 0.5*step(r)
@deprecate midpoints(v::AbstractVector) [0.5*(v[i] + v[i+1]) for i in 1:length(v)-1]

@deprecate_binding Filter    Iterators.Filter
@deprecate_binding Zip       Iterators.Zip
@deprecate filter(flt, itr)  Iterators.filter(flt, itr)
@deprecate_binding rest      Iterators.rest
@deprecate_binding countfrom Iterators.countfrom
@deprecate_binding take      Iterators.take
@deprecate_binding drop      Iterators.drop
@deprecate_binding cycle     Iterators.cycle
@deprecate_binding repeated  Iterators.repeated

# promote_op method where the operator is also a type
function promote_op(op::Type, Ts::Type...)
    depwarn("promote_op(op::Type, ::Type...) is deprecated as it is no " *
            "longer needed in Base. If you need its functionality, consider " *
            "defining it locally.", :promote_op)
    if isdefined(Core, :Inference)
        return Core.Inference.return_type(op, Tuple{Ts...})
    end
    return op
end

# NOTE: Deprecation of `isdefined(a::Array, i::Int)` is implemented in src/array.c
# and deprecation of `invoke(f, (types...), ...)` is implemented in src/builtins.c
# To be removed when 0.6 deprecations are removed

# NOTE: Deprecation of Channel{T}() is implemented in channels.jl.
# To be removed from there when 0.6 deprecations are removed.

# Not exported, but probably better to have deprecations anyway
function reduced_dims(::Tuple{}, d::Int)
    d < 1 && throw(ArgumentError("dimension must be ≥ 1, got $d"))
    ()
end
reduced_dims(::Tuple{}, region) = ()
function reduced_dims(dims::Dims, region)
    Base.depwarn("`reduced_dims` is deprecated for Dims-tuples; pass `indices` to `reduced_indices` instead", :reduced_dims)
    map(last, reduced_indices(map(OneTo, dims), region))
end

function reduced_dims0(::Tuple{}, d::Int)
    d < 1 && throw(ArgumentError("dimension must be ≥ 1, got $d"))
    ()
end
reduced_dims0(::Tuple{}, region) = ()
function reduced_dims0(dims::Dims, region)
    Base.depwarn("`reduced_dims0` is deprecated for Dims-tuples; pass `indices` to `reduced_indices0` instead", :reduced_dims0)
    map(last, reduced_indices0(map(OneTo, dims), region))
end

function reduced_dims(a::AbstractArray, region)
    Base.depwarn("`reduced_dims` is deprecated in favor of `reduced_indices`", :reduced_dims)
    to_shape(reduced_indices(a, region))  # to_shape keeps the return-type consistent, when it's possible to do so
end

function reduced_dims0(a::AbstractArray, region)
    Base.depwarn("`reduced_dims0` is deprecated in favor of `reduced_indices0`", :reduced_dims)
    to_shape(reduced_indices0(a, region))
end

# #18218
@eval Base.LinAlg begin
    function arithtype(T)
        Base.depwarn(string("arithtype is now deprecated. If you were using it inside a ",
            "promote_op call, use promote_op(LinAlg.matprod, Ts...) instead. Otherwise, ",
            "if you need its functionality, consider defining it locally."),
            :arithtype)
        T
    end
    function arithtype(::Type{Bool})
        Base.depwarn(string("arithtype is now deprecated. If you were using it inside a ",
            "promote_op call, use promote_op(LinAlg.matprod, Ts...) instead. Otherwise, ",
            "if you need its functionality, consider defining it locally."),
            :arithtype)
        Int
    end
end

# #19246
@deprecate den denominator
@deprecate num numerator

Filesystem.stop_watching(stream::Filesystem._FDWatcher) = depwarn("stop_watching(::_FDWatcher) should not be used", :stop_watching)

# #19088
@deprecate takebuf_array take!
@deprecate takebuf_string(b) String(take!(b))

# #19288
@eval Base.Dates begin
    function recur{T<:TimeType}(fun::Function, dr::StepRange{T}; negate::Bool=false, limit::Int=10000)
        Base.depwarn("Dates.recur is deprecated, use filter instead.",:recur)
        if negate
            filter(x -> !fun(x), dr)
        else
            filter(fun, dr)
        end
     end
     recur{T<:TimeType}(fun::Function, start::T, stop::T; step::Period=Day(1), negate::Bool=false, limit::Int=10000) = recur(fun, start:step:stop; negate=negate)
end

# Index conversions revamp; #19730
function getindex(A::LogicalIndex, i::Int)
    depwarn("getindex(A::LogicalIndex, i) is deprecated; use iteration or index into the result of `collect(A)` instead.", :getindex)
    checkbounds(A, i)
    first(Iterators.drop(A, i-1))
end
function to_indexes(I...)
    depwarn("to_indexes is deprecated; pass both the source array `A` and indices as `to_indices(A, $(I...))` instead.", :to_indexes)
    map(_to_index, I)
end
_to_index(i) = to_index(I)
_to_index(c::Colon) = c
const _colon_usage_msg = "convert Colons to a set of indices for indexing into array `A` by passing them in a complete tuple of indices `I` to `to_indices(A, I)`"
function getindex(::Colon, i)
    depwarn("getindex(::Colon, i) is deprecated; $_colon_usage_msg", :getindex)
    to_index(i)
end
function unsafe_getindex(::Colon, i::Integer)
    depwarn("getindex(::Colon, i) is deprecated; $_colon_usage_msg", :unsafe_getindex)
    to_index(i)
end
function step(::Colon)
    depwarn("step(::Colon) is deprecated; $_colon_usage_msg", :step)
    1
end
function isempty(::Colon)
    depwarn("isempty(::Colon) is deprecated; $_colon_usage_msg", :isempty)
    false
end
function in(::Integer, ::Colon)
    depwarn("in(::Integer, ::Colon) is deprecated; $_colon_usage_msg", :in)
    true
end

# #18931
@deprecate cummin(A, dim=1) accumulate(min, A, dim)
@deprecate cummax(A, dim=1) accumulate(max, A, dim)

# #19598
@deprecate sumabs(x)          sum(abs, x)
@deprecate sumabs(A, region)  sum(abs, A, region)
@deprecate sumabs2(x)         sum(abs2, x)
@deprecate sumabs2(A, region) sum(abs2, A, region)
@deprecate minabs(x)          minimum(abs, x)
@deprecate minabs(A, region)  minimum(abs, A, region)
@deprecate maxabs(x)          maximum(abs, x)
@deprecate maxabs(A, region)  maximum(abs, A, region)

for (dep, f, op) in [(:sumabs!, :sum!, :abs),
                     (:sumabs2!, :sum!, :abs2),
                     (:minabs!, :minimum!, :abs),
                     (:maxabs!, :maximum!, :abs)]
    @eval function ($dep)(r, A; init=true)
        Base.depwarn("$dep(r, A; init=$init) is deprecated, use $f($op, r, A; init=$init) instead.", Symbol($dep))
        ($f)($op, r, A; init=init)
    end
end

## Deprecate broadcast_zpreserving[!] (wasn't exported, but might as well be friendly)
function gen_broadcast_function_sparse(genbody::Function, f::Function, is_first_sparse::Bool)
    body = genbody(f, is_first_sparse)
    @eval let
        local _F_
        function _F_{Tv,Ti}(B::SparseMatrixCSC{Tv,Ti}, A_1, A_2)
            $body
        end
        _F_
    end
end
function gen_broadcast_body_zpreserving(f::Function, is_first_sparse::Bool)
    F = Expr(:quote, f)
    if is_first_sparse
        A1 = :(A_1)
        A2 = :(A_2)
        op1 = :(val1)
        op2 = :(val2)
    else
        A1 = :(A_2)
        A2 = :(A_1)
        op1 = :(val2)
        op2 = :(val1)
    end
    quote
        Base.Broadcast.check_broadcast_indices(indices(B), $A1)
        Base.Broadcast.check_broadcast_indices(indices(B), $A2)

        nnzB = isempty(B) ? 0 :
               nnz($A1) * div(B.n, ($A1).n) * div(B.m, ($A1).m)
        if length(B.rowval) < nnzB
            resize!(B.rowval, nnzB)
        end
        if length(B.nzval) < nnzB
            resize!(B.nzval, nnzB)
        end
        z = zero(Tv)

        ptrB = 1
        B.colptr[1] = 1

        @inbounds for col = 1:B.n
            ptr1::Int  = ($A1).n == 1 ? ($A1).colptr[1] : ($A1).colptr[col]
            stop1::Int = ($A1).n == 1 ? ($A1).colptr[2] : ($A1).colptr[col+1]
            col2 = size($A2, 2) == 1 ? 1 : col
            row = 1
            while ptr1 < stop1 && row <= B.m
                if ($A1).m != 1
                    row = ($A1).rowval[ptr1]
                end
                row2 = size($A2, 1) == 1 ? 1 : row
                val1 = ($A1).nzval[ptr1]
                val2 = ($A2)[row2,col2]
                res = ($F)($op1, $op2)
                if res != z
                    B.rowval[ptrB] = row
                    B.nzval[ptrB] = res
                    ptrB += 1
                end
                if ($A1).m != 1
                    ptr1 += 1
                else
                    row += 1
                end
            end
            B.colptr[col+1] = ptrB
        end
        deleteat!(B.rowval, B.colptr[end]:length(B.rowval))
        deleteat!(B.nzval, B.colptr[end]:length(B.nzval))
        nothing
    end
end
for (Bsig, A1sig, A2sig, gbb, funcname) in
    (
     (SparseMatrixCSC   , SparseMatrixCSC  ,  Array,  :gen_broadcast_body_zpreserving, :_broadcast_zpreserving!),
     (SparseMatrixCSC   , Array  ,  SparseMatrixCSC,  :gen_broadcast_body_zpreserving, :_broadcast_zpreserving!),
     (SparseMatrixCSC   , Number  ,  SparseMatrixCSC,  :gen_broadcast_body_zpreserving, :_broadcast_zpreserving!),
     (SparseMatrixCSC   , SparseMatrixCSC  ,  Number,  :gen_broadcast_body_zpreserving, :_broadcast_zpreserving!),
     (SparseMatrixCSC   , BitArray  ,  SparseMatrixCSC,  :gen_broadcast_body_zpreserving, :_broadcast_zpreserving!),
     (SparseMatrixCSC   , SparseMatrixCSC  ,  BitArray,  :gen_broadcast_body_zpreserving, :_broadcast_zpreserving!),
     )
    @eval let cache = Dict{Function,Function}()
        global $funcname
        function $funcname(f::Function, B::$Bsig, A1::$A1sig, A2::$A2sig)
            func       = @get! cache  f  gen_broadcast_function_sparse($gbb, f, ($A1sig) <: SparseMatrixCSC)
            # need eval because func was just created by gen_broadcast_function_sparse
            # TODO: convert this to a generated function
            eval(current_module(), Expr(:body, Expr(:return, Expr(:call, QuoteNode(func), QuoteNode(B), QuoteNode(A1), QuoteNode(A2)))))
            return B
        end
    end  # let broadcast_cache
end
_broadcast_zpreserving!(args...) = broadcast!(args...)
# note: promote_eltype_op also deprecated, defined later in this file
_broadcast_zpreserving(f, As...) =
    broadcast!(f, similar(Array{_promote_eltype_op(f, As...)}, Base.Broadcast.broadcast_indices(As...)), As...)
_broadcast_zpreserving{Tv1,Ti1,Tv2,Ti2}(f::Function, A_1::SparseMatrixCSC{Tv1,Ti1}, A_2::SparseMatrixCSC{Tv2,Ti2}) =
    _broadcast_zpreserving!(f, spzeros(promote_type(Tv1, Tv2), promote_type(Ti1, Ti2), Base.to_shape(Base.Broadcast.broadcast_indices(A_1, A_2))), A_1, A_2)
_broadcast_zpreserving{Tv,Ti}(f::Function, A_1::SparseMatrixCSC{Tv,Ti}, A_2::Union{Array,BitArray,Number}) =
    _broadcast_zpreserving!(f, spzeros(promote_eltype(A_1, A_2), Ti, Base.to_shape(Base.Broadcast.broadcast_indices(A_1, A_2))), A_1, A_2)
_broadcast_zpreserving{Tv,Ti}(f::Function, A_1::Union{Array,BitArray,Number}, A_2::SparseMatrixCSC{Tv,Ti}) =
    _broadcast_zpreserving!(f, spzeros(promote_eltype(A_1, A_2), Ti, Base.to_shape(Base.Broadcast.broadcast_indices(A_1, A_2))), A_1, A_2)

function _depstring_bczpres()
    return string("broadcast_zpreserving[!] is deprecated. Generic sparse broadcast[!] ",
        "provides most of broadcast_zpreserving[!]'s functionality. If you have a use case ",
        "that generic sparse broadcast[!] does not cover, please describe your use case in ",
        " issue #19533 (https://github.com/JuliaLang/julia/issues/19533).")
end
function _depwarn_bczpres(f, args...)
    depwarn(_depstring_bczpres(), :broadcast_zpreserving)
    return _broadcast_zpreserving(f, args...)
end
function _depwarn_bczpres!(f, args...)
    depwarn(_depstring_bczpres(), :broadcast_zpreserving!)
    return _broadcast_zpreserving!(f, args...)
end
@eval SparseArrays begin
    broadcast_zpreserving(f, args...) = Base._depwarn_bczpres(f, args...)
    broadcast_zpreserving(f, A::SparseMatrixCSC, B::SparseMatrixCSC) = Base._depwarn_bczpres(f, A, B)
    broadcast_zpreserving(f, A::SparseMatrixCSC, B::Union{Array,BitArray,Number}) = Base._depwarn_bczpres(f, A, B)
    broadcast_zpreserving(f, A::Union{Array,BitArray,Number}, B::SparseMatrixCSC) = Base._depwarn_bczpres(f, A, B)
    broadcast_zpreserving!(f, args...) = Base._depwarn_bczpres!(f, args...)
    broadcast_zpreserving!(f, C::SparseMatrixCSC, A::SparseMatrixCSC, B::Union{Array,BitArray,Number}) = Base._depwarn_bczpres!(f, C, A, B)
    broadcast_zpreserving!(f, C::SparseMatrixCSC, A::Union{Array,BitArray,Number}, B::SparseMatrixCSC) = Base._depwarn_bczpres!(f, C, A, B)
end

# #19719
@deprecate getindex(t::Tuple, r::AbstractArray)       getindex(t, vec(r))
@deprecate getindex(t::Tuple, b::AbstractArray{Bool}) getindex(t, vec(b))

# Deprecate isimag (#19947).
@deprecate isimag(z::Number) iszero(real(z))

@deprecate airy(z::Number) airyai(z)
@deprecate airyx(z::Number) airyaix(z)
@deprecate airyprime(z::Number) airyaiprime(z)
@deprecate airy{T<:Number}(x::AbstractArray{T}) airyai.(x)
@deprecate airyx{T<:Number}(x::AbstractArray{T}) airyaix.(x)
@deprecate airyprime{T<:Number}(x::AbstractArray{T}) airyprime.(x)

function _airy(k::Integer, z::Complex128)
    depwarn("`airy(k,x)` is deprecated, use `airyai(x)`, `airyaiprime(x)`, `airybi(x)` or `airybiprime(x)` instead.",:airy)
    id = Int32(k==1 || k==3)
    if k == 0 || k == 1
        return Base.Math._airy(z, id, Int32(1))
    elseif k == 2 || k == 3
        return Base.Math._biry(z, id, Int32(1))
    else
        throw(ArgumentError("k must be between 0 and 3"))
    end
end
function _airyx(k::Integer, z::Complex128)
    depwarn("`airyx(k,x)` is deprecated, use `airyaix(x)`, `airyaiprimex(x)`, `airybix(x)` or `airybiprimex(x)` instead.",:airyx)
    id = Int32(k==1 || k==3)
    if k == 0 || k == 1
        return Base.Math._airy(z, id, Int32(2))
    elseif k == 2 || k == 3
        return Base.Math._biry(z, id, Int32(2))
    else
        throw(ArgumentError("k must be between 0 and 3"))
    end
end

for afn in (:airy,:airyx)
    _afn = Symbol("_"*string(afn))
    suf  = string(afn)[5:end]
    @eval begin
        function $afn(k::Integer, z::Complex128)
            afn = $(QuoteNode(afn))
            suf = $(QuoteNode(suf))
            depwarn("`$afn(k,x)` is deprecated, use `airyai$suf(x)`, `airyaiprime$suf(x)`, `airybi$suf(x)` or `airybiprime$suf(x)` instead.",$(QuoteNode(afn)))
            $_afn(k,z)
        end

        $afn(k::Integer, z::Complex) = $afn(k, float(z))
        $afn{T<:AbstractFloat}(k::Integer, z::Complex{T}) = throw(MethodError($afn,(k,z)))
        $afn(k::Integer, z::Complex64) = Complex64($afn(k, Complex128(z)))
        $afn(k::Integer, x::Real) = $afn(k, float(x))
        $afn(k::Integer, x::AbstractFloat) = real($afn(k, complex(x)))

        function $afn{T<:Number}(k::Number, x::AbstractArray{T})
            $afn.(k,x)
        end
        function $afn{S<:Number}(k::AbstractArray{S}, x::Number)
            $afn.(k,x)
        end
        function $afn{S<:Number,T<:Number}(k::AbstractArray{S}, x::AbstractArray{T})
            $afn.(k,x)
        end
    end
end

# Deprecate vectorized xor in favor of compact broadcast syntax
@deprecate xor(a::Bool, B::BitArray)                xor.(a, B)
@deprecate xor(A::BitArray, b::Bool)                xor.(A, b)
@deprecate xor(a::Number, B::AbstractArray)         xor.(a, B)
@deprecate xor(A::AbstractArray, b::Number)         xor.(A, b)
@deprecate xor(A::AbstractArray, B::AbstractArray)  xor.(A, B)

# QuadGK moved to a package (#19741)
function quadgk(args...; kwargs...)
    error(string(quadgk, args, " has been moved to the package QuadGK.jl.\n",
                 "Run Pkg.add(\"QuadGK\") to install QuadGK on Julia v0.6 and later, and then run `using QuadGK`."))
end
export quadgk

# Collections functions moved to a package (#19800)
module Collections
    export PriorityQueue, enqueue!, dequeue!, heapify!, heapify, heappop!, heappush!, isheap, peek
    for f in (:PriorityQueue, :enqueue!, :dequeue!, :heapify!, :heapify, :heappop!, :heappush!, :isheap, :peek)
        @eval function ($f)(args...; kwargs...)
            error(string($f, args, " has been moved to the package DataStructures.jl.\n",
                         "Run Pkg.add(\"DataStructures\") to install DataStructures on Julia v0.6 and later, ",
                         "and then run `using DataStructures`."))
        end
    end
end
export Collections

# Broadcast now returns a BitArray when the resulting eltype is Bool (#17623)
@deprecate bitbroadcast broadcast

# Deprecate two-argument map! (map!(f, A)) for a cycle in anticipation of semantic change
@deprecate map!{F}(f::F, A::AbstractArray) map!(f, A, A)
@deprecate asyncmap!(f, c; ntasks=0, batch_size=nothing) asyncmap!(f, c, c; ntasks=ntasks, batch_size=batch_size)

# Not exported, but used outside Base
_promote_array_type(F, ::Type, ::Type, T::Type) = T
_promote_array_type{S<:Real, A<:AbstractFloat}(F, ::Type{S}, ::Type{A}, ::Type) = A
_promote_array_type{S<:Integer, A<:Integer}(F, ::Type{S}, ::Type{A}, ::Type) = A
_promote_array_type{S<:Integer, A<:Integer}(::typeof(/), ::Type{S}, ::Type{A}, T::Type) = T
_promote_array_type{S<:Integer, A<:Integer}(::typeof(\), ::Type{S}, ::Type{A}, T::Type) = T
_promote_array_type{S<:Integer}(::typeof(/), ::Type{S}, ::Type{Bool}, T::Type) = T
_promote_array_type{S<:Integer}(::typeof(\), ::Type{S}, ::Type{Bool}, T::Type) = T
_promote_array_type{S<:Integer}(F, ::Type{S}, ::Type{Bool}, T::Type) = T
_promote_array_type{S<:Union{Complex, Real}, T<:AbstractFloat}(F, ::Type{S}, ::Type{Complex{T}}, ::Type) = Complex{T}
function promote_array_type(F, R, S, T)
    Base.depwarn("`promote_array_type` is deprecated as it is no longer needed " *
                 "in Base. See https://github.com/JuliaLang/julia/issues/19669 " *
                 "for more information.", :promote_array_type)
    _promote_array_type(F, R, S, T)
end

# Deprecate manually vectorized abs2 methods in favor of compact broadcast syntax
@deprecate abs2(x::AbstractSparseVector) abs2.(x)

# Deprecate manually vectorized sign methods in favor of compact broadcast syntax
@deprecate sign(A::AbstractArray) sign.(A)

# Deprecate manually vectorized trigonometric and hyperbolic functions in favor of compact broadcast syntax
for f in (:sec, :sech, :secd, :asec, :asech,
            :csc, :csch, :cscd, :acsc, :acsch,
            :cot, :coth, :cotd, :acot, :acoth)
    @eval @deprecate $f{T<:Number}(A::AbstractArray{T}) $f.(A)
end

# Deprecate vectorized two-argument complex in favor of compact broadcast syntax
@deprecate complex(A::AbstractArray, b::Real)           complex.(A, b)
@deprecate complex(a::Real, B::AbstractArray)           complex.(a, B)
@deprecate complex(A::AbstractArray, B::AbstractArray)  complex.(A, B)

# Deprecate manually vectorized clamp methods in favor of compact broadcast syntax
@deprecate clamp(A::AbstractArray, lo, hi) clamp.(A, lo, hi)

# Deprecate manually vectorized round methods in favor of compact broadcast syntax
@deprecate round(M::Bidiagonal) round.(M)
@deprecate round(M::Tridiagonal) round.(M)
@deprecate round(M::SymTridiagonal) round.(M)
@deprecate round{T}(::Type{T}, x::AbstractArray) round.(T, x)
@deprecate round{T}(::Type{T}, x::AbstractArray, r::RoundingMode) round.(x, r)
@deprecate round(x::AbstractArray, r::RoundingMode) round.(x, r)
@deprecate round(x::AbstractArray, digits::Integer, base::Integer = 10) round.(x, digits, base)

# Deprecate manually vectorized trunc methods in favor of compact broadcast syntax
@deprecate trunc(M::Bidiagonal) trunc.(M)
@deprecate trunc(M::Tridiagonal) trunc.(M)
@deprecate trunc(M::SymTridiagonal) trunc.(M)
@deprecate trunc{T}(::Type{T}, x::AbstractArray) trunc.(T, x)
@deprecate trunc(x::AbstractArray, digits::Integer, base::Integer = 10) trunc.(x, digits, base)

# Deprecate manually vectorized floor methods in favor of compact broadcast syntax
@deprecate floor(M::Bidiagonal) floor.(M)
@deprecate floor(M::Tridiagonal) floor.(M)
@deprecate floor(M::SymTridiagonal) floor.(M)
@deprecate floor{T}(::Type{T}, A::AbstractArray) floor.(T, A)
@deprecate floor(A::AbstractArray, digits::Integer, base::Integer = 10) floor.(A, digits, base)

# Deprecate manually vectorized ceil methods in favor of compact broadcast syntax
@deprecate ceil(M::Bidiagonal) ceil.(M)
@deprecate ceil(M::Tridiagonal) ceil.(M)
@deprecate ceil(M::SymTridiagonal) ceil.(M)
@deprecate ceil{T}(::Type{T}, x::AbstractArray) ceil.(T, x)
@deprecate ceil(x::AbstractArray, digits::Integer, base::Integer = 10) ceil.(x, digits, base)

# Deprecate manually vectorized `big` methods in favor of compact broadcast syntax
@deprecate big(r::UnitRange) big.(r)
@deprecate big(r::StepRange) big.(r)
@deprecate big(r::StepRangeLen) big.(r)
@deprecate big(r::LinSpace) big.(r)
@deprecate big{T<:Integer,N}(x::AbstractArray{T,N}) big.(x)
@deprecate big{T<:AbstractFloat,N}(x::AbstractArray{T,N}) big.(x)
@deprecate big(A::LowerTriangular) big.(A)
@deprecate big(A::UpperTriangular) big.(A)
@deprecate big(A::Base.LinAlg.UnitLowerTriangular) big.(A)
@deprecate big(A::Base.LinAlg.UnitUpperTriangular) big.(A)
@deprecate big(B::Bidiagonal) big.(B)
@deprecate big{T<:Integer,N}(A::AbstractArray{Complex{T},N}) big.(A)
@deprecate big{T<:AbstractFloat,N}(A::AbstractArray{Complex{T},N}) big.(A)
@deprecate big{T<:Integer,N}(x::AbstractArray{Complex{Rational{T}},N}) big.(A)

# Deprecate manually vectorized div methods in favor of compact broadcast syntax
@deprecate div(A::Number, B::AbstractArray) div.(A, B)
@deprecate div(A::AbstractArray, B::Number) div.(A, B)
@deprecate div(A::AbstractArray, B::AbstractArray) div.(A, B)

# Deprecate manually vectorized rem methods in favor of compact broadcast syntax
@deprecate rem(A::Number, B::AbstractArray) rem.(A, B)
@deprecate rem(A::AbstractArray, B::Number) rem.(A, B)

# Deprecate manually vectorized div, mod, and % methods for dates
@deprecate div{P<:Dates.Period}(X::StridedArray{P}, y::P)         div.(X, y)
@deprecate div{P<:Dates.Period}(X::StridedArray{P}, y::Integer)   div.(X, y)
@deprecate (%){P<:Dates.Period}(X::StridedArray{P}, y::P)         X .% y
@deprecate mod{P<:Dates.Period}(X::StridedArray{P}, y::P)         mod.(X, y)

# Deprecate manually vectorized mod methods in favor of compact broadcast syntax
@deprecate mod(B::BitArray, x::Bool) mod.(B, x)
@deprecate mod(x::Bool, B::BitArray) mod.(x, B)
@deprecate mod(A::AbstractArray, B::AbstractArray) mod.(A, B)
@deprecate mod{T}(x::Number, A::AbstractArray{T}) mod.(x, A)
@deprecate mod{T}(A::AbstractArray{T}, x::Number) mod.(A, x)

# Deprecate vectorized & in favor of dot syntax
@deprecate (&)(a::Bool, B::BitArray)                a .& B
@deprecate (&)(A::BitArray, b::Bool)                A .& b
@deprecate (&)(a::Number, B::AbstractArray)         a .& B
@deprecate (&)(A::AbstractArray, b::Number)         A .& b
@deprecate (&)(A::AbstractArray, B::AbstractArray)  A .& B

# Deprecate vectorized | in favor of compact broadcast syntax
@deprecate (|)(a::Bool, B::BitArray)                a .| B
@deprecate (|)(A::BitArray, b::Bool)                A .| b
@deprecate (|)(a::Number, B::AbstractArray)         a .| B
@deprecate (|)(A::AbstractArray, b::Number)         A .| b
@deprecate (|)(A::AbstractArray, B::AbstractArray)  A .| B

# Deprecate vectorized ifelse
@deprecate ifelse(c::AbstractArray{Bool}, x, y) ifelse.(c, x, y)
@deprecate ifelse(c::AbstractArray{Bool}, x, y::AbstractArray) ifelse.(c, x, y)
@deprecate ifelse(c::AbstractArray{Bool}, x::AbstractArray, y) ifelse.(c, x, y)
@deprecate ifelse(c::AbstractArray{Bool}, x::AbstractArray, y::AbstractArray) ifelse.(c, x, y)

function frexp{T<:AbstractFloat}(A::Array{T})
    depwarn("`frexp(x::Array)` is discontinued.", :frexp)
    F = similar(A)
    E = Array{Int}(size(A))
    for (iF, iE, iA) in zip(eachindex(F), eachindex(E), eachindex(A))
        F[iF], E[iE] = frexp(A[iA])
    end
    return (F, E)
end

# Deprecate reducing isinteger over arrays
@deprecate isinteger(A::AbstractArray) all(isinteger, A)

# Deprecate promote_eltype_op (#19814, #19937)
_promote_eltype_op(::Any) = Any
_promote_eltype_op(op, A) = (@_inline_meta; promote_op(op, eltype(A)))
_promote_eltype_op(op, A, B) = (@_inline_meta; promote_op(op, eltype(A), eltype(B)))
_promote_eltype_op(op, A, B, C, D...) = (@_inline_meta; _promote_eltype_op(op, eltype(A), _promote_eltype_op(op, B, C, D...)))
@inline function promote_eltype_op(args...)
    depwarn("""
            `promote_eltype_op` is deprecated and should not be used.
            See https://github.com/JuliaLang/julia/issues/19669.""",
            :promote_eltype_op)
    _promote_eltype_op(args...)
end


function unsafe_wrap(::Type{String}, p::Union{Ptr{UInt8},Ptr{Int8}}, len::Integer, own::Bool=false)
    Base.depwarn("unsafe_wrap(String, ...) is deprecated; use `unsafe_string` instead.", :unsafe_wrap)
    #ccall(:jl_array_to_string, Ref{String}, (Any,),
    #      ccall(:jl_ptr_to_array_1d, Vector{UInt8}, (Any, Ptr{UInt8}, Csize_t, Cint),
    #            Vector{UInt8}, p, len, own))
    unsafe_string(p, len)
end
unsafe_wrap(::Type{String}, p::Union{Ptr{UInt8},Ptr{Int8}}, own::Bool=false) =
    unsafe_wrap(String, p, ccall(:strlen, Csize_t, (Ptr{UInt8},), p), own)
unsafe_wrap(::Type{String}, p::Cstring, own::Bool=false) = unsafe_wrap(String, convert(Ptr{UInt8}, p), own)
unsafe_wrap(::Type{String}, p::Cstring, len::Integer, own::Bool=false) =
    unsafe_wrap(String, convert(Ptr{UInt8}, p), len, own)

# #19660
@deprecate finalize(sa::LibGit2.StrArrayStruct) LibGit2.free(sa)
@deprecate finalize(sa::LibGit2.Buffer) LibGit2.free(sa)

## produce, consume, and task iteration
# NOTE: When removing produce/consume, also remove field Task.consumers and related code in
# task.jl and event.jl

function produce(v)
    depwarn("produce is now deprecated. Use Channels for inter-task communication.", :produce)

    ct = current_task()
    local empty, t, q
    while true
        q = ct.consumers
        if isa(q,Task)
            t = q
            ct.consumers = nothing
            empty = true
            break
        elseif isa(q,Condition) && !isempty(q.waitq)
            t = shift!(q.waitq)
            empty = isempty(q.waitq)
            break
        end
        wait()
    end

    t.state == :runnable || throw(AssertionError("producer.consumer.state == :runnable"))
    if empty
        schedule_and_wait(t, v)
        while true
            # wait until there are more consumers
            q = ct.consumers
            if isa(q,Task)
                return q.result
            elseif isa(q,Condition) && !isempty(q.waitq)
                return q.waitq[1].result
            end
            wait()
        end
    else
        schedule(t, v)
        # make sure `t` runs before us. otherwise, the producer might
        # finish before `t` runs again, causing it to see the producer
        # as done, causing done(::Task, _) to miss the value `v`.
        # see issue #7727
        yield()
        return q.waitq[1].result
    end
end
produce(v...) = produce(v)
export produce

function consume(P::Task, values...)
    depwarn("consume is now deprecated. Use Channels for inter-task communication.", :consume)

    if istaskdone(P)
        return wait(P)
    end

    ct = current_task()
    ct.result = length(values)==1 ? values[1] : values

    #### un-optimized version
    #if P.consumers === nothing
    #    P.consumers = Condition()
    #end
    #push!(P.consumers.waitq, ct)
    # optimized version that avoids the queue for 1 consumer
    if P.consumers === nothing || (isa(P.consumers,Condition)&&isempty(P.consumers.waitq))
        P.consumers = ct
    else
        if isa(P.consumers, Task)
            t = P.consumers
            P.consumers = Condition()
            push!(P.consumers.waitq, t)
        end
        push!(P.consumers.waitq, ct)
    end

    P.state == :runnable ? schedule_and_wait(P) : wait() # don't attempt to queue it twice
end
export consume

function start(t::Task)
    depwarn(string("Task iteration is now deprecated.",
                   " Use Channels for inter-task communication. ",
                   " A for-loop on a Channel object is terminated by calling `close` on the object."), :taskfor)
    nothing
end
function done(t::Task, val)
    t.result = consume(t)
    istaskdone(t)
end
next(t::Task, val) = (t.result, nothing)
iteratorsize(::Type{Task}) = SizeUnknown()
iteratoreltype(::Type{Task}) = EltypeUnknown()

isempty(::Task) = error("isempty not defined for Tasks")

@eval Base.Test begin
    approx_full(x::AbstractArray) = x
    approx_full(x::Number) = x
    approx_full(x) = full(x)

    function test_approx_eq(va, vb, Eps, astr, bstr)
        va = approx_full(va)
        vb = approx_full(vb)
        la, lb = length(linearindices(va)), length(linearindices(vb))
        if la != lb
            error("lengths of ", astr, " and ", bstr, " do not match: ",
                "\n  ", astr, " (length $la) = ", va,
                "\n  ", bstr, " (length $lb) = ", vb)
        end
        diff = real(zero(eltype(va)))
        for (xa, xb) = zip(va, vb)
            if isfinite(xa) && isfinite(xb)
                diff = max(diff, abs(xa-xb))
            elseif !isequal(xa,xb)
                error("mismatch of non-finite elements: ",
                    "\n  ", astr, " = ", va,
                    "\n  ", bstr, " = ", vb)
            end
        end

        if !isnan(Eps) && !(diff <= Eps)
            sdiff = string("|", astr, " - ", bstr, "| <= ", Eps)
            error("assertion failed: ", sdiff,
                "\n  ", astr, " = ", va,
                "\n  ", bstr, " = ", vb,
                "\n  difference = ", diff, " > ", Eps)
        end
    end

    array_eps{T}(a::AbstractArray{Complex{T}}) = eps(float(maximum(x->(isfinite(x) ? abs(x) : T(NaN)), a)))
    array_eps(a) = eps(float(maximum(x->(isfinite(x) ? abs(x) : oftype(x,NaN)), a)))

    test_approx_eq(va, vb, astr, bstr) =
        test_approx_eq(va, vb, 1E4*length(linearindices(va))*max(array_eps(va), array_eps(vb)), astr, bstr)

    """
        @test_approx_eq_eps(a, b, tol)

    Test two floating point numbers `a` and `b` for equality taking into account
    a margin of tolerance given by `tol`.
    """
    macro test_approx_eq_eps(a, b, c)
        Base.depwarn(string("@test_approx_eq_eps is deprecated, use `@test ", a, " ≈ ", b, " atol=", c, "` instead"),
                    Symbol("@test_approx_eq_eps"))
        :(test_approx_eq($(esc(a)), $(esc(b)), $(esc(c)), $(string(a)), $(string(b))))
    end
    export @test_approx_eq_eps

    """
        @test_approx_eq(a, b)

    Deprecated. Test two floating point numbers `a` and `b` for equality taking into
    account small numerical errors.
    """
    macro test_approx_eq(a, b)
        Base.depwarn(string("@test_approx_eq is deprecated, use `@test ", a, " ≈ ", b, "` instead"),
                    Symbol("@test_approx_eq"))
        :(test_approx_eq($(esc(a)), $(esc(b)), $(string(a)), $(string(b))))
    end
    export @test_approx_eq
end

# Deprecate partial linear indexing
function partial_linear_indexing_warning_lookup(nidxs_remaining)
    # We need to figure out how many indices were passed for a sensible deprecation warning
    opts = JLOptions()
    if opts.depwarn > 0
        # Find the caller -- this is very expensive so we don't want to do it twice
        bt = backtrace()
        found = false
        call = StackTraces.UNKNOWN
        caller = StackTraces.UNKNOWN
        for frame in bt
            lkups = StackTraces.lookup(frame)
            for caller in lkups
                if caller === StackTraces.UNKNOWN
                    continue
                end
                found && @goto found
                if caller.func in (:getindex, :setindex!, :view)
                    found = true
                    call = caller
                end
            end
        end
        @label found
        fn = "`reshape`"
        if call != StackTraces.UNKNOWN && !isnull(call.linfo)
            # Try to grab the number of dimensions in the parent array
            mi = get(call.linfo)
            args = mi.specTypes.parameters
            if length(args) >= 2 && args[2] <: AbstractArray
                fn = "`reshape(A, Val{$(ndims(args[2]) - nidxs_remaining + 1)})`"
            end
        end
        _depwarn("Partial linear indexing is deprecated. Use $fn to make the dimensionality of the array match the number of indices.", opts, bt, caller)
    end
end
function partial_linear_indexing_warning(n)
    depwarn("Partial linear indexing is deprecated. Use `reshape(A, Val{$n})` to make the dimensionality of the array match the number of indices.", (:getindex, :setindex!, :view))
end

# Deprecate Array(T, dims...) in favor of proper type constructors
@deprecate Array{T,N}(::Type{T}, d::NTuple{N,Int})               Array{T}(d)
@deprecate Array{T}(::Type{T}, d::Int...)                        Array{T}(d...)
@deprecate Array{T}(::Type{T}, m::Int)                           Array{T}(m)
@deprecate Array{T}(::Type{T}, m::Int,n::Int)                    Array{T}(m,n)
@deprecate Array{T}(::Type{T}, m::Int,n::Int,o::Int)             Array{T}(m,n,o)
@deprecate Array{T}(::Type{T}, d::Integer...)                    Array{T}(convert(Tuple{Vararg{Int}}, d))
@deprecate Array{T}(::Type{T}, m::Integer)                       Array{T}(Int(m))
@deprecate Array{T}(::Type{T}, m::Integer,n::Integer)            Array{T}(Int(m),Int(n))
@deprecate Array{T}(::Type{T}, m::Integer,n::Integer,o::Integer) Array{T}(Int(m),Int(n),Int(o))

# Likewise for SharedArrays
@deprecate SharedArray{T,N}(::Type{T}, dims::Dims{N}; kwargs...) SharedArray{T}(dims; kwargs...)
@deprecate SharedArray{T}(::Type{T}, dims::Int...; kwargs...)    SharedArray{T}(dims...; kwargs...)
@deprecate(SharedArray{T,N}(filename::AbstractString, ::Type{T}, dims::NTuple{N,Int}, offset; kwargs...),
           SharedArray{T}(filename, dims, offset; kwargs...))
@deprecate(SharedArray{T}(filename::AbstractString, ::Type{T}, dims::NTuple, offset; kwargs...),
           SharedArray{T}(filename, dims, offset; kwargs...))

@noinline function is_intrinsic_expr(x::ANY)
    Base.depwarn("is_intrinsic_expr is deprecated. There are no intrinsic functions anymore.", :is_intrinsic_expr)
    return false
end

@deprecate EachLine(stream, ondone) EachLine(stream, ondone=ondone)

# These conversions should not be defined, see #19896
@deprecate convert{T<:Number}(::Type{T}, x::Dates.Period) convert(T, Dates.value(x))
@deprecate convert{T<:Dates.Period}(::Type{T}, x::Real)   T(x)
@deprecate convert{R<:Real}(::Type{R}, x::Dates.DateTime) R(Dates.value(x))
@deprecate convert{R<:Real}(::Type{R}, x::Dates.Date)     R(Dates.value(x))
@deprecate convert(::Type{Dates.DateTime}, x::Real)       Dates.DateTime(Dates.Millisecond(x))
@deprecate convert(::Type{Dates.Date}, x::Real)           Dates.Date(Dates.Day(x))

function colon{T<:Dates.Period}(start::T, stop::T)
    depwarn("$start:$stop is deprecated, use $start:$T(1):$stop instead.", :colon)
    colon(start, T(1), stop)
end

# LibGit2 refactor (#19839)
@eval Base.LibGit2 begin
     Base.@deprecate_binding Oid GitHash
     Base.@deprecate_binding GitAnyObject GitUnknownObject

     @deprecate owner(x) repository(x) false
     @deprecate get{T<:GitObject}(::Type{T}, repo::GitRepo, x) T(repo, x) false
     @deprecate get{T<:GitObject}(::Type{T}, repo::GitRepo, oid::GitHash, oid_size::Int) T(repo, GitShortHash(oid, oid_size)) false
     @deprecate revparse(repo::GitRepo, objname::AbstractString) GitObject(repo, objname) false
     @deprecate object(repo::GitRepo, te::GitTreeEntry) GitObject(repo, te) false
     @deprecate commit(ann::GitAnnotated) GitHash(ann) false
     @deprecate cat{T<:GitObject}(repo::GitRepo, ::Type{T}, object::AbstractString) cat(repo, object)
end

# when this deprecation is deleted, remove all calls to it, and all
# negate=nothing keyword arguments, from base/dates/adjusters.jl
@eval Dates function deprecate_negate(f, func, sig, negate)
    if negate === nothing
        return func
    else
        msg = "$f($sig; negate=$negate) is deprecated, use $f("
        negate && (msg *= "!")
        msg *= "$sig) instead."
        Base.depwarn(msg, f)
        return negate ? !func : func
    end
end

# FloatRange replaced by StepRangeLen

@deprecate FloatRange{T}(start::T, step, len, den) Base.floatrange(T, start, step, len, den)

@noinline zero_arg_matrix_constructor(prefix::String) =
    depwarn("$prefix() is deprecated, use $prefix(0, 0) instead.", :zero_arg_matrix_constructor)
function (::Type{Matrix{T}}){T}()
    zero_arg_matrix_constructor("Matrix{T}")
    return Matrix{T}(0, 0)
end
function (::Type{Matrix})()
    zero_arg_matrix_constructor("Matrix")
    return Matrix(0, 0)
end

for name in ("alnum", "alpha", "cntrl", "digit", "number", "graph",
             "lower", "print", "punct", "space", "upper", "xdigit")
    f = Symbol("is",name)
    @eval @deprecate ($f)(s::AbstractString) all($f, s)
end

# END 0.6 deprecations

# BEGIN 1.0 deprecations
# END 1.0 deprecations
