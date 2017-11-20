# This file is a part of Julia. License is MIT: https://julialang.org/license

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

macro deprecate(old, new, ex=true)
    meta = Expr(:meta, :noinline)
    @gensym oldmtname
    if isa(old, Symbol)
        oldname = Expr(:quote, old)
        newname = Expr(:quote, new)
        Expr(:toplevel,
            ex ? Expr(:export, esc(old)) : nothing,
            :(function $(esc(old))(args...)
                  $meta
                  depwarn($"$old is deprecated, use $new instead.", $oldmtname)
                  $(esc(new))(args...)
              end),
            :(const $oldmtname = Core.Typeof($(esc(old))).name.mt.name))
    elseif isa(old, Expr) && (old.head == :call || old.head == :where)
        remove_linenums!(new)
        oldcall = sprint(show_unquoted, old)
        newcall = sprint(show_unquoted, new)
        # if old.head is a :where, step down one level to the :call to avoid code duplication below
        callexpr = old.head == :call ? old : old.args[1]
        if callexpr.head == :call
            if isa(callexpr.args[1], Symbol)
                oldsym = callexpr.args[1]::Symbol
            elseif isa(callexpr.args[1], Expr) && callexpr.args[1].head == :curly
                oldsym = callexpr.args[1].args[1]::Symbol
            else
                error("invalid usage of @deprecate")
            end
        else
            error("invalid usage of @deprecate")
        end
        Expr(:toplevel,
            ex ? Expr(:export, esc(oldsym)) : nothing,
            :($(esc(old)) = begin
                  $meta
                  depwarn($"$oldcall is deprecated, use $newcall instead.", $oldmtname)
                  $(esc(new))
              end),
            :(const $oldmtname = Core.Typeof($(esc(oldsym))).name.mt.name))
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

firstcaller(bt::Vector, funcsym::Symbol) = firstcaller(bt, (funcsym,))
function firstcaller(bt::Vector, funcsyms)
    # Identify the calling line
    found = false
    lkup = StackTraces.UNKNOWN
    for frame in bt
        lkups = StackTraces.lookup(frame)
        for outer lkup in lkups
            if lkup == StackTraces.UNKNOWN
                continue
            end
            found && @goto found
            found = lkup.func in funcsyms
            # look for constructor type name
            if !found && lkup.linfo isa Core.MethodInstance
                li = lkup.linfo
                ft = ccall(:jl_first_argument_datatype, Any, (Any,), li.def.sig)
                if isa(ft,DataType) && ft.name === Type.body.name
                    ft = unwrap_unionall(ft.parameters[1])
                    found = (isa(ft,DataType) && ft.name.name in funcsyms)
                end
            end
        end
    end
    return StackTraces.UNKNOWN
    @label found
    return lkup
end

deprecate(m::Module, s::Symbol, flag=1) = ccall(:jl_deprecate_binding, Void, (Any, Any, Cint), m, s, flag)

macro deprecate_binding(old, new, export_old=true, dep_message=nothing)
    return Expr(:toplevel,
         export_old ? Expr(:export, esc(old)) : nothing,
         dep_message != nothing ? Expr(:const, Expr(:(=),
             esc(Symbol(string("_dep_message_",old))), esc(dep_message))) :
             nothing,
         Expr(:const, Expr(:(=), esc(old), esc(new))),
         Expr(:call, :deprecate, __module__, Expr(:quote, old)))
end

macro deprecate_moved(old, new, export_old=true, default_package=false)
    eold = esc(old)
    return Expr(:toplevel,
         default_package ? :(function $eold(args...; kwargs...)
                                 error($eold, " has been moved to the standard library package ", $new, ".\n",
                                       "Restart Julia and then run `using ", $new, "` to load it.")
                             end) :
                           :(function $eold(args...; kwargs...)
                                 error($eold, " has been moved to the package ", $new, ".jl.\n",
                                       "Run `Pkg.add(\"", $new, "\")` to install it, restart Julia,\n",
                                       "and then run `using ", $new, "` to load it.")
                             end),
         export_old ? Expr(:export, eold) : nothing,
         Expr(:call, :deprecate, __module__, Expr(:quote, old), 2))
end

# BEGIN 0.6-alpha deprecations (delete when 0.6 is released)

@deprecate isambiguous(m1::Method, m2::Method, b::Bool) isambiguous(m1, m2, ambiguous_bottom=b) false
# TODO: delete allow_bottom keyword code in Test.detect_ambiguities

# END 0.6-alpha deprecations

# BEGIN 0.6 deprecations

const _oldstyle_array_vcat_ = false

@deprecate write(x) write(STDOUT::IO, x)

function delete!(::EnvDict, k::AbstractString, def)
    depwarn("`delete!(ENV, k, def)` should be replaced with `pop!(ENV, k, def)`. Be aware that `pop!` returns `k` or `def`, while `delete!` returns `ENV` or `def`.", :delete!)
    haskey(ENV,k) ? delete!(ENV,k) : def
end

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
for f in (:sind, :asind, :tand, :atand, :sinpi, :cosc, :ceil, :floor, :trunc,
        :round, :log1p, :expm1, :abs, :abs2, :log2, :log10, :exp2, :exp10,
        :sinc, :cospi, :cosd, :acosd, :cotd, :acotd, :secd, :cscd)
    @eval import .Math: $f
    @eval @deprecate $f(A::SparseMatrixCSC) $f.(A)
end

# For deprecating vectorized functions in favor of compact broadcast syntax
macro dep_vectorize_1arg(S, f)
    AbstractArray = GlobalRef(Base, :AbstractArray)
    return esc(:( @deprecate $f(x::$AbstractArray{T}) where {T<:$S} $f.(x) ))
end
macro dep_vectorize_2arg(S, f)
    AbstractArray = GlobalRef(Base, :AbstractArray)
    return esc(quote
        @deprecate $f(x::$S, y::$AbstractArray{T1}) where {T1<:$S} $f.(x, y)
        @deprecate $f(x::$AbstractArray{T1}, y::$S) where {T1<:$S} $f.(x, y)
        @deprecate $f(x::$AbstractArray{T1}, y::$AbstractArray{T2}) where {T1<:$S, T2<:$S} $f.(x, y)
    end)
end

# Deprecate @vectorize_1arg-vectorized functions from...
for f in (
        # base/special/trig.jl
        :sinpi, :cospi, :sinc, :cosc,
        # base/special/log.jl
        :log1p,
        # base/special/gamma.jl
        :gamma, :lfact,
        # base/math.jl
        :cbrt, :exp2, :expm1, :exp10, :log2, :log10, :lgamma, #=:log1p,=#
        # base/floatfuncs.jl
        :abs, :abs2, :angle, :isnan, :isinf, :isfinite,
        # base/complex.jl
        :cis,
        )
    @eval import .Math: $f
    @eval @dep_vectorize_1arg Number $f
end
# base/fastmath.jl
for f in ( :acos_fast, :acosh_fast, :angle_fast, :asin_fast, :asinh_fast,
            :atan_fast, :atanh_fast, :cbrt_fast, :cis_fast, :cos_fast,
            :cosh_fast, :exp10_fast, :exp2_fast, :exp_fast, :expm1_fast,
            :lgamma_fast, :log10_fast, :log1p_fast, :log2_fast, :log_fast,
            :sin_fast, :sinh_fast, :sqrt_fast, :tan_fast, :tanh_fast )
    @eval import .FastMath: $f
    @eval @dep_vectorize_1arg Number $f
end
for f in (
        :trunc, :floor, :ceil, :round, # base/floatfuncs.jl
        :rad2deg, :deg2rad, :exponent, :significand, # base/math.jl
        :sind, :cosd, :tand, :asind, :acosd, :atand, :asecd, :acscd, :acotd, # base/special/trig.jl
        )
    @eval import .Math: $f
    @eval @dep_vectorize_1arg Real $f
end
# base/complex.jl
@dep_vectorize_1arg Complex round
@dep_vectorize_1arg Complex float

# Deprecate @vectorize_2arg-vectorized functions from...
for f in (
        # base/special/gamma.jl
        :beta, :lbeta,
        # base/math.jl
        :log, :hypot, :atan2,
    )
    @eval import .Math: $f
    @eval @dep_vectorize_2arg Number $f
end
# base/fastmath.jl
for f in (:pow_fast, :atan2_fast, :hypot_fast, :max_fast, :min_fast, :minmax_fast)
    @eval import .FastMath: $f
    @eval @dep_vectorize_2arg Number $f
end
for f in (
        :max, :min, # base/math.jl
        :copysign, :flipsign, # base/floatfuncs.jl
    )
    @eval @dep_vectorize_2arg Real $f
end

# Deprecate @vectorize_1arg and @vectorize_2arg themselves
macro vectorize_1arg(S, f)
    depwarn(string("`@vectorize_1arg` is deprecated in favor of compact broadcast syntax. ",
        "Instead of `@vectorize_1arg`'ing function `f` and calling `f(arg)`, call `f.(arg)`."),
        :vectorize_1arg)
    quote
        @dep_vectorize_1arg($S, $f)
    end
end
macro vectorize_2arg(S, f)
    depwarn(string("`@vectorize_2arg` is deprecated in favor of compact broadcast syntax. ",
        "Instead of `@vectorize_2arg`'ing function `f` and calling `f(arg1, arg2)`, call ",
        "`f.(arg1, arg2)`. "), :vectorize_2arg)
    quote
        @dep_vectorize_2arg($S, $f)
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
@deprecate midpoints(r::AbstractRange) r[1:length(r)-1] + 0.5*step(r)
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

# #19088
@deprecate takebuf_array take!
@deprecate takebuf_string(b) String(take!(b))

# Index conversions revamp; #19730
function getindex(A::LogicalIndex, i::Int)
    depwarn("getindex(A::LogicalIndex, i) is deprecated; use iteration or index into the result of `collect(A)` instead.", :getindex)
    checkbounds(A, i)
    first(Iterators.drop(A, i-1))
end
function to_indexes(I...)
    Istr = join(I, ", ")
    depwarn("to_indexes is deprecated; pass both the source array `A` and indices as `to_indices(A, $Istr)` instead.", :to_indexes)
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
        function _F_(B::SparseMatrixCSC{Tv,Ti}, A_1, A_2) where {Tv,Ti}
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
            eval(_current_module(), Expr(:body, Expr(:return, Expr(:call, QuoteNode(func), QuoteNode(B), QuoteNode(A1), QuoteNode(A2)))))
            return B
        end
    end  # let broadcast_cache
end
_broadcast_zpreserving!(args...) = broadcast!(args...)
# note: promote_eltype_op also deprecated, defined later in this file
_broadcast_zpreserving(f, As...) =
    broadcast!(f, similar(Array{_promote_eltype_op(f, As...)}, Base.Broadcast.broadcast_indices(As...)), As...)
_broadcast_zpreserving(f::Function, A_1::SparseMatrixCSC{Tv1,Ti1}, A_2::SparseMatrixCSC{Tv2,Ti2}) where {Tv1,Ti1,Tv2,Ti2} =
    _broadcast_zpreserving!(f, spzeros(promote_type(Tv1, Tv2), promote_type(Ti1, Ti2), Base.to_shape(Base.Broadcast.broadcast_indices(A_1, A_2))), A_1, A_2)
_broadcast_zpreserving(f::Function, A_1::SparseMatrixCSC{<:Any,Ti}, A_2::Union{Array,BitArray,Number}) where {Ti} =
    _broadcast_zpreserving!(f, spzeros(promote_eltype(A_1, A_2), Ti, Base.to_shape(Base.Broadcast.broadcast_indices(A_1, A_2))), A_1, A_2)
_broadcast_zpreserving(f::Function, A_1::Union{Array,BitArray,Number}, A_2::SparseMatrixCSC{<:Any,Ti}) where {Ti} =
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

# Deprecate vectorized xor in favor of compact broadcast syntax
@deprecate xor(a::Bool, B::BitArray)                xor.(a, B)
@deprecate xor(A::BitArray, b::Bool)                xor.(A, b)
@deprecate xor(a::Number, B::AbstractArray)         xor.(a, B)
@deprecate xor(A::AbstractArray, b::Number)         xor.(A, b)
@deprecate xor(A::AbstractArray, B::AbstractArray)  xor.(A, B)

# Broadcast now returns a BitArray when the resulting eltype is Bool (#17623)
@deprecate bitbroadcast broadcast

# Deprecate two-argument map! (map!(f, A)) for a cycle in anticipation of semantic change
@deprecate map!(f::F, A::AbstractArray) where {F} map!(f, A, A)
@deprecate asyncmap!(f, c; ntasks=0, batch_size=nothing) asyncmap!(f, c, c; ntasks=ntasks, batch_size=batch_size)

# Not exported, but used outside Base
_promote_array_type(F, ::Type, ::Type, T::Type) = T
_promote_array_type(F, ::Type{<:Real}, ::Type{A}, ::Type) where {A<:AbstractFloat} = A
_promote_array_type(F, ::Type{<:Integer}, ::Type{A}, ::Type) where {A<:Integer} = A
_promote_array_type(::typeof(/), ::Type{<:Integer}, ::Type{<:Integer}, T::Type) = T
_promote_array_type(::typeof(\), ::Type{<:Integer}, ::Type{<:Integer}, T::Type) = T
_promote_array_type(::typeof(/), ::Type{<:Integer}, ::Type{Bool}, T::Type) = T
_promote_array_type(::typeof(\), ::Type{<:Integer}, ::Type{Bool}, T::Type) = T
_promote_array_type(F, ::Type{<:Integer}, ::Type{Bool}, T::Type) = T
_promote_array_type(F, ::Type{<:Union{Complex, Real}}, ::Type{Complex{T}}, ::Type) where {T<:AbstractFloat} = Complex{T}
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
for f in (:secd, :cscd, :cotd)
    @eval import .Math: $f
    @eval @deprecate $f(A::AbstractArray{<:Number}) $f.(A)
end

# Deprecate vectorized two-argument complex in favor of compact broadcast syntax
@deprecate complex(A::AbstractArray, b::Real)           complex.(A, b)
@deprecate complex(a::Real, B::AbstractArray)           complex.(a, B)
@deprecate complex(A::AbstractArray, B::AbstractArray)  complex.(A, B)

# Deprecate manually vectorized clamp methods in favor of compact broadcast syntax
import .Math: clamp
@deprecate clamp(A::AbstractArray, lo, hi) clamp.(A, lo, hi)

# Deprecate manually vectorized round methods in favor of compact broadcast syntax
@deprecate round(M::Bidiagonal) round.(M)
@deprecate round(M::Tridiagonal) round.(M)
@deprecate round(M::SymTridiagonal) round.(M)
@deprecate round(::Type{T}, x::AbstractArray) where {T} round.(T, x)
@deprecate round(::Type{T}, x::AbstractArray, r::RoundingMode) where {T} round.(T, x, r)
@deprecate round(x::AbstractArray, r::RoundingMode) round.(x, r)
@deprecate round(x::AbstractArray, digits::Integer, base::Integer = 10) round.(x, digits, base)

# Deprecate manually vectorized trunc methods in favor of compact broadcast syntax
@deprecate trunc(M::Bidiagonal) trunc.(M)
@deprecate trunc(M::Tridiagonal) trunc.(M)
@deprecate trunc(M::SymTridiagonal) trunc.(M)
@deprecate trunc(::Type{T}, x::AbstractArray) where {T} trunc.(T, x)
@deprecate trunc(x::AbstractArray, digits::Integer, base::Integer = 10) trunc.(x, digits, base)

# Deprecate manually vectorized floor methods in favor of compact broadcast syntax
@deprecate floor(M::Bidiagonal) floor.(M)
@deprecate floor(M::Tridiagonal) floor.(M)
@deprecate floor(M::SymTridiagonal) floor.(M)
@deprecate floor(::Type{T}, A::AbstractArray) where {T} floor.(T, A)
@deprecate floor(A::AbstractArray, digits::Integer, base::Integer = 10) floor.(A, digits, base)

# Deprecate manually vectorized ceil methods in favor of compact broadcast syntax
@deprecate ceil(M::Bidiagonal) ceil.(M)
@deprecate ceil(M::Tridiagonal) ceil.(M)
@deprecate ceil(M::SymTridiagonal) ceil.(M)
@deprecate ceil(::Type{T}, x::AbstractArray) where {T} ceil.(T, x)
@deprecate ceil(x::AbstractArray, digits::Integer, base::Integer = 10) ceil.(x, digits, base)

# Deprecate manually vectorized `big` methods in favor of compact broadcast syntax
@deprecate big(r::UnitRange) big.(r)
@deprecate big(r::StepRange) big.(r)
@deprecate big(r::StepRangeLen) big.(r)
@deprecate big(r::LinSpace) big.(r)
@deprecate big(x::AbstractArray{<:Integer}) big.(x)
@deprecate big(x::AbstractArray{<:AbstractFloat}) big.(x)
@deprecate big(A::LowerTriangular) big.(A)
@deprecate big(A::UpperTriangular) big.(A)
@deprecate big(A::Base.LinAlg.UnitLowerTriangular) big.(A)
@deprecate big(A::Base.LinAlg.UnitUpperTriangular) big.(A)
@deprecate big(B::Bidiagonal) big.(B)
@deprecate big(A::AbstractArray{<:Complex{<:Integer}}) big.(A)
@deprecate big(A::AbstractArray{<:Complex{<:AbstractFloat}}) big.(A)
@deprecate big(x::AbstractArray{<:Complex{<:Rational{<:Integer}}}) big.(A)

# Deprecate manually vectorized div methods in favor of compact broadcast syntax
@deprecate div(A::Number, B::AbstractArray) div.(A, B)
@deprecate div(A::AbstractArray, B::Number) div.(A, B)
@deprecate div(A::AbstractArray, B::AbstractArray) div.(A, B)

# Deprecate manually vectorized rem methods in favor of compact broadcast syntax
@deprecate rem(A::Number, B::AbstractArray) rem.(A, B)
@deprecate rem(A::AbstractArray, B::Number) rem.(A, B)

# Deprecate manually vectorized mod methods in favor of compact broadcast syntax
@deprecate mod(B::BitArray, x::Bool) mod.(B, x)
@deprecate mod(x::Bool, B::BitArray) mod.(x, B)
@deprecate mod(A::AbstractArray, B::AbstractArray) mod.(A, B)
@deprecate mod(x::Number, A::AbstractArray) mod.(x, A)
@deprecate mod(A::AbstractArray, x::Number) mod.(A, x)

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

# Deprecate vectorized !
@deprecate(!(A::AbstractArray{Bool}), .!A) # parens for #20541
@deprecate(!(B::BitArray), .!B) # parens for #20541

# Deprecate vectorized ~
@deprecate ~(A::AbstractArray) .~A
@deprecate ~(B::BitArray) .~B

function Math.frexp(A::Array{<:AbstractFloat})
    depwarn(string("`frexp(x::Array)` is discontinued. Though not a direct replacement, ",
                   "consider using dot-syntax to `broadcast` scalar `frexp` over `Array`s ",
                   "instead, for example `frexp.(rand(4))`."), :frexp)
    F = similar(A)
    E = Array{Int}(uninitialized, size(A))
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

# Deprecate Array(T, dims...) in favor of proper type constructors
@deprecate Array(::Type{T}, d::NTuple{N,Int}) where {T,N}               Array{T}(uninitialized, d)
@deprecate Array(::Type{T}, d::Int...) where {T}                        Array{T}(uninitialized, d...)
@deprecate Array(::Type{T}, m::Int) where {T}                           Array{T}(uninitialized, m)
@deprecate Array(::Type{T}, m::Int,n::Int) where {T}                    Array{T}(uninitialized, m,n)
@deprecate Array(::Type{T}, m::Int,n::Int,o::Int) where {T}             Array{T}(uninitialized, m,n,o)
@deprecate Array(::Type{T}, d::Integer...) where {T}                    Array{T}(uninitialized, convert(Tuple{Vararg{Int}}, d))
@deprecate Array(::Type{T}, m::Integer) where {T}                       Array{T}(uninitialized, Int(m))
@deprecate Array(::Type{T}, m::Integer,n::Integer) where {T}            Array{T}(uninitialized, Int(m),Int(n))
@deprecate Array(::Type{T}, m::Integer,n::Integer,o::Integer) where {T} Array{T}(uninitialized, Int(m),Int(n),Int(o))

@noinline function is_intrinsic_expr(@nospecialize(x))
    Base.depwarn("is_intrinsic_expr is deprecated. There are no intrinsic functions anymore.", :is_intrinsic_expr)
    return false
end

@deprecate EachLine(stream, ondone) EachLine(stream, ondone=ondone)

# LibGit2 refactor (#19839)
@eval Base.LibGit2 begin
     Base.@deprecate_binding Oid GitHash
     Base.@deprecate_binding GitAnyObject GitUnknownObject

     @deprecate owner(x) repository(x) false
     @deprecate get(::Type{T}, repo::GitRepo, x) where {T<:GitObject} T(repo, x) false
     @deprecate get(::Type{T}, repo::GitRepo, oid::GitHash, oid_size::Int) where {T<:GitObject} T(repo, GitShortHash(oid, oid_size)) false
     @deprecate revparse(repo::GitRepo, objname::AbstractString) GitObject(repo, objname) false
     @deprecate object(repo::GitRepo, te::GitTreeEntry) GitObject(repo, te) false
     @deprecate commit(ann::GitAnnotated) GitHash(ann) false
     @deprecate lookup(repo::GitRepo, oid::GitHash) GitBlob(repo, oid) false
    function Base.cat(repo::GitRepo, ::Type{T}, spec::Union{AbstractString,AbstractGitHash}) where T<:GitObject
        Base.depwarn("cat(repo::GitRepo, T, spec) is deprecated, use content(T(repo, spec))", :cat)
        try
            return content(GitBlob(repo, spec))
        catch e
            isa(e, LibGit2.GitError) && return nothing
            rethrow(e)
        end
    end
    Base.cat(repo::GitRepo, spec::Union{AbstractString,AbstractGitHash}) = cat(repo, GitBlob, spec)
end

# TODO: remove `:typealias` from BINDING_HEADS in base/docs/Docs.jl
# TODO: remove `'typealias` case in expand-table in julia-syntax.scm

# FloatRange replaced by StepRangeLen

## Old-style floating point ranges. We reimplement them here because
## the replacement StepRangeLen also has 4 real-valued fields, which
## makes deprecation tricky. See #20506.

struct Use_StepRangeLen_Instead{T<:AbstractFloat} <: AbstractRange{T}
    start::T
    step::T
    len::T
    divisor::T
end

Use_StepRangeLen_Instead(a::AbstractFloat, s::AbstractFloat, l::Real, d::AbstractFloat) =
    Use_StepRangeLen_Instead{promote_type(typeof(a),typeof(s),typeof(d))}(a,s,l,d)

isempty(r::Use_StepRangeLen_Instead) = length(r) == 0

step(r::Use_StepRangeLen_Instead) = r.step/r.divisor

length(r::Use_StepRangeLen_Instead) = Integer(r.len)

first(r::Use_StepRangeLen_Instead{T}) where {T} = convert(T, r.start/r.divisor)

last(r::Use_StepRangeLen_Instead{T}) where {T} = convert(T, (r.start + (r.len-1)*r.step)/r.divisor)

start(r::Use_StepRangeLen_Instead) = 0
done(r::Use_StepRangeLen_Instead, i::Int) = length(r) <= i
next(r::Use_StepRangeLen_Instead{T}, i::Int) where {T} =
    (convert(T, (r.start + i*r.step)/r.divisor), i+1)

function getindex(r::Use_StepRangeLen_Instead{T}, i::Integer) where T
    @_inline_meta
    @boundscheck checkbounds(r, i)
    convert(T, (r.start + (i-1)*r.step)/r.divisor)
end

function getindex(r::Use_StepRangeLen_Instead, s::OrdinalRange)
    @_inline_meta
    @boundscheck checkbounds(r, s)
    Use_StepRangeLen_Instead(r.start + (first(s)-1)*r.step, step(s)*r.step, length(s), r.divisor)
end

-(r::Use_StepRangeLen_Instead)   = Use_StepRangeLen_Instead(-r.start, -r.step, r.len, r.divisor)
+(x::Real, r::Use_StepRangeLen_Instead) = Use_StepRangeLen_Instead(r.divisor*x + r.start, r.step, r.len, r.divisor)
-(x::Real, r::Use_StepRangeLen_Instead) = Use_StepRangeLen_Instead(r.divisor*x - r.start, -r.step, r.len, r.divisor)
-(r::Use_StepRangeLen_Instead, x::Real) = Use_StepRangeLen_Instead(r.start - r.divisor*x, r.step, r.len, r.divisor)
*(x::Real, r::Use_StepRangeLen_Instead)   = Use_StepRangeLen_Instead(x*r.start, x*r.step, r.len, r.divisor)
*(r::Use_StepRangeLen_Instead, x::Real)   = x * r
/(r::Use_StepRangeLen_Instead, x::Real)   = Use_StepRangeLen_Instead(r.start/x, r.step/x, r.len, r.divisor)
promote_rule(::Type{Use_StepRangeLen_Instead{T1}},::Type{Use_StepRangeLen_Instead{T2}}) where {T1,T2} =
    Use_StepRangeLen_Instead{promote_type(T1,T2)}
convert(::Type{Use_StepRangeLen_Instead{T}}, r::Use_StepRangeLen_Instead{T}) where {T<:AbstractFloat} = r
convert(::Type{Use_StepRangeLen_Instead{T}}, r::Use_StepRangeLen_Instead) where {T<:AbstractFloat} =
    Use_StepRangeLen_Instead{T}(r.start,r.step,r.len,r.divisor)

promote_rule(::Type{Use_StepRangeLen_Instead{F}}, ::Type{OR}) where {F,OR<:OrdinalRange} =
    Use_StepRangeLen_Instead{promote_type(F,eltype(OR))}
convert(::Type{Use_StepRangeLen_Instead{T}}, r::OrdinalRange) where {T<:AbstractFloat} =
    Use_StepRangeLen_Instead{T}(first(r), step(r), length(r), one(T))
convert(::Type{Use_StepRangeLen_Instead}, r::OrdinalRange{T}) where {T} =
    Use_StepRangeLen_Instead{typeof(float(first(r)))}(first(r), step(r), length(r), one(T))

promote_rule(::Type{LinSpace{F}}, ::Type{OR}) where {F,OR<:Use_StepRangeLen_Instead} =
    LinSpace{promote_type(F,eltype(OR))}
convert(::Type{LinSpace{T}}, r::Use_StepRangeLen_Instead) where {T<:AbstractFloat} =
    linspace(convert(T, first(r)), convert(T, last(r)), convert(T, length(r)))
convert(::Type{LinSpace}, r::Use_StepRangeLen_Instead{T}) where {T<:AbstractFloat} =
    convert(LinSpace{T}, r)

reverse(r::Use_StepRangeLen_Instead)   = Use_StepRangeLen_Instead(r.start + (r.len-1)*r.step, -r.step, r.len, r.divisor)

function sum(r::Use_StepRangeLen_Instead)
    l = length(r)
    if iseven(l)
        s = r.step * (l-1) * (l>>1)
    else
        s = (r.step * l) * ((l-1)>>1)
    end
    return (l * r.start + s)/r.divisor
end

@deprecate_binding FloatRange Use_StepRangeLen_Instead

## end of FloatRange

@noinline zero_arg_matrix_constructor(prefix::String) =
    depwarn("$prefix() is deprecated, use $prefix(uninitialized, 0, 0) instead.", :zero_arg_matrix_constructor)
function Matrix{T}() where T
    zero_arg_matrix_constructor("Matrix{T}")
    return Matrix{T}(uninitialized, 0, 0)
end
function Matrix()
    zero_arg_matrix_constructor("Matrix")
    return Matrix(uninitialized, 0, 0)
end

for name in ("alnum", "alpha", "cntrl", "digit", "number", "graph",
             "lower", "print", "punct", "space", "upper", "xdigit")
    f = Symbol("is",name)
    @eval import .UTF8proc: $f
    @eval @deprecate ($f)(s::AbstractString) all($f, s)
end

# TODO: remove warning for using `_` in parse_input_line in base/client.jl

# Special functions have been moved to a package
for f in (:airyai, :airyaiprime, :airybi, :airybiprime, :airyaix, :airyaiprimex, :airybix, :airybiprimex,
          :besselh, :besselhx, :besseli, :besselix, :besselj, :besselj0, :besselj1, :besseljx, :besselk,
          :besselkx, :bessely, :bessely0, :bessely1, :besselyx,
          :dawson, :erf, :erfc, :erfcinv, :erfcx, :erfi, :erfinv,
          :eta, :zeta, :digamma, :invdigamma, :polygamma, :trigamma,
          :hankelh1, :hankelh1x, :hankelh2, :hankelh2x,
          :airy, :airyx, :airyprime)
    @eval @deprecate_moved $f "SpecialFunctions"
end

@deprecate_binding LinearIndexing IndexStyle false
@deprecate_binding LinearFast IndexLinear false
@deprecate_binding LinearSlow IndexCartesian false
@deprecate_binding linearindexing IndexStyle false

# #19635
for fname in (:ones, :zeros)
    @eval @deprecate ($fname)(T::Type, arr) ($fname)(T, size(arr))
    @eval ($fname)(::Type{T}, i::Integer) where {T} = ($fname)(T, (i,)) # provides disambiguation with method in Base
    @eval function ($fname)(::Type{T}, arr::Array{T}) where T
        msg = $("`$fname{T}(::Type{T}, arr::Array{T})` is deprecated, use `$fname(T, size(arr))` instead.")
        error(msg)
    end
end

# END 0.6 deprecations

# BEGIN 0.7 deprecations

@deprecate issubtype (<:)

@deprecate union() Set()

# 12807
start(::Union{Process, ProcessChain}) = 1
done(::Union{Process, ProcessChain}, i::Int) = (i == 3)
next(p::Union{Process, ProcessChain}, i::Int) = (getindex(p, i), i + 1)
@noinline function getindex(p::Union{Process, ProcessChain}, i::Int)
    depwarn("open(cmd) now returns only a Process<:IO object", :getindex)
    return i == 1 ? getfield(p, p.openstream) : p
end

import .LinAlg: cond
@deprecate cond(F::LinAlg.LU, p::Integer) cond(convert(AbstractArray, F), p)

# PR #21359
import .Random: srand
@deprecate srand(r::MersenneTwister, filename::AbstractString, n::Integer=4) srand(r, read!(filename, Vector{UInt32}(uninitialized, Int(n))))
@deprecate srand(filename::AbstractString, n::Integer=4) srand(read!(filename, Vector{UInt32}(uninitialized, Int(n))))
@deprecate MersenneTwister(filename::AbstractString)  srand(MersenneTwister(0), read!(filename, Vector{UInt32}(uninitialized, Int(4))))

# PR #21974
@deprecate versioninfo(verbose::Bool) versioninfo(verbose=verbose)
@deprecate versioninfo(io::IO, verbose::Bool) versioninfo(io, verbose=verbose)

# PR #22188
import .LinAlg: cholfact, cholfact!
@deprecate cholfact!(A::StridedMatrix, uplo::Symbol, ::Type{Val{false}}) cholfact!(Hermitian(A, uplo), Val(false))
@deprecate cholfact!(A::StridedMatrix, uplo::Symbol) cholfact!(Hermitian(A, uplo))
@deprecate cholfact(A::StridedMatrix, uplo::Symbol, ::Type{Val{false}}) cholfact(Hermitian(A, uplo), Val(false))
@deprecate cholfact(A::StridedMatrix, uplo::Symbol) cholfact(Hermitian(A, uplo))
@deprecate cholfact!(A::StridedMatrix, uplo::Symbol, ::Type{Val{true}}; tol = 0.0) cholfact!(Hermitian(A, uplo), Val(true), tol = tol)
@deprecate cholfact(A::StridedMatrix, uplo::Symbol, ::Type{Val{true}}; tol = 0.0) cholfact(Hermitian(A, uplo), Val(true), tol = tol)

# PR #22245
import .LinAlg: isposdef, isposdef!
@deprecate isposdef(A::AbstractMatrix, UL::Symbol) isposdef(Hermitian(A, UL))
@deprecate isposdef!(A::StridedMatrix, UL::Symbol) isposdef!(Hermitian(A, UL))

# also remove all support machinery in src for current_module when removing this deprecation
# and make Base.include an error
_current_module() = ccall(:jl_get_current_module, Ref{Module}, ())
@noinline function binding_module(s::Symbol)
    depwarn("binding_module(symbol) is deprecated, use `binding_module(module, symbol)` instead.", :binding_module)
    return binding_module(_current_module(), s)
end
export expand
@noinline function expand(@nospecialize(x))
    depwarn("expand(x) is deprecated, use `Meta.lower(module, x)` instead.", :expand)
    return Meta.lower(_current_module(), x)
end
@noinline function macroexpand(@nospecialize(x))
    depwarn("macroexpand(x) is deprecated, use `macroexpand(module, x)` instead.", :macroexpand)
    return macroexpand(_current_module(), x)
end
@noinline function isconst(s::Symbol)
    depwarn("isconst(symbol) is deprecated, use `isconst(module, symbol)` instead.", :isconst)
    return isconst(_current_module(), s)
end
@noinline function include_string(txt::AbstractString, fname::AbstractString)
    depwarn("include_string(string, fname) is deprecated, use `include_string(module, string, fname)` instead.", :include_string)
    return include_string(_current_module(), txt, fname)
end
@noinline function include_string(txt::AbstractString)
    depwarn("include_string(string) is deprecated, use `include_string(module, string)` instead.", :include_string)
    return include_string(_current_module(), txt, "string")
end

"""
    current_module() -> Module

Get the *dynamically* current `Module`, which is the `Module` code is currently being read
from. In general, this is not the same as the module containing the call to this function.

DEPRECATED: use @__MODULE__ instead
"""
@noinline function current_module()
    depwarn("current_module() is deprecated, use `@__MODULE__` instead.", :current_module)
    return _current_module()
end
export current_module

# PR #22062
function LibGit2.set_remote_url(repo::LibGit2.GitRepo, url::AbstractString; remote::AbstractString="origin")
    Base.depwarn(string(
        "`LibGit2.set_remote_url(repo, url; remote=remote)` is deprecated, use ",
        "`LibGit2.set_remote_url(repo, remote, url)` instead."), :set_remote_url)
    LibGit2.set_remote_url(repo, remote, url)
end
function LibGit2.set_remote_url(path::AbstractString, url::AbstractString; remote::AbstractString="origin")
    Base.depwarn(string(
        "`LibGit2.set_remote_url(path, url; remote=remote)` is deprecated, use ",
        "`LibGit2.set_remote_url(path, remote, url)` instead."), :set_remote_url)
    LibGit2.set_remote_url(path, remote, url)
end

module Operators
    for op in [:!, :(!=), :(!==), :%, :&, :*, :+, :-, :/, ://, :<, :<:, :<<, :(<=),
               :<|, :(==), :(===), :>, :>:, :(>=), :>>, :>>>, :\, :^, :colon,
               :adjoint, :getindex, :hcat, :hvcat, :setindex!, :transpose, :vcat,
               :xor, :|, :|>, :~, :×, :÷, :∈, :∉, :∋, :∌, :∘, :√, :∛, :∩, :∪, :≠, :≤,
               :≥, :⊆, :⊈, :⊊, :⊻, :⋅]
        if isdefined(Base, op)
            @eval Base.@deprecate_binding $op Base.$op
        end
    end
end
export Operators

# PR #21956
# This mimics the structure as it was defined in Base to avoid directly breaking code
# that assumes this structure
module DFT
    for f in [:bfft, :bfft!, :brfft, :dct, :dct!, :fft, :fft!, :fftshift, :idct, :idct!,
              :ifft, :ifft!, :ifftshift, :irfft, :plan_bfft, :plan_bfft!, :plan_brfft,
              :plan_dct, :plan_dct!, :plan_fft, :plan_fft!, :plan_idct, :plan_idct!,
              :plan_ifft, :plan_ifft!, :plan_irfft, :plan_rfft, :rfft]
        pkg = endswith(String(f), "shift") ? "AbstractFFTs" : "FFTW"
        @eval Base.@deprecate_moved $f $pkg
    end
    module FFTW
        for f in [:r2r, :r2r!, :plan_r2r, :plan_r2r!]
            @eval Base.@deprecate_moved $f "FFTW"
        end
    end
    Base.deprecate(DFT, :FFTW, 2)
    export FFTW
end
using .DFT
for f in filter(s -> isexported(DFT, s), names(DFT, true))
    @eval export $f
end
module DSP
    for f in [:conv, :conv2, :deconv, :filt, :filt!, :xcorr]
        @eval Base.@deprecate_moved $f "DSP"
    end
end
deprecate(Base, :DSP, 2)
using .DSP
export conv, conv2, deconv, filt, filt!, xcorr

@deprecate_moved SharedArray "SharedArrays" true true

@eval @deprecate_moved $(Symbol("@profile")) "Profile" true true

@deprecate_moved base64encode "Base64" true true
@deprecate_moved base64decode "Base64" true true
@deprecate_moved Base64EncodePipe "Base64" true true
@deprecate_moved Base64DecodePipe "Base64" true true

@deprecate_moved poll_fd "FileWatching" true true
@deprecate_moved poll_file "FileWatching" true true
@deprecate_moved PollingFileWatcher "FileWatching" true true
@deprecate_moved watch_file "FileWatching" true true
@deprecate_moved FileMonitor "FileWatching" true true

@deprecate_moved crc32c "CRC32c" true true

@deprecate_moved DateTime "Dates" true true
@deprecate_moved DateFormat "Dates" true true
@eval @deprecate_moved $(Symbol("@dateformat_str")) "Dates" true true
@deprecate_moved now "Dates" true true

@deprecate_moved eigs "IterativeEigenSolvers" true true
@deprecate_moved svds "IterativeEigenSolvers" true true

# PR #21709
@deprecate cov(x::AbstractVector, corrected::Bool) cov(x, corrected=corrected)
@deprecate cov(x::AbstractMatrix, vardim::Int, corrected::Bool) cov(x, vardim, corrected=corrected)
@deprecate cov(X::AbstractVector, Y::AbstractVector, corrected::Bool) cov(X, Y, corrected=corrected)
@deprecate cov(X::AbstractVecOrMat, Y::AbstractVecOrMat, vardim::Int, corrected::Bool) cov(X, Y, vardim, corrected=corrected)

# bkfact
import .LinAlg: bkfact, bkfact!
function bkfact(A::StridedMatrix, uplo::Symbol, symmetric::Bool = issymmetric(A), rook::Bool = false)
    depwarn("bkfact with uplo and symmetric arguments deprecated. Please use bkfact($(symmetric ? "Symmetric(" : "Hermitian(")A, :$uplo))",
        :bkfact)
    return bkfact(symmetric ? Symmetric(A, uplo) : Hermitian(A, uplo), rook)
end
function bkfact!(A::StridedMatrix, uplo::Symbol, symmetric::Bool = issymmetric(A), rook::Bool = false)
    depwarn("bkfact! with uplo and symmetric arguments deprecated. Please use bkfact!($(symmetric ? "Symmetric(" : "Hermitian(")A, :$uplo))",
        :bkfact!)
    return bkfact!(symmetric ? Symmetric(A, uplo) : Hermitian(A, uplo), rook)
end

# PR #22325
# TODO: when this replace is removed from deprecated.jl:
# 1) rename the function replace_new from strings/util.jl to replace
# 2) update the replace(s::AbstractString, pat, f) method, below replace_new
#    (see instructions there)
function replace(s::AbstractString, pat, f, n::Integer)
    if n <= 0
        depwarn(string("`replace(s, pat, r, count)` with `count <= 0` is deprecated, use ",
                       "`replace(s, pat, r, typemax(Int))` or `replace(s, pat, r)` instead"),
                :replace)
        replace(s, pat, f)
    else
        replace_new(String(s), pat, f, n)
    end
end

# PR #22475
@deprecate ntuple(f, ::Type{Val{N}}) where {N}  ntuple(f, Val(N))
@deprecate fill_to_length(t, val, ::Type{Val{N}}) where {N} fill_to_length(t, val, Val(N)) false
@deprecate literal_pow(a, b, ::Type{Val{N}}) where {N} literal_pow(a, b, Val(N)) false
@eval IteratorsMD @deprecate split(t, V::Type{Val{n}}) where {n} split(t, Val(n)) false
@deprecate sqrtm(A::UpperTriangular{T},::Type{Val{realmatrix}}) where {T,realmatrix} sqrtm(A, Val(realmatrix))
import .LinAlg: lufact, lufact!, qrfact, qrfact!, cholfact, cholfact!
@deprecate lufact(A::AbstractMatrix, ::Type{Val{false}}) lufact(A, Val(false))
@deprecate lufact(A::AbstractMatrix, ::Type{Val{true}}) lufact(A, Val(true))
@deprecate lufact!(A::AbstractMatrix, ::Type{Val{false}}) lufact!(A, Val(false))
@deprecate lufact!(A::AbstractMatrix, ::Type{Val{true}}) lufact!(A, Val(true))
@deprecate qrfact(A::AbstractMatrix, ::Type{Val{false}}) qrfact(A, Val(false))
@deprecate qrfact(A::AbstractMatrix, ::Type{Val{true}}) qrfact(A, Val(true))
@deprecate qrfact!(A::AbstractMatrix, ::Type{Val{false}}) qrfact!(A, Val(false))
@deprecate qrfact!(A::AbstractMatrix, ::Type{Val{true}}) qrfact!(A, Val(true))
@deprecate cholfact(A::AbstractMatrix, ::Type{Val{false}}) cholfact(A, Val(false))
@deprecate cholfact(A::AbstractMatrix, ::Type{Val{true}}; tol = 0.0) cholfact(A, Val(true); tol = tol)
@deprecate cholfact!(A::AbstractMatrix, ::Type{Val{false}}) cholfact!(A, Val(false))
@deprecate cholfact!(A::AbstractMatrix, ::Type{Val{true}}; tol = 0.0) cholfact!(A, Val(true); tol = tol)
@deprecate cat(::Type{Val{N}}, A::AbstractArray...) where {N} cat(Val(N), A...)
@deprecate cat(::Type{Val{N}}, A::SparseArrays._SparseConcatGroup...) where {N} cat(Val(N), A...)
@deprecate cat(::Type{Val{N}}, A::SparseArrays._DenseConcatGroup...) where {N} cat(Val(N), A...)
@deprecate cat_t(::Type{Val{N}}, ::Type{T}, A, B) where {N,T} cat_t(Val(N), T, A, B) false
@deprecate reshape(A::AbstractArray, ::Type{Val{N}}) where {N} reshape(A, Val(N))

@deprecate read(s::IO, x::Ref) read!(s, x)

@deprecate read(s::IO, t::Type, d1::Int, dims::Int...) read!(s, Array{t}(uninitialized, tuple(d1,dims...)))
@deprecate read(s::IO, t::Type, d1::Integer, dims::Integer...) read!(s, Array{t}(uninitialized, convert(Tuple{Vararg{Int}},tuple(d1,dims...))))
@deprecate read(s::IO, t::Type, dims::Dims) read!(s, Array{t}(uninitialized, dims))

function CartesianRange(start::CartesianIndex{N}, stop::CartesianIndex{N}) where N
    inds = map((f,l)->f:l, start.I, stop.I)
    depwarn("the internal representation of CartesianRange has changed, use CartesianRange($inds) (or other more approriate AbstractUnitRange type) instead.", :CartesianRange)
    CartesianRange(inds)
end

# PR #20005
function InexactError()
    depwarn("InexactError now supports arguments, use `InexactError(funcname::Symbol, ::Type, value)` instead.", :InexactError)
    InexactError(:none, Any, nothing)
end

# PR #22751
function DomainError()
    depwarn("DomainError now supports arguments, use `DomainError(value)` or `DomainError(value, msg)` instead.", :DomainError)
    DomainError(nothing)
end

# PR #22761
function OverflowError()
    depwarn("OverflowError now supports a message string, use `OverflowError(msg)` instead.", :OverflowError)
    OverflowError("")
end

# PR #22703
@deprecate Bidiagonal(dv::AbstractVector, ev::AbstractVector, isupper::Bool) Bidiagonal(dv, ev, ifelse(isupper, :U, :L))
@deprecate Bidiagonal(dv::AbstractVector, ev::AbstractVector, uplo::Char) Bidiagonal(dv, ev, ifelse(uplo == 'U', :U, :L))
@deprecate Bidiagonal(A::AbstractMatrix, isupper::Bool) Bidiagonal(A, ifelse(isupper, :U, :L))

@deprecate fieldnames(v) fieldnames(typeof(v))
# nfields(::Type) deprecation in builtins.c: update nfields tfunc in inference.jl when it is removed.
# also replace `_nfields` with `nfields` in summarysize.c when this is removed.

# ::ANY is deprecated in src/method.c
# also remove all instances of `jl_ANY_flag` in src/

# issue #13079
# in julia-parser.scm:
#     move prec-bitshift after prec-rational
#     remove parse-with-chains-warn and bitshift-warn
# update precedence table in doc/src/manual/mathematical-operations.md

# deprecate remaining vectorized methods over SparseVectors (zero-preserving)
for op in (:floor, :ceil, :trunc, :round,
        :log1p, :expm1,  :sinpi,
        :sin,   :tan,    :sind,   :tand,
        :asin,  :atan,   :asind,  :atand,
        :sinh,  :tanh,   :asinh,  :atanh)
    @eval @deprecate ($op)(x::AbstractSparseVector{<:Number,<:Integer}) ($op).(x)
end
# deprecate remaining vectorized methods over SparseVectors (not-zero-preserving)
for op in (:exp, :exp2, :exp10, :log, :log2, :log10,
           :cos, :cosd, :acos, :cosh, :cospi,
           :csc, :cscd, :acot, :csch, :acsch,
           :cot, :cotd, :acosd, :coth,
           :sec, :secd, :acotd, :sech, :asech)
    @eval import .Math: $op
    @eval @deprecate ($op)(x::AbstractSparseVector{<:Number,<:Integer}) ($op).(x)
end

# PR #22182
@deprecate is_apple   Sys.isapple
@deprecate is_bsd     Sys.isbsd
@deprecate is_linux   Sys.islinux
@deprecate is_unix    Sys.isunix
@deprecate is_windows Sys.iswindows

@deprecate read(cmd::AbstractCmd, stdin::Redirectable) read(pipeline(stdin, cmd))
@deprecate readstring(cmd::AbstractCmd, stdin::Redirectable) readstring(pipeline(stdin, cmd))
@deprecate eachline(cmd::AbstractCmd, stdin; chomp::Bool=true) eachline(pipeline(stdin, cmd), chomp=chomp)

@deprecate showall(x)     show(x)
@deprecate showall(io, x) show(IOContext(io, :limit => false), x)

@deprecate_binding AbstractIOBuffer GenericIOBuffer false

@deprecate String(io::GenericIOBuffer) String(take!(copy(io)))

@deprecate readstring(s::IO) read(s, String)
@deprecate readstring(filename::AbstractString) read(filename, String)
@deprecate readstring(cmd::AbstractCmd) read(cmd, String)

# issue #11310
# remove "parametric method syntax" deprecation in julia-syntax.scm

@deprecate momenttype(::Type{T}) where {T} typeof((zero(T)*zero(T) + zero(T)*zero(T))/2) false

# issue #6466
# `write` on non-isbits arrays is deprecated in io.jl.

# PR #22925
# also uncomment constructor tests in test/linalg/bidiag.jl
function Bidiagonal(dv::AbstractVector{T}, ev::AbstractVector{S}, uplo::Symbol) where {T,S}
    depwarn(string("Bidiagonal(dv::AbstractVector{T}, ev::AbstractVector{S}, uplo::Symbol) where {T,S}",
        " is deprecated; manually convert both vectors to the same type instead."), :Bidiagonal)
    R = promote_type(T, S)
    Bidiagonal(convert(Vector{R}, dv), convert(Vector{R}, ev), uplo)
end

# PR #23035
# also uncomment constructor tests in test/linalg/tridiag.jl
function SymTridiagonal(dv::AbstractVector{T}, ev::AbstractVector{S}) where {T,S}
    depwarn(string("SymTridiagonal(dv::AbstractVector{T}, ev::AbstractVector{S}) ",
        "where {T,S} is deprecated; convert both vectors to the same type instead."), :SymTridiagonal)
    R = promote_type(T, S)
    SymTridiagonal(convert(Vector{R}, dv), convert(Vector{R}, ev))
end

# PR #23154
# also uncomment constructor tests in test/linalg/tridiag.jl
function Tridiagonal(dl::AbstractVector{Tl}, d::AbstractVector{Td}, du::AbstractVector{Tu}) where {Tl,Td,Tu}
    depwarn(string("Tridiagonal(dl::AbstractVector{Tl}, d::AbstractVector{Td}, du::AbstractVector{Tu}) ",
        "where {Tl, Td, Tu} is deprecated; convert all vectors to the same type instead."), :Tridiagonal)
    Tridiagonal(map(v->convert(Vector{promote_type(Tl,Td,Tu)}, v), (dl, d, du))...)
end

# deprecate sqrtm in favor of sqrt
@deprecate sqrtm sqrt

# deprecate expm in favor of exp
@deprecate expm! exp!
@deprecate expm exp

# deprecate logm in favor of log
@deprecate logm log

# PR #23092
@eval LibGit2 begin
    function prompt(msg::AbstractString; default::AbstractString="", password::Bool=false)
        Base.depwarn(string(
            "`LibGit2.prompt(msg::AbstractString; default::AbstractString=\"\", password::Bool=false)` is deprecated, use ",
            "`get(Base.prompt(msg, default=default, password=password), \"\")` instead."), :prompt)
        Base.get(Base.prompt(msg, default=default, password=password), "")
    end
end

# PR #23187
@deprecate cpad(s, n::Integer, p=" ") rpad(lpad(s, div(n+textwidth(s), 2), p), n, p) false

# PR #22088
function hex2num(s::AbstractString)
    depwarn("hex2num(s) is deprecated. Use reinterpret(Float64, parse(UInt64, s, 16)) instead.", :hex2num)
    if length(s) <= 4
        return reinterpret(Float16, parse(UInt16, s, 16))
    end
    if length(s) <= 8
        return reinterpret(Float32, parse(UInt32, s, 16))
    end
    return reinterpret(Float64, parse(UInt64, s, 16))
end
export hex2num

@deprecate num2hex(x::Union{Float16,Float32,Float64}) hex(reinterpret(Unsigned, x), sizeof(x)*2)
@deprecate num2hex(n::Integer) hex(n, sizeof(n)*2)

# PR #22742: change in isapprox semantics
@deprecate rtoldefault(x,y) rtoldefault(x,y,0) false

# PR #23235
@deprecate ctranspose adjoint
@deprecate ctranspose! adjoint!

@deprecate convert(::Type{Vector{UInt8}}, s::AbstractString)  Vector{UInt8}(s)
@deprecate convert(::Type{Array{UInt8}}, s::AbstractString)   Vector{UInt8}(s)
@deprecate convert(::Type{Vector{Char}}, s::AbstractString)   Vector{Char}(s)
@deprecate convert(::Type{Symbol}, s::AbstractString)         Symbol(s)
@deprecate convert(::Type{String}, s::Symbol)                 String(s)
@deprecate convert(::Type{String}, v::Vector{UInt8})          String(v)
@deprecate convert(::Type{S}, g::UTF8proc.GraphemeIterator) where {S<:AbstractString}  convert(S, g.s)

# Issue #19923
@deprecate ror                  circshift
@deprecate ror!                 circshift!
@deprecate rol(B, i)            circshift(B, -i)
@deprecate rol!(dest, src, i)   circshift!(dest, src, -i)
@deprecate rol!(B, i)           circshift!(B, -i)

# issue #5148, PR #23259
# warning for `const` on locals should be changed to an error in julia-syntax.scm

# issue #22789
# remove code for `importall` in src/

# issue #17886
# deprecations for filter[!] with 2-arg functions are in associative.jl

# PR #23066
@deprecate cfunction(f, r, a::Tuple) cfunction(f, r, Tuple{a...})

# PR 23341
import .LinAlg: diagm
@deprecate diagm(A::SparseMatrixCSC) sparse(Diagonal(sparsevec(A)))

# PR #23373
@deprecate diagm(A::BitMatrix) BitMatrix(Diagonal(vec(A)))

# PR 23341
@eval GMP @deprecate gmp_version() version() false
@eval GMP @Base.deprecate_binding GMP_VERSION VERSION false
@eval GMP @deprecate gmp_bits_per_limb() bits_per_limb() false
@eval GMP @Base.deprecate_binding GMP_BITS_PER_LIMB BITS_PER_LIMB false
@eval MPFR @deprecate get_version() version() false
@eval LinAlg.LAPACK @deprecate laver() version() false

# PR #23427
@deprecate_binding e          ℯ true ", use ℯ (\\euler) or Base.MathConstants.e"
@deprecate_binding eu         ℯ true ", use ℯ (\\euler) or Base.MathConstants.e"
@deprecate_binding γ          MathConstants.γ
@deprecate_binding eulergamma MathConstants.eulergamma
@deprecate_binding catalan    MathConstants.catalan
@deprecate_binding φ          MathConstants.φ
@deprecate_binding golden     MathConstants.golden

# PR #23271
function IOContext(io::IO; kws...)
    depwarn("IOContext(io, k=v, ...) is deprecated, use IOContext(io, :k => v, ...) instead.", :IOContext)
    IOContext(io, (k=>v for (k, v) in pairs(kws))...)
end

@deprecate IOContext(io::IO, key, value) IOContext(io, key=>value)

# PR #23485
export countnz
function countnz(x)
    depwarn("countnz(x) is deprecated, use either count(!iszero, x) or count(t -> t != 0, x) instead.", :countnz)
    return count(t -> t != 0, x)
end

# issue #14470
# TODO: More deprecations must be removed in src/cgutils.cpp:emit_array_nd_index()
# TODO: Re-enable the disabled tests marked PLI
# On the Julia side, this definition will gracefully supercede the new behavior (already coded)
@inline function checkbounds_indices(::Type{Bool}, IA::Tuple{Any,Vararg{Any}}, ::Tuple{})
    any(x->unsafe_length(x)==0, IA) && return false
    any(x->unsafe_length(x)!=1, IA) && return _depwarn_for_trailing_indices(IA)
    return true
end
function _depwarn_for_trailing_indices(n::Integer) # Called by the C boundscheck
    depwarn("omitting indices for non-singleton trailing dimensions is deprecated. Add `1`s as trailing indices or use `reshape(A, Val($n))` to make the dimensionality of the array match the number of indices.", (:getindex, :setindex!, :view))
    true
end
function _depwarn_for_trailing_indices(t::Tuple)
    depwarn("omitting indices for non-singleton trailing dimensions is deprecated. Add `$(join(map(first, t),','))` as trailing indices or use `reshape` to make the dimensionality of the array match the number of indices.", (:getindex, :setindex!, :view))
    true
end

# issue #22791
@deprecate select partialsort
@deprecate select! partialsort!
@deprecate selectperm partialsortperm
@deprecate selectperm! partialsortperm!

# `initialized` keyword arg to `sort` is deprecated in sort.jl

@deprecate promote_noncircular promote false

import .Iterators.enumerate

@deprecate enumerate(i::IndexLinear,    A::AbstractArray)  pairs(i, A)
@deprecate enumerate(i::IndexCartesian, A::AbstractArray)  pairs(i, A)

@deprecate_binding Range AbstractRange

# issue #5794
@deprecate map(f, d::T) where {T<:Associative}  T( f(p) for p in pairs(d) )

# issue #17086
@deprecate isleaftype isconcrete

# PR #22932
@deprecate +(a::Number, b::AbstractArray) broadcast(+, a, b)
@deprecate +(a::AbstractArray, b::Number) broadcast(+, a, b)
@deprecate -(a::Number, b::AbstractArray) broadcast(-, a, b)
@deprecate -(a::AbstractArray, b::Number) broadcast(-, a, b)

# PR #23640
# when this deprecation is deleted, remove all calls to it, and replace all keywords of:
# `payload::Union{CredentialPayload,Nullable{<:Union{AbstractCredential, CachedCredentials}}}`
#  with `payload::CredentialPayload` from base/libgit2/libgit2.jl
@eval LibGit2 function deprecate_nullable_creds(f, sig, payload)
    if isa(payload, Nullable{<:Union{AbstractCredential, CachedCredentials}})
        # Note: Be careful not to show the contents of the credentials as it could reveal a
        # password.
        if isnull(payload)
            msg = "LibGit2.$f($sig; payload=Nullable()) is deprecated, use "
            msg *= "LibGit2.$f($sig; payload=LibGit2.CredentialPayload()) instead."
            p = CredentialPayload()
        else
            cred = unsafe_get(payload)
            C = typeof(cred)
            msg = "LibGit2.$f($sig; payload=Nullable($C(...))) is deprecated, use "
            msg *= "LibGit2.$f($sig; payload=LibGit2.CredentialPayload($C(...))) instead."
            p = CredentialPayload(cred)
        end
        Base.depwarn(msg, f)
    else
        p = payload::CredentialPayload
    end
    return p
end

# ease transition for return type change of e.g. indmax due to PR #22907 when used in the
# common pattern `ind2sub(size(a), indmax(a))`
@deprecate(ind2sub(dims::NTuple{N,Integer}, idx::CartesianIndex{N}) where N, Tuple(idx))

@deprecate contains(eq::Function, itr, x) any(y->eq(y,x), itr)

# PR #23757
import .SparseArrays.spdiagm
@deprecate spdiagm(x::AbstractVector) sparse(Diagonal(x))
function spdiagm(x::AbstractVector, d::Number)
    depwarn(string("spdiagm(x::AbstractVector, d::Number) is deprecated, use ",
        "spdiagm(d => x) instead, which now returns a square matrix. To preserve the old ",
        "behaviour, use sparse(SparseArrays.spdiagm_internal(d => x)...)"), :spdiagm)
    I, J, V = SparseArrays.spdiagm_internal(d => x)
    return sparse(I, J, V)
end
function spdiagm(x, d)
    depwarn(string("spdiagm((x1, x2, ...), (d1, d2, ...)) is deprecated, use ",
        "spdiagm(d1 => x1, d2 => x2, ...) instead, which now returns a square matrix. ",
        "To preserve the old behaviour, use ",
        "sparse(SparseArrays.spdiagm_internal(d1 => x1, d2 => x2, ...)...)"), :spdiagm)
    I, J, V = SparseArrays.spdiagm_internal((d[i] => x[i] for i in 1:length(x))...)
    return sparse(I, J, V)
end
function spdiagm(x, d, m::Integer, n::Integer)
    depwarn(string("spdiagm((x1, x2, ...), (d1, d2, ...), m, n) is deprecated, use ",
        "spdiagm(d1 => x1, d2 => x2, ...) instead, which now returns a square matrix. ",
        "To specify a non-square matrix and preserve the old behaviour, use ",
        "I, J, V = SparseArrays.spdiagm_internal(d1 => x1, d2 => x2, ...); sparse(I, J, V, m, n)"), :spdiagm)
    I, J, V = SparseArrays.spdiagm_internal((d[i] => x[i] for i in 1:length(x))...)
    return sparse(I, J, V, m, n)
end

# deprecate zeros(D::Diagonal[, opts...])
@deprecate zeros(D::Diagonal)                         Diagonal(fill!(similar(D.diag), 0))
@deprecate zeros(D::Diagonal, ::Type{T}) where {T}    Diagonal(fill!(similar(D.diag, T), 0))
@deprecate zeros(D::Diagonal, ::Type{T}, dims::Dims) where {T}          fill!(similar(D, T, dims), 0)
@deprecate zeros(D::Diagonal, ::Type{T}, dims::Integer...) where {T}    fill!(similar(D, T, dims), 0)

# PR #23690
# `SSHCredential` and `UserPasswordCredential` constructors using `prompt_if_incorrect`
# are deprecated in base/libgit2/types.jl.

# deprecate ones/zeros methods accepting an array as first argument
@deprecate ones(a::AbstractArray, ::Type{T}, dims::Tuple) where {T} fill!(similar(a, T, dims), 1)
@deprecate ones(a::AbstractArray, ::Type{T}, dims...) where {T}     fill!(similar(a, T, dims...), 1)
@deprecate ones(a::AbstractArray, ::Type{T}) where {T}              fill!(similar(a, T), 1)
@deprecate ones(a::AbstractArray)                                   fill!(similar(a), 1)
@deprecate zeros(a::AbstractArray, ::Type{T}, dims::Tuple) where {T}  fill!(similar(a, T, dims), 0)
@deprecate zeros(a::AbstractArray, ::Type{T}, dims...) where {T}      fill!(similar(a, T, dims...), 0)
@deprecate zeros(a::AbstractArray, ::Type{T}) where {T}               fill!(similar(a, T), 0)
@deprecate zeros(a::AbstractArray)                                    fill!(similar(a), 0)

# PR #23711
@eval LibGit2 begin
    @deprecate get_creds!(cache::CachedCredentials, credid, default) get!(cache, credid, default)
end

## goodbeye, eye!
export eye
function eye(m::Integer)
    depwarn(string("`eye(m::Integer)` has been deprecated in favor of `I` and `Matrix` ",
        "constructors. For a direct replacement, consider `Matrix(1.0I, m, m)` or ",
        "`Matrix{Float64}(I, m, m)`. If `Float64` element type is not necessary, ",
        "consider the shorter `Matrix(I, m, m)` (with default `eltype(I)` `Bool`)."), :eye)
    return Matrix{Float64}(I, m, m)
end
function eye(::Type{T}, m::Integer) where T
    depwarn(string("`eye(T::Type, m::Integer)` has been deprecated in favor of `I` and ",
        "`Matrix` constructors. For a direct replacement, consider `Matrix{T}(I, m, m)`. If ",
        "`T` element type is not necessary, consider the shorter `Matrix(I, m, m)`",
        "(with default `eltype(I)` `Bool`)"), :eye)
    return Matrix{T}(I, m, m)
end
function eye(m::Integer, n::Integer)
    depwarn(string("`eye(m::Integer, n::Integer)` has been deprecated in favor of `I` and ",
        "`Matrix` constructors. For a direct replacement, consider `Matrix(1.0I, m, n)` ",
        "or `Matrix{Float64}(I, m, n)`. If `Float64` element type is not necessary, ",
        "consider the shorter `Matrix(I, m, n)` (with default `eltype(I)` `Bool`)."), :eye)
    return Matrix{Float64}(I, m, n)
end
function eye(::Type{T}, m::Integer, n::Integer) where T
    depwarn(string("`eye(T::Type, m::Integer, n::Integer)` has been deprecated in favor of ",
        "`I` and `Matrix` constructors. For a direct replacement, consider `Matrix{T}(I, m, n)`.",
        "If `T` element type is not necessary, consider the shorter `Matrix(I, m, n)` ",
        "(with default `eltype(I)` `Bool`)."), :eye)
    return Matrix{T}(I, m, n)
end
function eye(A::AbstractMatrix{T}) where T
    depwarn(string("`eye(A::AbstractMatrix{T})` has been deprecated in favor of `I` and ",
        "`Matrix` constructors. For a direct replacement, consider `Matrix{eltype(A)}(I, size(A))`.",
        "If `eltype(A)` element type is not necessary, consider the shorter `Matrix(I, size(A))` ",
        "(with default `eltype(I)` `Bool`)."), :eye)
    return Matrix(one(T)I, size(A))
end
function eye(::Type{Diagonal{T}}, n::Int) where T
    depwarn(string("`eye(DT::Type{Diagonal{T}}, n::Int)` has been deprecated in favor of `I` ",
        "and `Diagonal` constructors. For a direct replacement, consider `Diagonal{T}(I, n)`. ",
        "If `T` element type is not necessary, consider the shorter `Diagonal(I, n)` ",
        "(with default `eltype(I)` `Bool`)."), :eye)
    return Diagonal{T}(I, n)
end
@eval Base.LinAlg import Base.eye
# @eval Base.SparseArrays import Base.eye # SparseArrays has an eye for things cholmod


export tic, toq, toc
function tic()
    depwarn("tic() is deprecated, use @time, @elapsed, or calls to time_ns() instead.", :tic)
    t0 = time_ns()
    task_local_storage(:TIMERS, (t0, get(task_local_storage(), :TIMERS, ())))
    return t0
end

function _toq()
    t1 = time_ns()
    timers = get(task_local_storage(), :TIMERS, ())
    if timers === ()
        error("toc() without tic()")
    end
    t0 = timers[1]::UInt64
    task_local_storage(:TIMERS, timers[2])
    (t1-t0)/1e9
end

function toq()
    depwarn("toq() is deprecated, use @elapsed or calls to time_ns() instead.", :toq)
    return _toq()
end

function toc()
    depwarn("toc() is deprecated, use @time, @elapsed, or calls to time_ns() instead.", :toc)
    t = _toq()
    println("elapsed time: ", t, " seconds")
    return t
end

@eval Base.SparseArrays @deprecate sparse(s::UniformScaling, m::Integer) sparse(s, m, m)

# A[I...] .= with scalar indices should modify the element at A[I...]
function Broadcast.dotview(A::AbstractArray, args::Number...)
    depwarn("the behavior of `A[I...] .= X` with scalar indices will change in the future. Use `A[I...] = X` instead.", :broadcast!)
    view(A, args...)
end
Broadcast.dotview(A::AbstractArray{<:AbstractArray}, args::Integer...) = getindex(A, args...)
# Upon removing deprecations, also enable the @testset "scalar .=" in test/broadcast.jl

# PR #23816: deprecation of gradient
export gradient
@eval Base.LinAlg begin
    export gradient

    function gradient(args...)
        Base.depwarn("gradient is deprecated and will be removed in the next release.", :gradient)
        return _gradient(args...)
    end

    _gradient(F::BitVector) = _gradient(Array(F))
    _gradient(F::BitVector, h::Real) = _gradient(Array(F), h)
    _gradient(F::Vector, h::BitVector) = _gradient(F, Array(h))
    _gradient(F::BitVector, h::Vector) = _gradient(Array(F), h)
    _gradient(F::BitVector, h::BitVector) = _gradient(Array(F), Array(h))

    function _gradient(F::AbstractVector, h::Vector)
        n = length(F)
        T = typeof(oneunit(eltype(F))/oneunit(eltype(h)))
        g = similar(F, T)
        if n == 1
            g[1] = zero(T)
        elseif n > 1
            g[1] = (F[2] - F[1]) / (h[2] - h[1])
            g[n] = (F[n] - F[n-1]) / (h[end] - h[end-1])
            if n > 2
                h = h[3:n] - h[1:n-2]
                g[2:n-1] = (F[3:n] - F[1:n-2]) ./ h
            end
        end
        g
    end

    _gradient(F::AbstractVector) = _gradient(F, [1:length(F);])
    _gradient(F::AbstractVector, h::Real) = _gradient(F, [h*(1:length(F));])
end

@noinline function getaddrinfo(callback::Function, host::AbstractString)
    depwarn("getaddrinfo with a callback function is deprecated, wrap code in @async instead for deferred execution", :getaddrinfo)
    @async begin
        r = getaddrinfo(host)
        callback(r)
    end
    nothing
end

@deprecate whos(io::IO, m::Module, pat::Regex) show(io, varinfo(m, pat))
@deprecate whos(io::IO, m::Module)             show(io, varinfo(m))
@deprecate whos(io::IO)                        show(io, varinfo())
@deprecate whos(m::Module, pat::Regex)         varinfo(m, pat)
@deprecate whos(m::Module)                     varinfo(m)
@deprecate whos(pat::Regex)                    varinfo(pat)
@deprecate whos()                              varinfo()

# indexing with A[true] will throw an argument error in the future
function to_index(i::Bool)
    depwarn("indexing with Bool values is deprecated. Convert the index to an integer first with `Int(i)`.", (:getindex, :setindex!, :view))
    convert(Int,i)::Int
end
# After deprecation is removed, enable the @testset "indexing by Bool values" in test/arrayops.jl
# Also un-comment the new definition in base/indices.jl

# deprecate odd fill! methods
@deprecate fill!(D::Diagonal, x)                       LinAlg.fillslots!(D, x)
@deprecate fill!(A::Base.LinAlg.AbstractTriangular, x) LinAlg.fillslots!(A, x)

function diagm(v::BitVector)
    depwarn(string("diagm(v::BitVector) is deprecated, use diagm(0 => v) or ",
        "BitMatrix(Diagonal(v)) instead"), :diagm)
    return BitMatrix(Diagonal(v))
end
function diagm(v::AbstractVector)
    depwarn(string("diagm(v::AbstractVector) is deprecated, use diagm(0 => v) or ",
        "Matrix(Diagonal(v)) instead"), :diagm)
    return Matrix(Diagonal(v))
end
@deprecate diagm(v::AbstractVector, k::Integer) diagm(k => v)
@deprecate diagm(x::Number) fill(x, 1, 1)

# deprecate BitArray{...}(shape...) constructors to BitArray{...}(uninitialized, shape...) equivalents
@deprecate BitArray{N}(dims::Vararg{Int,N}) where {N}   BitArray{N}(uninitialized, dims)
@deprecate BitArray(dims::NTuple{N,Int}) where {N}      BitArray(uninitialized, dims...)
@deprecate BitArray(dims::Integer...)                   BitArray(uninitialized, dims)

## deprecate full
export full
# full no-op fallback
function full(A::AbstractArray)
    depwarn(string(
        "The no-op `full(A::AbstractArray)` fallback has been deprecated, and no more ",
        "specific `full` method for $(typeof(A)) exists. Furthermore, `full` in general ",
        "has been deprecated.\n\n",
        "To replace `full(A)`, as appropriate consider dismabiguating with a concrete ",
        "array constructor (e.g. `Array(A)`), with an abstract array constructor (e.g.`AbstractArray(A)`), ",
        "instead `convert`ing to an array type (e.g `convert(Array, A)`, `convert(AbstractArray, A)`), ",
        "or using another such operation that addresses your specific use case."),  :full)
    return A
end

# full for structured arrays
function full(A::Union{Diagonal,Bidiagonal,Tridiagonal,SymTridiagonal})
    mattypestr = isa(A, Diagonal)        ? "Diagonal"        :
                 isa(A, Bidiagonal)      ? "Bidiagonal"      :
                 isa(A, Tridiagonal)     ? "Tridiagonal"     :
                 isa(A, SymTridiagonal)  ? "SymTridiagonal"  :
                    error("should not be reachable!")
    depwarn(string(
        "`full(A::$(mattypestr))` (and `full` in general) has been deprecated. ",
        "To replace `full(A::$(mattypestr))`, consider `Matrix(A)` or, if that ",
        "option is too narrow, `Array(A)`. Also consider `SparseMatrixCSC(A)` ",
        "or, if that option is too narrow, `sparse(A)`."),  :full)
    return Matrix(A)
end

# full for sparse arrays
function full(S::Union{SparseVector,SparseMatrixCSC})
    (arrtypestr, desttypestr) =
        isa(S, SparseVector)    ? ("SparseVector",    "Vector") :
        isa(S, SparseMatrixCSC) ? ("SparseMatrixCSC", "Matrix") :
            error("should not be reachable!")
    depwarn(string(
        "`full(S::$(arrtypestr))` (and `full` in general) has been deprecated. ",
        "To replace `full(S::$(arrtypestr))`, consider `$(desttypestr)(S)` or, ",
        "if that option is too narrow, `Array(S)`."), :full)
    return Array(S)
end

# full for factorizations
function full(F::Union{LinAlg.LU,LinAlg.LQ,LinAlg.QR,LinAlg.QRPivoted,LinAlg.QRCompactWY,
                        LinAlg.SVD,LinAlg.LDLt,LinAlg.Schur,LinAlg.Eigen,LinAlg.Hessenberg,
                        LinAlg.Cholesky,LinAlg.CholeskyPivoted})
    facttypestr = isa(F, LinAlg.LU)               ? "LU"              :
                  isa(F, LinAlg.LQ)               ? "LQ"              :
                  isa(F, LinAlg.QR)               ? "QR"              :
                  isa(F, LinAlg.QRPivoted)        ? "QRPivoted"       :
                  isa(F, LinAlg.QRCompactWY)      ? "QRCompactWY"     :
                  isa(F, LinAlg.SVD)              ? "SVD"             :
                  isa(F, LinAlg.LDLt)             ? "LDLt"            :
                  isa(F, LinAlg.Schur)            ? "Schur"           :
                  isa(F, LinAlg.Eigen)            ? "Eigen"           :
                  isa(F, LinAlg.Hessenberg)       ? "Hessenberg"      :
                  isa(F, LinAlg.Cholesky)         ? "Cholesky"        :
                  isa(F, LinAlg.CholeskyPivoted)  ? "CholeskyPivoted" :
                      error("should not be reachable!")
   depwarn(string(
       "`full(F::$(facttypestr))` (and `full` in general) has been deprecated. ",
       "To replace `full(F::$(facttypestr))`, consider `Matrix(F)`, `AbstractMatrix(F)` or, ",
       "if those options are too narrow, `Array(F)` or `AbstractArray(F)`."), :full)
   return AbstractMatrix(F)
end

# full for implicit orthogonal factors
function full(Q::LinAlg.HessenbergQ)
    depwarn(string(
        "`full(Q::HessenbergQ)` (and `full` in general) has been deprecated. ",
        "To replace `full(Q::HessenbergQ)`, consider `Matrix(Q)` or, ",
        "if that option is too narrow, `Array(Q)`."), :full)
    return Matrix(Q)
end
function full(Q::LinAlg.LQPackedQ; thin::Bool = true)
    depwarn(string(
        "`full(Q::LQPackedQ; thin::Bool = true)` (and `full` in general) ",
        "has been deprecated. To replace `full(Q::LQPackedQ, true)`, ",
        "consider `Matrix(Q)` or `Array(Q)`. To replace `full(Q::LQPackedQ, false)`, ",
        "consider `Base.LinAlg.A_mul_B!(Q, Matrix{eltype(Q)}(I, size(Q.factors, 2), size(Q.factors, 2)))`."), :full)
    return thin ? Array(Q) : A_mul_B!(Q, Matrix{eltype(Q)}(I, size(Q.factors, 2), size(Q.factors, 2)))
end
function full(Q::Union{LinAlg.QRPackedQ,LinAlg.QRCompactWYQ}; thin::Bool = true)
    qtypestr = isa(Q, LinAlg.QRPackedQ)    ? "QRPackedQ"    :
               isa(Q, LinAlg.QRCompactWYQ) ? "QRCompactWYQ" :
                  error("should not be reachable!")
    depwarn(string(
        "`full(Q::$(qtypestr); thin::Bool = true)` (and `full` in general) ",
        "has been deprecated. To replace `full(Q::$(qtypestr), true)`, ",
        "consider `Matrix(Q)` or `Array(Q)`. To replace `full(Q::$(qtypestr), false)`, ",
        "consider `Base.LinAlg.A_mul_B!(Q, Matrix{eltype(Q)}(I, size(Q.factors, 1), size(Q.factors, 1)))`."), :full)
    return thin ? Array(Q) : A_mul_B!(Q, Matrix{eltype(Q)}(I, size(Q.factors, 1), size(Q.factors, 1)))
end

# full for symmetric / hermitian / triangular wrappers
function full(A::Symmetric)
    depwarn(string(
        "`full(A::Symmetric)` (and `full` in general) has been deprecated. ",
        "To replace `full(A::Symmetric)`, as appropriate consider `Matrix(A)`, ",
        "`Array(A)`, `SparseMatrixCSC(A)`, `sparse(A)`, `copy!(similar(parent(A)), A)`, ",
        "or `Base.LinAlg.copytri!(copy(parent(A)), A.uplo)`."), :full)
    return Matrix(A)
end
function full(A::Hermitian)
    depwarn(string(
        "`full(A::Hermitian)` (and `full` in general) has been deprecated. ",
        "To replace `full(A::Hermitian)`, as appropriate consider `Matrix(A)`, ",
        "`Array(A)`, `SparseMatrixCSC(A)`, `sparse(A)`, `copy!(similar(parent(A)), A)`, ",
        "or `Base.LinAlg.copytri!(copy(parent(A)), A.uplo, true)`."), :full)
    return Matrix(A)
end
function full(A::Union{UpperTriangular,LowerTriangular})
    (tritypestr, tri!str) =
        isa(A, UpperTriangular) ? ("UpperTriangular", "triu!") :
        isa(A, LowerTriangular) ? ("LowerTriangular", "tril!") :
            error("should not be reachable!")
    depwarn(string(
        "`full(A::$(tritypestr))` (and `full` in general) has been deprecated. ",
        "To replace `full(A::$(tritypestr))`, as appropriate consider `Matrix(A)`, ",
        "`Array(A)`, `SparseMatrixCSC(A)`, `sparse(A)`, `copy!(similar(parent(A)), A)`, ",
        "or `$(tri!str)(copy(parent(A)))`."), :full)
    return Matrix(A)
end
function full(A::Union{LinAlg.UnitUpperTriangular,LinAlg.UnitLowerTriangular})
    tritypestr = isa(A, LinAlg.UnitUpperTriangular) ? "LinAlg.UnitUpperTriangular" :
                 isa(A, LinAlg.UnitLowerTriangular) ? "LinAlg.UnitLowerTriangular" :
                     error("should not be reachable!")
    depwarn(string(
        "`full(A::$(tritypestr))` (and `full` in general) has been deprecated. ",
        "To replace `full(A::$(tritypestr))`, as appropriate consider `Matrix(A)`, ",
        "`Array(A)`, `SparseMatrixCSC(A)`, `sparse(A)`, or `copy!(similar(parent(A)), A)`."), :full)
    return Matrix(A)
end


# issue #20816
@deprecate strwidth textwidth
@deprecate charwidth textwidth

# TODO: after 0.7, remove thin keyword argument and associated logic from...
# (1) base/linalg/svd.jl
# (2) base/linalg/qr.jl
# (3) base/linalg/lq.jl

@deprecate find(x::Number)            find(!iszero, x)
@deprecate findnext(A, v, i::Integer) findnext(equalto(v), A, i)
@deprecate findfirst(A, v)            findfirst(equalto(v), A)
@deprecate findprev(A, v, i::Integer) findprev(equalto(v), A, i)
@deprecate findlast(A, v)             findlast(equalto(v), A)
# also remove deprecation warnings in find* functions in array.jl, sparse/sparsematrix.jl,
# and sparse/sparsevector.jl.

# issue #22849
@deprecate reinterpret(::Type{T}, a::Array{S}, dims::NTuple{N,Int}) where {T, S, N} reshape(reinterpret(T, vec(a)), dims)
@deprecate reinterpret(::Type{T}, a::SparseMatrixCSC{S}, dims::NTuple{N,Int}) where {T, S, N} reinterpret(T, reshape(a, dims))
@deprecate reinterpret(::Type{T}, a::ReinterpretArray{S}, dims::NTuple{N,Int}) where {T, S, N} reshape(reinterpret(T, vec(a)), dims)

# issue #24006
@deprecate linearindices(s::AbstractString) eachindex(s)

# deprecate Array(shape...)-like constructors to Array(uninitialized, shape...) equivalents
# --> former primitive constructors
@deprecate Array{T,1}(m::Int) where {T}                      Array{T,1}(uninitialized, m)
@deprecate Array{T,2}(m::Int, n::Int) where {T}              Array{T,2}(uninitialized, m, n)
@deprecate Array{T,3}(m::Int, n::Int, o::Int) where {T}      Array{T,3}(uninitialized, m, n, o)
@deprecate Array{T,N}(d::Vararg{Int,N}) where {T,N}          Array{T,N}(uninitialized, d)
@deprecate Array{T,N}(d::NTuple{N,Int}) where {T,N}          Array{T,N}(uninitialized, d)
@deprecate Array{T}(m::Int) where {T}                        Array{T}(uninitialized, m)
@deprecate Array{T}(m::Int, n::Int) where {T}                Array{T}(uninitialized, m, n)
@deprecate Array{T}(m::Int, n::Int, o::Int) where {T}        Array{T}(uninitialized, m, n, o)
@deprecate Array{T}(d::NTuple{N,Int}) where {T,N}            Array{T}(uninitialized, d)
# --> former convenience constructors
@deprecate Vector{T}(m::Integer) where {T}                          Vector{T}(uninitialized, m)
@deprecate Matrix{T}(m::Integer, n::Integer) where {T}              Matrix{T}(uninitialized, m, n)
@deprecate Array{T}(m::Integer) where {T}                           Array{T}(uninitialized, m)
@deprecate Array{T}(m::Integer, n::Integer) where {T}               Array{T}(uninitialized, m, n)
@deprecate Array{T}(m::Integer, n::Integer, o::Integer) where {T}   Array{T}(uninitialized, m, n, o)
@deprecate Array{T}(d::Integer...) where {T}                        Array{T}(uninitialized, d)
@deprecate Vector(m::Integer)                                       Vector(uninitialized, m)
@deprecate Matrix(m::Integer, n::Integer)                           Matrix(uninitialized, m, n)

# deprecate IntSet to BitSet
@deprecate_binding IntSet BitSet

# Issue 24219
@deprecate float(x::AbstractString) parse(Float64, x)
@deprecate float(a::AbstractArray{<:AbstractString}) parse.(Float64, a)

# deprecate bits to bitstring (#24263, #24281)
@deprecate bits bitstring

# deprecate speye
export speye
function speye(n::Integer)
    depwarn(string("`speye(n::Integer)` has been deprecated in favor of `I`, `sparse`, and ",
                    "`SparseMatrixCSC` constructor methods. For a direct replacement, consider ",
                    "`sparse(1.0I, n, n)`, `SparseMatrixCSC(1.0I, n, n)`, or `SparseMatrixCSC{Float64}(I, n, n)`. ",
                    "If `Float64` element type is not necessary, consider the shorter `sparse(I, n, n)` ",
                    "or `SparseMatrixCSC(I, n, n)` (with default `eltype(I)` of `Bool`)."), :speye)
    return sparse(1.0I, n, n)
end
function speye(m::Integer, n::Integer)
    depwarn(string("`speye(m::Integer, n::Integer)` has been deprecated in favor of `I`, ",
                    "`sparse`, and `SparseMatrixCSC` constructor methods. For a direct ",
                    "replacement, consider `sparse(1.0I, m, n)`, `SparseMatrixCSC(1.0I, m, n)`, ",
                    "or `SparseMatrixCSC{Float64}(I, m, n)`. If `Float64` element type is not ",
                    " necessary, consider the shorter `sparse(I, m, n)` or `SparseMatrixCSC(I, m, n)` ",
                    "(with default `eltype(I)` of `Bool`)."), :speye)
    return sparse(1.0I, m, n)
end
function speye(::Type{T}, n::Integer) where T
    depwarn(string("`speye(T, n::Integer)` has been deprecated in favor of `I`, `sparse`, and ",
                    "`SparseMatrixCSC` constructor methods. For a direct replacement, consider ",
                    "`sparse(T(1)I, n, n)` if `T` is concrete or `SparseMatrixCSC{T}(I, n, n)` ",
                    "if `T` is either concrete or abstract. If element type `T` is not necessary, ",
                    "consider the shorter `sparse(I, n, n)` or `SparseMatrixCSC(I, n, n)` ",
                    "(with default `eltype(I)` of `Bool`)."), :speye)
    return SparseMatrixCSC{T}(I, n, n)
end
function speye(::Type{T}, m::Integer, n::Integer) where T
    depwarn(string("`speye(T, m::Integer, n::Integer)` has been deprecated in favor of `I`, ",
                    "`sparse`, and `SparseMatrixCSC` constructor methods. For a direct ",
                    "replacement, consider `sparse(T(1)I, m, n)` if `T` is concrete or ",
                    "`SparseMatrixCSC{T}(I, m, n)` if `T` is either concrete or abstract. ",
                    "If element type `T` is not necessary, consider the shorter ",
                    "`sparse(I, m, n)` or `SparseMatrixCSC(I, m, n)` (with default `eltype(I)` ",
                    "of `Bool`)."), :speye)
    return SparseMatrixCSC{T}(I, m, n)
end
function speye(S::SparseMatrixCSC{T}) where T
    depwarn(string("`speye(S::SparseMatrixCSC{T})` has been deprecated in favor of `I`, ",
                    "`sparse`, and `SparseMatrixCSC` constructor methods. For a direct ",
                    "replacement, consider `sparse(T(1)I, size(S)...)` if `T` is concrete or ",
                    "`SparseMatrixCSC{eltype(S)}(I, size(S))` if `T` is either concrete or abstract. ",
                    "If preserving element type `T` is not necessary, consider the shorter ",
                    "`sparse(I, size(S)...)` or `SparseMatrixCSC(I, size(S))` (with default ",
                    "`eltype(I)` of `Bool`)."), :speye)
    return SparseMatrixCSC{T}(I, m, n)
end

# issue #24167
@deprecate EnvHash EnvDict

# issue #24349
@deprecate parse(str::AbstractString; kwargs...) Meta.parse(str; kwargs...)
@deprecate parse(str::AbstractString, pos::Int, ; kwargs...) Meta.parse(str, pos; kwargs...)
@deprecate_binding ParseError Meta.ParseError

@eval LinAlg begin
    @deprecate chol!(x::Number, uplo) chol(x) false
end

# deprecate RowVector{T}(shape...) constructors to RowVector{T}(uninitialized, shape...) equivalents
@deprecate RowVector{T}(n::Int) where {T}               RowVector{T}(uninitialized, n)
@deprecate RowVector{T}(n1::Int, n2::Int) where {T}     RowVector{T}(uninitialized, n1, n2)
@deprecate RowVector{T}(n::Tuple{Int}) where {T}        RowVector{T}(uninitialized, n)
@deprecate RowVector{T}(n::Tuple{Int,Int}) where {T}    RowVector{T}(uninitialized, n)

@deprecate cumsum(A::AbstractArray)     cumsum(A, 1)
@deprecate cumprod(A::AbstractArray)    cumprod(A, 1)

# issue #16307
@deprecate finalizer(o, f::Function) finalizer(f, o)
# This misses other callables but they are very rare in the wild
@deprecate finalizer(o, f::Ptr{Void}) finalizer(f, o)

# Avoid ambiguity, can remove when deprecations are removed:
# This is almost certainly going to be a silent failure for code that is not updated.
finalizer(f::Ptr{Void}, o::Ptr{Void}) = invoke(finalizer, Tuple{Ptr{Void}, Any}, f, o)
finalizer(f::Ptr{Void}, o::Function) = invoke(finalizer, Tuple{Ptr{Void}, Any}, f, o)

# Broadcast extension API (#23939)
@eval Broadcast begin
    Base.@deprecate_binding containertype combine_styles false
    Base.@deprecate_binding _containertype BroadcastStyle false
    Base.@deprecate_binding promote_containertype BroadcastStyle false
    Base.@deprecate_binding broadcast_c! broadcast! false ", broadcast_c!(f, ::Type, ::Type, C, As...) should become broadcast!(f, C, As...) (see the manual chapter Interfaces)"
    Base.@deprecate_binding broadcast_c broadcast false ", `broadcast_c(f, ::Type{C}, As...)` should become `broadcast(f, C, nothing, nothing, As...))` (see the manual chapter Interfaces)"
    Base.@deprecate_binding broadcast_t broadcast false ", broadcast_t(f, ::Type{ElType}, shape, iter, As...)` should become `broadcast(f, Broadcast.DefaultArrayStyle{N}(), ElType, shape, As...))` (see the manual chapter Interfaces)"
end

# issue #24822
@deprecate_binding Display AbstractDisplay

# 24595
@deprecate falses(A::AbstractArray) falses(size(A))
@deprecate trues(A::AbstractArray) trues(size(A))

# issue #24794
@deprecate linspace(start, stop)     linspace(start, stop, 50)
@deprecate logspace(start, stop)     logspace(start, stop, 50)

@deprecate merge!(repo::LibGit2.GitRepo, args...; kwargs...) LibGit2.merge!(repo, args...; kwargs...)

# issue #24019
@deprecate similar(a::Associative) empty(a)
@deprecate similar(a::Associative, ::Type{Pair{K,V}}) where {K, V} empty(a, K, V)

# PR #24594
@eval LibGit2 begin
    @deprecate AbstractCredentials AbstractCredential false
    @deprecate UserPasswordCredentials UserPasswordCredential false
    @deprecate SSHCredentials SSHCredential false
end

# issue #24804
@deprecate_moved sum_kbn "KahanSummation"
@deprecate_moved cumsum_kbn "KahanSummation"

# END 0.7 deprecations

# BEGIN 1.0 deprecations

# END 1.0 deprecations
