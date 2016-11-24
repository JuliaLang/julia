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

macro deprecate(old,new)
    meta = Expr(:meta, :noinline)
    if isa(old,Symbol)
        oldname = Expr(:quote,old)
        newname = Expr(:quote,new)
        Expr(:toplevel,
            Expr(:export,esc(old)),
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
        ln = Int(unsafe_load(cglobal(:jl_lineno, Cint)))
        fn = unsafe_string(unsafe_load(cglobal(:jl_filename, Ptr{Cchar})))
        bt = backtrace()
        caller = firstcaller(bt, funcsym)
        if opts.depwarn == 1 # raise a warning
            warn(msg, once=(caller != C_NULL), key=caller, bt=bt,
                 filename=fn, lineno=ln)
        elseif opts.depwarn == 2 # raise an error
            throw(ErrorException(msg))
        end
    end
    nothing
end

function firstcaller(bt::Array{Ptr{Void},1}, funcsym::Symbol)
    # Identify the calling line
    i = 1
    while i <= length(bt)
        lkups = StackTraces.lookup(bt[i])
        i += 1
        for lkup in lkups
            if lkup === StackTraces.UNKNOWN
                continue
            end
            if lkup.func == funcsym
                @goto found
            end
        end
    end
    @label found
    if i <= length(bt)
        return bt[i]
    end
    return C_NULL
end

deprecate(s::Symbol) = deprecate(current_module(), s)
deprecate(m::Module, s::Symbol) = ccall(:jl_deprecate_binding, Void, (Any, Any), m, s)

macro deprecate_binding(old, new)
    Expr(:toplevel,
         Expr(:export, esc(old)),
         Expr(:const, Expr(:(=), esc(old), esc(new))),
         Expr(:call, :deprecate, Expr(:quote, old)))
end

# 0.5 deprecations

for f in (:remotecall, :remotecall_fetch, :remotecall_wait)
    @eval begin
        @deprecate ($f)(w::LocalProcess, f::Function, args...)    ($f)(f, w::LocalProcess, args...)
        @deprecate ($f)(w::Worker, f::Function, args...)          ($f)(f, w::Worker, args...)
        @deprecate ($f)(id::Integer, f::Function, args...)        ($f)(f, id::Integer, args...)
    end
end

# 13232
@deprecate with_bigfloat_precision setprecision
@deprecate set_bigfloat_precision(prec) setprecision(prec)
@deprecate get_bigfloat_precision() precision(BigFloat)

@deprecate set_rounding setrounding
@deprecate with_rounding setrounding
@deprecate get_rounding rounding

#13465
#@deprecate cov(x::AbstractVector; corrected=true, mean=Base.mean(x)) Base.covm(x, mean, corrected)
#@deprecate cov(X::AbstractMatrix; vardim=1, corrected=true, mean=Base.mean(X, vardim)) Base.covm(X, mean, vardim, corrected)
#@deprecate cov(x::AbstractVector, y::AbstractVector; corrected=true, mean=(Base.mean(x), Base.mean(y))) Base.covm(x, mean[1], y, mean[2], corrected)
@deprecate cov(X::AbstractVecOrMat, Y::AbstractVecOrMat; vardim=1, corrected=true, mean=(Base.mean(X, vardim), Base.mean(Y, vardim))) Base.covm(X, mean[1], Y, mean[2], vardim, corrected)

#@deprecate cor(x::AbstractVector; mean=Base.mean(x)) Base.corm(x, mean)
#@deprecate cor(X::AbstractMatrix; vardim=1, mean=Base.mean(X, vardim)) Base.corm(X, mean, vardim)
#@deprecate cor(x::AbstractVector, y::AbstractVector; mean=(Base.mean(x), Base.mean(y))) Base.corm(x, mean[1], y, mean[2])
@deprecate cor(X::AbstractVecOrMat, Y::AbstractVecOrMat; vardim=1, mean=(Base.mean(X, vardim), Base.mean(Y, vardim))) Base.corm(X, mean[1], Y, mean[2], vardim)

@deprecate_binding SparseMatrix SparseArrays

#13496
@deprecate A_ldiv_B!(A::SparseMatrixCSC, B::StridedVecOrMat) A_ldiv_B!(factorize(A), B)

@deprecate chol(A::Number, ::Type{Val{:U}})         chol(A)
@deprecate chol(A::AbstractMatrix, ::Type{Val{:U}}) chol(A)
@deprecate chol(A::Number, ::Type{Val{:L}})         ctranspose(chol(A))
@deprecate chol(A::AbstractMatrix, ::Type{Val{:L}}) ctranspose(chol(A))


# Number updates

# rem1 is inconsistent for x==0: The result should both have the same
# sign as x, and should be non-zero.
function rem1{T<:Real}(x::T, y::T)
    depwarn("`rem1(x,y)` is discontinued, as it cannot be defined consistently for `x==0`. Rewrite the expression using `mod1` instead.", :rem1)
    rem(x-1,y)+1
end
rem1(x::Real, y::Real) = rem1(promote(x,y)...)
export rem1

# Filesystem module updates

@deprecate_binding FS Filesystem

isreadable(path...) = isreadable(stat(path...))
iswritable(path...) = iswritable(stat(path...))
isexecutable(path...) = isexecutable(stat(path...))
function isreadable(st::Filesystem.StatStruct)
    depwarn("isreadable is deprecated as it implied that the file would actually be readable by the user; consider using `isfile` instead. see also the system man page for `access`", :isreadable)
    return (st.mode & 0o444) > 0
end
function iswritable(st::Filesystem.StatStruct)
    depwarn("iswritable is deprecated as it implied that the file would actually be writable by the user; consider using `isfile` instead. see also the system man page for `access`", :iswritable)
    return (st.mode & 0o222) > 0
end
function isexecutable(st::Filesystem.StatStruct)
    depwarn("isexecutable is deprecated as it implied that the file would actually be executable by the user; consider using `isfile` instead. see also the system man page for `access`", :isexecutable)
    return (st.mode & 0o111) > 0
end
export isreadable, iswritable, isexecutable

@deprecate RemoteRef RemoteChannel

function tty_size()
    depwarn("tty_size is deprecated. use `displaysize(io)` as a replacement", :tty_size)
    if isdefined(Base, :active_repl)
        os = REPL.outstream(Base.active_repl)
        if isa(os, Terminals.TTYTerminal)
            return displaysize(os)
        end
    end
    if isdefined(Base, :STDOUT)
        return displaysize(STDOUT)
    end
    return displaysize()
end

#14335
@deprecate super(T::DataType) supertype(T)

function with_output_limit(thk, lim::Bool=true) # thk is usually show()
    depwarn("with_output_limit is deprecated. use `io = IOContext(io, :limit => lim)` as a replacement", :with_output_limit)
    global _limit_output
    last = _limit_output
    _limit_output = lim
    try
        thk()
    finally
        _limit_output = last
    end
end

#14555
@deprecate_binding Coff_t Int64
@deprecate_binding FileOffset Int64

#14474
macro boundscheck(yesno,blk)
    depwarn("The meaning of `@boundscheck` has changed. It now indicates that the provided code block performs bounds checking, and may be elided when inbounds.", Symbol("@boundscheck"))
    if yesno === true
        :(@inbounds $(esc(blk)))
    end
end


@deprecate parseip(str::AbstractString) parse(IPAddr, str)

#https://github.com/JuliaLang/julia/issues/14608
@deprecate readall readstring
@deprecate readbytes read

@deprecate field_offset(x::DataType, idx) fieldoffset(x, idx+1)
@noinline function fieldoffsets(x::DataType)
    depwarn("fieldoffsets is deprecated. use `map(idx->fieldoffset(x, idx), 1:nfields(x))` instead", :fieldoffsets)
    nf = nfields(x)
    offsets = Array{Int}(nf)
    for i = 1:nf
        offsets[i] = fieldoffset(x, i)
    end
    return offsets
end
export fieldoffsets

# 14766
@deprecate write(io::IO, p::Ptr, nb::Integer) unsafe_write(io, p, nb)

@deprecate(isgeneric(f), isa(f, Function))

# need to do this manually since the front end deprecates method defs of `call`
const call = @eval function(f, args...; kw...)
    $(Expr(:meta, :noinline))
    depwarn("call(f,args...) is deprecated, use f(args...) instead.", :call)
    f(args...; kw...)
end
export call

# Changed issym to issymmetric. #15192
@deprecate issym issymmetric

# 15258
@deprecate scale(α::Number, A::AbstractArray) α*A
@deprecate scale(A::AbstractArray, α::Number) A*α
@deprecate scale(A::AbstractMatrix, x::AbstractVector) A*Diagonal(x)
@deprecate scale(x::AbstractVector, A::AbstractMatrix) Diagonal(x)*A

# 1933
@deprecate_binding SingleAsyncWork AsyncCondition

# #12872
@deprecate istext istextmime

# 15692
typealias Func{N} Function
deprecate(:Func)
for (Fun, func) in [(:IdFun, :identity),
                    (:AbsFun, :abs),
                    (:Abs2Fun, :abs2),
                    (:ExpFun, :exp),
                    (:LogFun, :log),
                    (:ConjFun, :conj),
                    (:AndFun, :&),
                    (:OrFun, :|),
                    (:XorFun, :xor),
                    (:AddFun, :+),
                    # (:DotAddFun, :.+),
                    (:SubFun, :-),
                    # (:DotSubFun, :.-),
                    (:MulFun, :*),
                    # (:DotMulFun, :.*),
                    (:RDivFun, :/),
                    # (:DotRDivFun, :./),
                    (:LDivFun, :\),
                    (:IDivFun, :div),
                    # (:DotIDivFun, :.÷),
                    (:ModFun, :mod),
                    (:RemFun, :rem),
                    # (:DotRemFun, :.%),
                    (:PowFun, :^),
                    (:MaxFun, :scalarmax),
                    (:MinFun, :scalarmin),
                    (:LessFun, :<),
                    (:MoreFun, :>),
                    # (:DotLSFun, :.<<),
                    # (:DotRSFun, :.>>),
                    (:ElementwiseMaxFun, :max),
                    (:ElementwiseMinFun, :min),
                    (:ComplexFun, :complex),
                    (:DotFun, :dot),
                    ]
    @eval begin
        @deprecate_binding $(Fun) typeof($(func))
        (::Type{typeof($(func))})() = $(func)
    end
end
@deprecate_binding CentralizedAbs2Fun typeof(centralizedabs2fun(0)).name.wrapper
(::Type{typeof(centralizedabs2fun(0)).name.wrapper})(m::Number) = centralizedabs2fun(m)
@deprecate specialized_unary(f::Function) f
@deprecate specialized_binary(f::Function) f
@deprecate specialized_bitwise_unary(f::Function) f
@deprecate specialized_bitwise_binary(f::Function) f

@deprecate bitunpack(B::BitArray) Array(B)
@deprecate bitpack(A::AbstractArray) BitArray(A)

# #4163
@deprecate xdump dump

@deprecate copy(x::AbstractString)  identity(x)
@deprecate copy(x::Tuple)  identity(x)

@deprecate sprandbool(m::Integer, n::Integer, density::AbstractFloat) sprand(Bool, m, n, density)
@deprecate sprandbool(r::AbstractRNG, m::Integer, n::Integer, density::AbstractFloat) sprand(r, Bool, m, n, density)
@deprecate sprandbool(n::Integer, density::AbstractFloat) sprand(Bool, n, density)
@deprecate sprandbool(r::AbstractRNG, n::Integer, density::AbstractFloat) sprand(r, Bool, n, density)
@deprecate sprand{T}(n::Integer, density::AbstractFloat, ::Type{T}) sprand(T, n, density)
@deprecate sprand{T}(r::AbstractRNG, n::Integer, density::AbstractFloat, ::Type{T}) sprand(r, T, n, density)

#15995
@deprecate symbol Symbol

#15032: Expressions like Base.(:+) now call broadcast.  Since calls
#       to broadcast(x, ::Symbol) are unheard of, and broadcast(x, ::Integer)
#       are unlikely, we can treat these as deprecated getfield calls.
#       (See julia-syntax.scm for the Base.(:+)(...) = ... deprecation.)
function broadcast(x::Any, i::Union{Integer,Symbol})
    depwarn("x.(i) is deprecated; use getfield(x, i) instead.", :broadcast)
    getfield(x, i)
end
# clearer to be more explicit in the warning for the Module case
function broadcast(m::Module, s::Symbol)
    S = repr(s) # 16295
    depwarn("$m.($S) is deprecated; use $m.$S or getfield($m, $S) instead.", :broadcast)
    getfield(m, s)
end
# expressions like f.(3) should still call broadcast for f::Function,
# and in general broadcast should work for scalar arguments, while
# getfield is certainly not intended for the case of f::Function.
broadcast(f::Function, i::Integer) = f(i)

#16167
macro ccallable(def)
    depwarn("@ccallable requires a return type", Symbol("@ccallable"))
    if isa(def,Expr) && (def.head === :(=) || def.head === :function)
        sig = def.args[1]
        if sig.head === :call
            name = sig.args[1]
            at = map(sig.args[2:end]) do a
                if isa(a,Expr) && a.head === :(::)
                    a.args[2]
                else
                    :Any
                end
            end
            return quote
                $(esc(def))
                let name = $(esc(name)), tt = $(Expr(:curly, :Tuple, map(esc, at)...))
                    rt = return_types(name, tt)
                    length(rt) == 1 || error("function not ccallable")
                    ccallable(name, rt[1], tt)
                end
            end
        end
    end
    error("expected method definition in @ccallable")
end

@deprecate_binding ASCIIString String
@deprecate_binding UTF8String String
@deprecate_binding ByteString String

@deprecate utf8(p::Ptr{UInt8}, len::Integer) unsafe_string(p, len)
@deprecate utf8(p::Ptr{UInt8}) unsafe_string(p)
@deprecate utf8(v::Vector{UInt8}) String(v)
@deprecate utf8(s::AbstractString) String(s)
@deprecate utf8(x) convert(String, x)

@deprecate ascii(p::Ptr{UInt8}, len::Integer) ascii(unsafe_string(p, len))
@deprecate ascii(p::Ptr{UInt8}) ascii(unsafe_string(p))
@deprecate ascii(v::Vector{UInt8}) ascii(String(v))
@deprecate ascii(x) ascii(convert(String, x))

@deprecate bytestring(s::Cstring) unsafe_string(s)
@deprecate bytestring(v::Vector{UInt8}) String(copy(v))
@deprecate bytestring(io::Base.AbstractIOBuffer) String(io)
@deprecate bytestring(p::Union{Ptr{Int8},Ptr{UInt8}}) unsafe_string(p)
@deprecate bytestring(p::Union{Ptr{Int8},Ptr{UInt8}}, len::Integer) unsafe_string(p,len)
@deprecate bytestring(s::AbstractString...) string(s...)
@deprecate String(s::Cstring) unsafe_string(s)
@deprecate String(p::Union{Ptr{Int8},Ptr{UInt8}}) unsafe_string(p)
@deprecate String(p::Union{Ptr{Int8},Ptr{UInt8}}, len::Integer) unsafe_string(p,len)

@deprecate(
    convert(::Type{String}, a::Vector{UInt8}, invalids_as::AbstractString),
    let a = a, invalids_as = invalids_as
        l = length(a)
        idx = 1
        iscopy = false
        while idx <= l
            if !is_valid_continuation(a[idx])
                nextidx = idx+1+utf8_trailing[a[idx]+1]
                (nextidx <= (l+1)) && (idx = nextidx; continue)
            end
            !iscopy && (a = copy(a); iscopy = true)
            endn = idx
            while endn <= l
                !is_valid_continuation(a[endn]) && break
                endn += 1
            end
            (endn > idx) && (endn -= 1)
            splice!(a, idx:endn, Vector{UInt8}(invalids_as))
            l = length(a)
        end
        String(a)
    end
)

@deprecate ==(x::Char, y::Integer) UInt32(x) == y
@deprecate ==(x::Integer, y::Char) x == UInt32(y)
# Note: when these deprecations are deleted, the specialized definitions isequal(x::Char, y::Integer)
# and isequal(x::Integer, y::Char) in operators.jl can be deleted, too
@deprecate isless(x::Char, y::Integer) UInt32(x) < y
@deprecate isless(x::Integer, y::Char) x < UInt32(y)


#6674 and #4233
macro windows(qm,ex)
    depwarn("`@windows` is deprecated, use `@static is_windows()` instead", Symbol("@windows"))
    return @static is_windows() ? esc(ex.args[1]) : esc(ex.args[2])
end
macro unix(qm,ex)
    depwarn("`@unix` is deprecated, use `@static is_unix()` instead", Symbol("@unix"))
    return @static is_unix() ? esc(ex.args[1]) : esc(ex.args[2])
end
macro osx(qm,ex)
    depwarn("`@osx` is deprecated, use `@static is_apple()` instead", Symbol("@osx"))
    return @static is_apple() ? esc(ex.args[1]) : esc(ex.args[2])
end
macro linux(qm,ex)
    depwarn("`@linux` is deprecated, use `@static is_linux()` instead", Symbol("@linux"))
    return @static is_linux() ? esc(ex.args[1]) : esc(ex.args[2])
end
macro windows_only(ex)
    depwarn("`@windows_only` is deprecated, use `@static if is_windows()` instead", Symbol("@windows_only"))
    return @static if is_windows() esc(ex) end
end
macro unix_only(ex)
    depwarn("`@unix_only` is deprecated, use `@static if is_unix()` instead", Symbol("@unix_only"))
    return @static if is_unix() esc(ex) end
end
macro osx_only(ex)
    depwarn("`@osx_only` is deprecated, use `@static if is_apple()` instead", Symbol("@osx_only"))
    return @static if is_apple() esc(ex) end
end
macro linux_only(ex)
    depwarn("`@linux_only` is deprecated, use `@static if is_linux()` instead", Symbol("@linux_only"))
    return @static if is_linux() esc(ex) end
end
export
    @windows,
    @unix,
    @osx,
    @linux,
    @windows_only,
    @unix_only,
    @osx_only,
    @linux_only

export OS_NAME
const OS_NAME =
    if Sys.KERNEL === :NT
        :Windows
    else
        Sys.KERNEL
    end
deprecate(:OS_NAME) # use Sys.KERNEL now

export CPU_CORES
function _set_CPU_CORES()
    global const CPU_CORES = Sys.CPU_CORES
    deprecate(Base, :CPU_CORES)
end
module Init_CPU_CORES
    const __init__ = Base._set_CPU_CORES
end

@deprecate_binding WORD_SIZE Sys.WORD_SIZE

@deprecate showcompact_lim show
@deprecate_binding writemime show

@deprecate blas_set_num_threads BLAS.set_num_threads

@deprecate print_escaped escape_string
@deprecate print_unescaped unescape_string
@deprecate print_joined join

@deprecate broadcast!_function(f) (B, As...) -> broadcast!(f, B, As...)
@deprecate broadcast_function(f)  (As...) -> broadcast(f, As...)

##### histogram #####

## nice-valued ranges for histograms
export hist, hist!, hist2d, hist2d!, histrange

function histrange{T<:AbstractFloat,N}(v::AbstractArray{T,N}, n::Integer)
    depwarn("histrange(...) is deprecated, use StatsBase.histrange(...) instead",:histrange)
    nv = length(v)
    if nv == 0 && n < 0
        throw(ArgumentError("number of bins must be ≥ 0 for an empty array, got $n"))
    elseif nv > 0 && n < 1
        throw(ArgumentError("number of bins must be ≥ 1 for a non-empty array, got $n"))
    end
    if nv == 0
        return 0.0:1.0:0.0
    end
    lo, hi = extrema(v)
    if hi == lo
        step = 1.0
    else
        bw = (hi - lo) / n
        e = 10.0^floor(log10(bw))
        r = bw / e
        if r <= 2
            step = 2*e
        elseif r <= 5
            step = 5*e
        else
            step = 10*e
        end
    end
    start = step*(ceil(lo/step)-1)
    nm1 = ceil(Int,(hi - start)/step)
    start:step:(start + nm1*step)
end

function histrange{T<:Integer,N}(v::AbstractArray{T,N}, n::Integer)
    depwarn("histrange(...) is deprecated, use StatsBase.histrange(...) instead",:histrange)
    nv = length(v)
    if nv == 0 && n < 0
        throw(ArgumentError("number of bins must be ≥ 0 for an empty array, got $n"))
    elseif nv > 0 && n < 1
        throw(ArgumentError("number of bins must be ≥ 1 for a non-empty array, got $n"))
    end
    if nv == 0
        return 0:1:0
    end
    if n <= 0
        throw(ArgumentError("number of bins n=$n must be positive"))
    end
    lo, hi = extrema(v)
    if hi == lo
        step = 1
    else
        bw = (Float64(hi) - Float64(lo)) / n
        e = 10.0^max(0,floor(log10(bw)))
        r = bw / e
        if r <= 1
            step = e
        elseif r <= 2
            step = 2*e
        elseif r <= 5
            step = 5*e
        else
            step = 10*e
        end
    end
    start = step*(ceil(lo/step)-1)
    nm1 = ceil(Int,(hi - start)/step)
    start:step:(start + nm1*step)
end

## hist ##
function sturges(n)  # Sturges' formula
    depwarn("sturges(n) is deprecated, use StatsBase.sturges(n) instead.",:sturges)
    n==0 && return one(n)
    ceil(Int,log2(n))+1
end

function hist!{HT}(h::AbstractArray{HT}, v::AbstractVector, edg::AbstractVector; init::Bool=true)
    depwarn("hist(...) and hist!(...) are deprecated. Use fit(Histogram,...) in StatsBase.jl instead.",:hist!)
    n = length(edg) - 1
    length(h) == n || throw(DimensionMismatch("length(histogram) must equal length(edges) - 1"))
    if init
        fill!(h, zero(HT))
    end
    for x in v
        i = searchsortedfirst(edg, x)-1
        if 1 <= i <= n
            h[i] += 1
        end
    end
    edg, h
end

hist(v::AbstractVector, edg::AbstractVector) = hist!(Array{Int,1}(length(edg)-1), v, edg)
hist(v::AbstractVector, n::Integer) = hist(v,histrange(v,n))
hist(v::AbstractVector) = hist(v,sturges(length(v)))

function hist!{HT}(H::AbstractArray{HT,2}, A::AbstractMatrix, edg::AbstractVector; init::Bool=true)
    depwarn("hist(...) and hist!(...) are deprecated. Use fit(Histogram,...) in StatsBase.jl instead.",:hist!)

    m, n = size(A)
    sH = size(H)
    sE = (length(edg)-1,n)
    sH == sE || throw(DimensionMismatch("incorrect size of histogram"))
    if init
        fill!(H, zero(HT))
    end
    for j = 1:n
        hist!(sub(H, :, j), sub(A, :, j), edg)
    end
    edg, H
end

hist(A::AbstractMatrix, edg::AbstractVector) = hist!(Array{Int,2}(length(edg)-1, size(A,2)), A, edg)
hist(A::AbstractMatrix, n::Integer) = hist(A,histrange(A,n))
hist(A::AbstractMatrix) = hist(A,sturges(size(A,1)))


## hist2d
function hist2d!{HT}(H::AbstractArray{HT,2}, v::AbstractMatrix,
                     edg1::AbstractVector, edg2::AbstractVector; init::Bool=true)
    depwarn("hist2d!(...) and hist2d(...) are deprecated. Use fit(Histogram,...) in StatsBase.jl instead.",:hist2d!)

    size(v,2) == 2 || throw(DimensionMismatch("hist2d requires an Nx2 matrix"))
    n = length(edg1) - 1
    m = length(edg2) - 1
    size(H) == (n, m) || throw(DimensionMismatch("incorrect size of histogram"))
    if init
        fill!(H, zero(HT))
    end
    for i = indices(v,1)
        x = searchsortedfirst(edg1, v[i,1]) - 1
        y = searchsortedfirst(edg2, v[i,2]) - 1
        if 1 <= x <= n && 1 <= y <= m
            @inbounds H[x,y] += 1
        end
    end
    edg1, edg2, H
end

hist2d(v::AbstractMatrix, edg1::AbstractVector, edg2::AbstractVector) =
    hist2d!(Array{Int,2}(length(edg1)-1, length(edg2)-1), v, edg1, edg2)

hist2d(v::AbstractMatrix, edg::AbstractVector) = hist2d(v, edg, edg)

hist2d(v::AbstractMatrix, n1::Integer, n2::Integer) =
    hist2d(v, histrange(sub(v,:,1),n1), histrange(sub(v,:,2),n2))
hist2d(v::AbstractMatrix, n::Integer) = hist2d(v, n, n)
hist2d(v::AbstractMatrix) = hist2d(v, sturges(size(v,1)))

@deprecate cell(dims::Integer...) Array{Any}(dims...)
@deprecate cell(dims::Tuple{Vararg{Integer}}) Array{Any}(dims)

@deprecate(pointer_to_array{T}(p::Ptr{T}, d::Union{Integer, Tuple{Vararg{Integer}}}, own::Bool=false),
    unsafe_wrap(Array, p, d, own))
@deprecate(pointer_to_string(p::Ptr{UInt8}, len::Integer, own::Bool=false),
    unsafe_wrap(String, p, len, own))
@deprecate(pointer_to_string(p::Ptr{UInt8}, own::Bool=false),
    unsafe_wrap(String, p, own))

function checkbounds(::Type{Bool}, sz::Integer, i)
    depwarn("checkbounds(Bool, size(A, d), i) is deprecated, use checkindex(Bool, indices(A, d), i).", :checkbounds)
    checkbounds(Bool, 1:sz, i)
end
immutable FakeArray{T,N} <: AbstractArray{T,N}
    dims::NTuple{N,Int}
end
size(A::FakeArray) = A.dims
function checkbounds{N,T}(::Type{Bool}, sz::NTuple{N,Integer}, I1::T, I...)
    depwarn("checkbounds(Bool, size(A), I...) is deprecated, use checkbounds(Bool, A, I...).", :checkbounds)
    checkbounds(Bool, FakeArray(sz), I1, I...)
end

function first(::Colon)
    depwarn("first(:) is deprecated, see http://docs.julialang.org/en/latest/devdocs/offset-arrays/", :first)
    1
end
function _first(i, A, d)
    depwarn("_first is deprecated, see http://docs.julialang.org/en/latest/devdocs/offset-arrays/", :_first)
    __first(i, A, d)
end
__first(::Colon, P, ::Colon) = first(linearindices(P))
__first(i, P, ::Colon) = first(i)
__first(::Colon, P, d) = first(indices(P, d))
__first(i, P, d) = first(i)

# Not exported, but deprecation may be useful just in case
function Broadcast.check_broadcast_shape(sz::Dims, As::Union{AbstractArray,Number}...)
    depwarn("check_broadcast_shape(size(A), B...) should be replaced with check_broadcast_shape(indices(A), B...)", :check_broadcast_shape)
    Broadcast.check_broadcast_shape(map(OneTo, sz), As...)
end

@deprecate trailingsize{n}(A::AbstractArray, ::Type{Val{n}}) trailingsize(A, n)

@deprecate slice view
@deprecate sub view

# Point users to SuiteSparse
function ereach{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, k::Integer, parent::Vector{Ti})
    error(string("ereach(A, k, parent) now lives in package SuiteSparse.jl. Run",
        "Pkg.add(\"SuiteSparse\") to install SuiteSparse on Julia v0.5."))
end
export etree
function etree{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, postorder::Bool)
    error(string("etree(A[, post]) now lives in package SuiteSparse.jl. Run",
        "Pkg.add(\"SuiteSparse\") to install SuiteSparse on Julia v0.5."))
end
etree(A::SparseMatrixCSC) = etree(A, false)
function csc_permute{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, pinv::Vector{Ti}, q::Vector{Ti})
    error(string("csc_permute(A, pinv, q) now lives in package SuiteSparse.jl. Run",
        "Pkg.add(\"SuiteSparse\") to install SuiteSparse on Julia v0.5."))
end
function symperm{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, pinv::Vector{Ti})
    error(string("symperm(A, pinv) now lives in package SuiteSparse.jl. Run,",
        "Pkg.add(\"SuiteSparse\") to install SuiteSparse on Julia v0.5."))
end

# Deprecate no-op transpose fallback. Please see #13171 and #17075.
function transpose(x)
    depwarn(string("the no-op `transpose` for non-numeric arrays is deprecated, ",
        "and no specific `transpose` method for $(typeof(x)) exists. Use ",
        "`permutedims(x, (2, 1))` for matrices and `reshape(x, 1, length(x))` for vectors, ",
        "or write a specific `transpose(x::$(typeof(x)))` method if appropriate."),
        :transpose)
    return x
end

@deprecate cholfact!(A::Base.LinAlg.HermOrSym, uplo::Symbol, ::Type{Val{false}}) cholfact!(A, Val{false})
@deprecate cholfact!(A::Base.LinAlg.HermOrSym, uplo::Symbol = :U) cholfact!(A)

# During the 0.5 development cycle, do not add any deprecations below this line
# To be deprecated in 0.6

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
    if !all(x -> x == one(x), A.diag)
        throw(ArgumentError("matrix cannot be represented as UnitUpperTriangular"))
    end
    Base.LinAlg.UnitUpperTriangular(Array(A))
end
function convert(::Type{Base.LinAlg.UnitLowerTriangular}, A::Diagonal)
    depwarn(string("`convert(::Type{UnitLowerTriangular}, A::Diagonal)` and other methods ",
        "that convert `Diagonal`/`Bidiagonal` to `<:AbstractTriangular` are deprecated. ",
        "Consider calling the `UnitLowerTriangular` constructor directly ",
        "(`Base.LinAlg.UnitLowerTriangular(A)`) instead."), :convert)
    if !all(x -> x == one(x), A.diag)
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
    eval(FastMath, :(Base.@dep_vectorize_1arg Number $f))
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
    eval(Dates, :(Base.@dep_vectorize_1arg Real $f))
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
    eval(Dates, :(Base.@dep_vectorize_1arg Dates.TimeType $f))
end
for f in (
    :hour, :minute, :second, :millisecond, # base/dates/accessors.jl
    :Date, :datetime2unix, :datetime2rata, :datetime2julian, # base/dates/conversions.jl
    )
    eval(Dates, :(Base.@dep_vectorize_1arg Dates.DateTime $f))
end
eval(Dates, :(Base.@dep_vectorize_1arg Dates.Date Datetime)) # base/dates/conversions.jl

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
    eval(FastMath, :(Base.@dep_vectorize_2arg Number $f))
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
eval(Multimedia, :(macro textmime(mime)
    Base.depwarn(string("`@textmime \"mime\"` is deprecated; use ",
        "`Base.Multimedia.istextmime(::MIME\"mime\") = true` instead"
        ), :textmime)
    quote
        Base.Multimedia.istextmime(::MIME{$(Meta.quot(Symbol(mime)))}) = true
    end
end))

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
eval(Base.LinAlg, quote
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
end)

# #19246
@deprecate den denominator
@deprecate num numerator

Filesystem.stop_watching(stream::Filesystem._FDWatcher) = depwarn("stop_watching(::_FDWatcher) should not be used", :stop_watching)

# #19088
@deprecate takebuf_array take!
@deprecate takebuf_string(b) String(take!(b))

# #19288
eval(Base.Dates, quote
    function recur{T<:TimeType}(fun::Function, dr::StepRange{T}; negate::Bool=false, limit::Int=10000)
        Base.depwarn("Dates.recur is deprecated, use filter instead.",:recur)
        if negate
            filter(x -> !fun(x), dr)
        else
            filter(fun, dr)
        end
     end
     recur{T<:TimeType}(fun::Function, start::T, stop::T; step::Period=Day(1), negate::Bool=false, limit::Int=10000) = recur(fun, start:step:stop; negate=negate)
end)

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
eval(SparseArrays, :(broadcast_zpreserving(f, args...) = Base._depwarn_bczpres(f, args...)))
eval(SparseArrays, :(broadcast_zpreserving(f, A::SparseMatrixCSC, B::SparseMatrixCSC) = Base._depwarn_bczpres(f, A, B)))
eval(SparseArrays, :(broadcast_zpreserving(f, A::SparseMatrixCSC, B::Union{Array,BitArray,Number}) = Base._depwarn_bczpres(f, A, B)))
eval(SparseArrays, :(broadcast_zpreserving(f, A::Union{Array,BitArray,Number}, B::SparseMatrixCSC) = Base._depwarn_bczpres(f, A, B)))
eval(SparseArrays, :(broadcast_zpreserving!(f, args...) = Base._depwarn_bczpres!(f, args...)))
eval(SparseArrays, :(broadcast_zpreserving!(f, C::SparseMatrixCSC, A::SparseMatrixCSC, B::Union{Array,BitArray,Number}) = Base._depwarn_bczpres!(f, C, A, B)))
eval(SparseArrays, :(broadcast_zpreserving!(f, C::SparseMatrixCSC, A::Union{Array,BitArray,Number}, B::SparseMatrixCSC) = Base._depwarn_bczpres!(f, C, A, B)))

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
@deprecate big(r::FloatRange) big.(r)
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

# Rename LibGit2.Oid to LibGit2.GitHash (part of #19839)
eval(Base.LibGit2, :(Base.@deprecate_binding Oid GitHash))

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
@deprecate finalize(sa::LibGit2.StrArrayStruct) close(sa)
@deprecate finalize(sa::LibGit2.Buffer) close(sa)

# Rename LibGit2.GitAnyObject to LibGit2.GitUnknownObject (part of #19839)
eval(LibGit2, :(Base.@deprecate_binding GitAnyObject GitUnknownObject))

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

eval(Base.Test, quote
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
end)

# Deprecate Array(T, dims...) in favor of proper type constructors
@deprecate Array{T,N}(::Type{T}, d::NTuple{N,Int})               Array{T,N}(d)
@deprecate Array{T}(::Type{T}, d::Int...)                        Array{T,length(d)}(d...)
@deprecate Array{T}(::Type{T}, m::Int)                           Array{T,1}(m)
@deprecate Array{T}(::Type{T}, m::Int,n::Int)                    Array{T,2}(m,n)
@deprecate Array{T}(::Type{T}, m::Int,n::Int,o::Int)             Array{T,3}(m,n,o)
@deprecate Array{T}(::Type{T}, d::Integer...)                    Array{T,length(d)}(convert(Tuple{Vararg{Int}}, d))
@deprecate Array{T}(::Type{T}, m::Integer)                       Array{T,1}(Int(m))
@deprecate Array{T}(::Type{T}, m::Integer,n::Integer)            Array{T,2}(Int(m),Int(n))
@deprecate Array{T}(::Type{T}, m::Integer,n::Integer,o::Integer) Array{T,3}(Int(m),Int(n),Int(o))

# Likewise for SharedArrays
@deprecate SharedArray{T,N}(::Type{T}, dims::Dims{N}; kwargs...) SharedArray{T,N}(dims; kwargs...)
@deprecate SharedArray{T}(::Type{T}, dims::Int...; kwargs...)    SharedArray{T,length(dims)}(dims...; kwargs...)
@deprecate(SharedArray{T,N}(filename::AbstractString, ::Type{T}, dims::NTuple{N,Int}, offset; kwargs...),
           SharedArray{T,N}(filename, dims, offset; kwargs...))
@deprecate(SharedArray{T}(filename::AbstractString, ::Type{T}, dims::NTuple, offset; kwargs...),
           SharedArray{T,length(dims)}(filename, dims, offset; kwargs...))

@noinline function is_intrinsic_expr(x::ANY)
    Base.depwarn("is_intrinsic_expr is deprecated. There are no intrinsic functions anymore.", :is_intrinsic_expr)
    return false
end

# End deprecations scheduled for 0.6
