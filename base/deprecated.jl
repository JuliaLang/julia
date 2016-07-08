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
@deprecate cov(x::AbstractVector; corrected=true, mean=Base.mean(x)) Base.covm(x, mean, corrected)
@deprecate cov(X::AbstractMatrix; vardim=1, corrected=true, mean=Base.mean(X, vardim)) Base.covm(X, mean, vardim, corrected)
@deprecate cov(x::AbstractVector, y::AbstractVector; corrected=true, mean=(Base.mean(x), Base.mean(y))) Base.covm(x, mean[1], y, mean[2], corrected)
@deprecate cov(X::AbstractVecOrMat, Y::AbstractVecOrMat; vardim=1, corrected=true, mean=(Base.mean(X, vardim), Base.mean(Y, vardim))) Base.covm(X, mean[1], Y, mean[2], vardim, corrected)

@deprecate cor(x::AbstractVector; mean=Base.mean(x)) Base.corm(x, mean)
@deprecate cor(X::AbstractMatrix; vardim=1, mean=Base.mean(X, vardim)) Base.corm(X, mean, vardim)
@deprecate cor(x::AbstractVector, y::AbstractVector; mean=(Base.mean(x), Base.mean(y))) Base.corm(x, mean[1], y, mean[2])
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

# Combinatorics functions that have been moved out of base (#13897)
# Note: only the two-argument form of factorial has been moved
for deprecatedfunc in [:combinations, :factorial, :prevprod, :levicivita,
        :nthperm!, :nthperm, :parity, :partitions, :permutations]
    @eval begin
        $deprecatedfunc(args...) = error(string($deprecatedfunc, args,
            " has been moved to the package Combinatorics.jl.\n",
            "Run Pkg.add(\"Combinatorics\") to install Combinatorics on Julia v0.5-"))
        export $deprecatedfunc
    end
end

# Primes functions that have been moved out of base (#16481)
for deprecatedfunc in [:isprime, :primes, :primesmask, :factor]
    @eval begin
        $deprecatedfunc(args...) = error(string($deprecatedfunc, args,
            " has been moved to the package Primes.jl.\n",
            "Run Pkg.add(\"Primes\") to install Primes on Julia v0.5-"))
        export $deprecatedfunc
    end
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

@deprecate isgeneric(f) isa(f,Function)

# need to do this manually since the front end deprecates method defs of `call`
const call = @eval function(f, args...; kw...)
    $(Expr(:meta, :noinline))
    depwarn("call(f,args...) is deprecated, use f(args...) instead.", :call)
    f(args...; kw...)
end
export call

@deprecate_binding LambdaStaticData LambdaInfo

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

#15409
# Deprecated definition of pmap with keyword arguments.
# When this is removed the following definition needs to be uncommented
# and added to pmap.jl
# pmap(f, c...) = pmap(default_worker_pool(), f, c...)

function pmap(f, c...; err_retry=nothing, err_stop=nothing, pids=nothing, kwargs...)
    kwargs = Dict{Symbol, Any}(kwargs)

    if err_retry != nothing
        depwarn("err_retry is deprecated, use pmap(retry(f), c...).", :pmap)
        if err_retry == true
            f = retry(f)
        end
    end

    if pids == nothing
        p = default_worker_pool()
    else
        depwarn("pids is deprecated, use pmap(::WorkerPool, f, c...).", :pmap)
        p = WorkerPool(pids)
    end

    if err_stop != nothing
        depwarn("err_stop is deprecated, use pmap(f, c...; on_error = error_handling_func).", :pmap)
        if err_stop == false
            kwargs[:on_error] = e->e
        end
    end

    pmap(p, f, c...; kwargs...)
end

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
                    (:XorFun, :$),
                    (:AddFun, :+),
                    (:DotAddFun, :.+),
                    (:SubFun, :-),
                    (:DotSubFun, :.-),
                    (:MulFun, :*),
                    (:DotMulFun, :.*),
                    (:RDivFun, :/),
                    (:DotRDivFun, :./),
                    (:LDivFun, :\),
                    (:IDivFun, :div),
                    (:DotIDivFun, :.÷),
                    (:ModFun, :mod),
                    (:RemFun, :rem),
                    (:DotRemFun, :.%),
                    (:PowFun, :^),
                    (:MaxFun, :scalarmax),
                    (:MinFun, :scalarmin),
                    (:LessFun, :<),
                    (:MoreFun, :>),
                    (:DotLSFun, :.<<),
                    (:DotRSFun, :.>>),
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
@deprecate_binding CentralizedAbs2Fun typeof(centralizedabs2fun(0)).name.primary
(::Type{typeof(centralizedabs2fun(0)).name.primary})(m::Number) = centralizedabs2fun(m)
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
broadcast(f::Function, i::Integer) = invoke(broadcast, (Function, Number), f, i)

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
            splice!(a, idx:endn, invalids_as.data)
            l = length(a)
        end
        String(a)
    end
)

if sizeof(Cwchar_t) == 2
    @deprecate_binding WString UTF16String
    @deprecate_binding wstring utf16
    utf16(s::Cwstring) = utf16(convert(Ptr{Cwchar_t}, s))
elseif sizeof(Cwchar_t) == 4
    @deprecate_binding WString UTF32String
    @deprecate_binding wstring utf32
    utf32(s::Cwstring) = utf32(convert(Ptr{Cwchar_t}, s))
end

@deprecate ==(x::Char, y::Integer) UInt32(x) == y
@deprecate ==(x::Integer, y::Char) x == UInt32(y)
@deprecate isless(x::Char, y::Integer) UInt32(x) < y
@deprecate isless(x::Integer, y::Char) x < UInt32(y)

# delete these methods along with deprecations:
isequal(x::Char, y::Integer) = false
isequal(x::Integer, y::Char) = false

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

## midpoints of intervals
midpoints(r::Range) = r[1:length(r)-1] + 0.5*step(r)
midpoints(v::AbstractVector) = [0.5*(v[i] + v[i+1]) for i in 1:length(v)-1]

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

hist(v::AbstractVector, edg::AbstractVector) = hist!(Array(Int, length(edg)-1), v, edg)
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

hist(A::AbstractMatrix, edg::AbstractVector) = hist!(Array(Int, length(edg)-1, size(A,2)), A, edg)
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
    hist2d!(Array(Int, length(edg1)-1, length(edg2)-1), v, edg1, edg2)

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
    depwarn("first(:) is no longer unambiguous, call Base._first(:, A, dim)", :first)
    1
end

# Not exported, but may be useful just in case
function Broadcast.check_broadcast_shape(sz::Dims, As::Union{AbstractArray,Number}...)
    depwarn("check_broadcast_shape(size(A), B...) should be replaced with check_broadcast_shape(indices(A), B...)", :check_broadcast_shape)
    Broadcast.check_broadcast_shape(map(OneTo, sz), As...)
end

@deprecate slice view
@deprecate sub view

# Point users to SuiteSparse
function ereach{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, k::Integer, parent::Vector{Ti})
    error(string("ereach(A, k, parent) now lives in package SuiteSparse.jl. Run",
        "Pkg.add(\"SuiteSparse\") to install SuiteSparse on Julia v0.5."))
end
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

# During the 0.5 development cycle, do not add any deprecations below this line
# To be deprecated in 0.6

const _oldstyle_array_vcat_ = false

# End deprecations scheduled for 0.6
