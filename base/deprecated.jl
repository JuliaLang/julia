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
    if opts.depwarn == 1  # raise a warning
        ln = Int(unsafe_load(cglobal(:jl_lineno, Cint)))
        fn = String(unsafe_load(cglobal(:jl_filename, Ptr{Cchar})))
        bt = backtrace()
        caller = firstcaller(bt, funcsym)
        warn(msg, once=(caller != C_NULL), key=caller, bt=bt, filename=fn, lineno=ln)
    elseif opts.depwarn == 2  # raise an error
        throw(ErrorException(msg))
    end
end

function firstcaller(bt::Array{Ptr{Void},1}, funcsym::Symbol)
    # Identify the calling line
    i = 1
    len = length(bt)
    while i <= len
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
    if i <= len
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

# 0.4 deprecations

@deprecate split(x,y,l::Integer,k::Bool) split(x,y;limit=l,keep=k)
@deprecate split(x,y,l::Integer) split(x,y;limit=l)
@deprecate split(x,y,k::Bool) split(x,y;keep=k)

@deprecate rsplit(x,y,l::Integer,k::Bool) rsplit(x,y;limit=l,keep=k)
@deprecate rsplit(x,y,l::Integer) rsplit(x,y;limit=l)
@deprecate rsplit(x,y,k::Bool) rsplit(x,y;keep=k)

const TcpSocket = TCPSocket
deprecate(:TcpSocket)
const IpAddr = IPAddr
deprecate(:IpAddr)
@deprecate_binding UdpSocket UDPSocket

@deprecate isblank(c::Char) c == ' ' || c == '\t'
@deprecate isblank(s::AbstractString) all(c -> c == ' ' || c == '\t', s)

@deprecate_binding Nothing Void
@deprecate_binding None Union{}

export apply
@noinline function apply(f, args...)
    depwarn("apply(f, x) is deprecated, use `f(x...)` instead", :apply)
    return Core._apply(f, args...)
end

@deprecate median(v::AbstractArray; checknan::Bool=true)  median(v)
@deprecate median(v::AbstractArray, region; checknan::Bool=true)  median(v, region)
@deprecate median!(v::AbstractVector; checknan::Bool=true)  median!(v)

@deprecate Dict{K,V}(ks::AbstractArray{K}, vs::AbstractArray{V}) Dict{K,V}(zip(ks, vs))
@deprecate Dict{K,V}(ks::Tuple{Vararg{K}}, vs::Tuple{Vararg{V}}) Dict{K,V}(zip(ks, vs))
@deprecate Dict{K}(ks::Tuple{Vararg{K}}, vs::Tuple)              Dict{K,Any}(zip(ks, vs))
@deprecate Dict{V}(ks::Tuple, vs::Tuple{Vararg{V}})              Dict{Any,V}(zip(ks, vs))
@deprecate Dict(ks, vs)                                          Dict{Any,Any}(zip(ks, vs))

@deprecate itrunc{T<:Integer}(::Type{T}, n::Integer) (n % T)

@deprecate oftype{T}(::Type{T},c)  convert(T,c)

@deprecate inf(x::AbstractFloat)  oftype(x,Inf)
@deprecate nan(x::AbstractFloat)  oftype(x,NaN)
@deprecate inf{T<:AbstractFloat}(::Type{T})  convert(T,Inf)
@deprecate nan{T<:AbstractFloat}(::Type{T})  convert(T,NaN)

# 13221 - when removing Uint deprecation, remove hack in jl_binding_deprecation_warning
@deprecate_binding Uint    UInt
@deprecate_binding Uint8   UInt8
@deprecate_binding Uint16  UInt16
@deprecate_binding Uint32  UInt32
@deprecate_binding Uint64  UInt64
@deprecate_binding Uint128 UInt128

@deprecate zero{T}(::Type{Ptr{T}}) Ptr{T}(0)
@deprecate zero{T}(x::Ptr{T})      Ptr{T}(0)
@deprecate one{T}(::Type{Ptr{T}})  Ptr{T}(1)
@deprecate one{T}(x::Ptr{T})       Ptr{T}(1)

@deprecate rand!(r::Range, A::AbstractArray) rand!(A, r)
@deprecate rand!(mt::MersenneTwister, r::Range, A::AbstractArray) rand!(mt, A, r)

@deprecate itrunc(x)              trunc(Integer,x)
@deprecate itrunc{T<:Integer}(::Type{T},x::Real) trunc(T,x)
@deprecate iceil(x)               ceil(Integer,x)
@deprecate iceil{T}(::Type{T},x)  ceil(T,x)
@deprecate ifloor(x)              floor(Integer,x)
@deprecate ifloor{T}(::Type{T},x) floor(T,x)
@deprecate iround(x)              round(Integer,x)
@deprecate iround{T}(::Type{T},x) round(T,x)

@deprecate_binding Base64Pipe Base64EncodePipe
@deprecate base64 base64encode

@deprecate prevind(a::Any, i::Integer)   i-1
@deprecate nextind(a::Any, i::Integer)   i+1

@deprecate givens{T}(f::T, g::T, i1::Integer, i2::Integer, cols::Integer)   givens(f, g, i1, i2)

@deprecate squeeze(X, dims) squeeze(X, tuple(dims...))

@deprecate sizehint(A, n) sizehint!(A, n)

@deprecate randbool!                               rand!
@deprecate randbool()                              rand(Bool)
@deprecate randbool(r::AbstractRNG)                rand(r, Bool)
@deprecate randbool(dims::Dims)                    bitrand(dims)
@deprecate randbool(dims::Int...)                  bitrand(dims)
@deprecate randbool(r::AbstractRNG, dims::Dims)    bitrand(r, dims)
@deprecate randbool(r::AbstractRNG, dims::Int...)  bitrand(r, dims)

@deprecate beginswith startswith

@deprecate functionlocs(f,t)  map(functionloc, methods(f,t))

@deprecate null nullspace

@deprecate error(ex::Exception) throw(ex)
@deprecate error{E<:Exception}(::Type{E}) throw(E())

@deprecate_binding MemoryError OutOfMemoryError

@deprecate map!(f::Callable, dest::StridedArray, A::StridedArray, B::Number) broadcast!(f, dest, A, B)
@deprecate map!(f::Callable, dest::StridedArray, A::Number, B::StridedArray) broadcast!(f, dest, A, B)

#9295
@deprecate push!(t::Associative, key, v)  setindex!(t, v, key)

@deprecate (|>)(src::AbstractCmd,    dest::AbstractCmd)    pipeline(src, dest)
@deprecate (.>)(src::AbstractCmd,    dest::AbstractCmd)    pipeline(src, stderr=dest)
@deprecate (|>)(src::Redirectable,   dest::AbstractCmd)    pipeline(src, dest)
@deprecate (|>)(src::AbstractCmd,    dest::Redirectable)   pipeline(src, dest)
@deprecate (.>)(src::AbstractCmd,    dest::Redirectable)   pipeline(src, stderr=dest)
@deprecate (|>)(src::AbstractCmd,    dest::AbstractString) pipeline(src, dest)
@deprecate (|>)(src::AbstractString, dest::AbstractCmd)    pipeline(src, dest)
@deprecate (.>)(src::AbstractCmd,    dest::AbstractString) pipeline(src, stderr=dest)
@deprecate (>>)(src::AbstractCmd,    dest::AbstractString) pipeline(src, stdout=dest, append=true)
@deprecate (.>>)(src::AbstractCmd,   dest::AbstractString) pipeline(src, stderr=dest, append=true)
@deprecate pipe pipeline

# 10314
@deprecate filter!(r::Regex, d::Dict) filter!((k,v)->ismatch(r,k), d)

# 1470
@deprecate integer(s::AbstractString)   parse(Int,s)
@deprecate unsigned(s::AbstractString)  parse(UInt,s)
@deprecate int(s::AbstractString)       parse(Int,s)
@deprecate uint(s::AbstractString)      parse(UInt,s)
@deprecate int8(s::AbstractString)      parse(Int8,s)
@deprecate uint8(s::AbstractString)     parse(UInt8,s)
@deprecate int16(s::AbstractString)     parse(Int16,s)
@deprecate uint16(s::AbstractString)    parse(UInt16,s)
@deprecate int32(s::AbstractString)     parse(Int32,s)
@deprecate uint32(s::AbstractString)    parse(UInt32,s)
@deprecate int64(s::AbstractString)     parse(Int64,s)
@deprecate uint64(s::AbstractString)    parse(UInt64,s)
@deprecate int128(s::AbstractString)    parse(Int128,s)
@deprecate uint128(s::AbstractString)   parse(UInt128,s)
@deprecate float64(s::AbstractString)   parse(Float64,s)
@deprecate float32(s::AbstractString)   parse(Float32,s)

for (f,t) in ((:integer, Integer), (:signed, Signed), (:unsigned, Unsigned))
    @eval begin
        @deprecate $f(x::AbstractArray) round($t, x)
    end
end

for (f,t) in ((:int,    Int), (:int8,   Int8), (:int16,  Int16), (:int32,  Int32),
              (:int64,  Int64), (:int128, Int128), (:uint,   UInt), (:uint8,  UInt8),
              (:uint16, UInt16), (:uint32, UInt32), (:uint64, UInt64), (:uint128,UInt128))
    ex1 = sprint(io->show_unquoted(io,:([parse($t,s) for s in a])))
    ex2 = sprint(io->show_unquoted(io,:(round($t, a))))
    name = Expr(:quote,f)
    @eval begin
        function ($f)(x::AbstractArray)
            if all(y->isa(y,AbstractString), x)
                depwarn(string($name,"(a::AbstractArray) is deprecated, use ", $ex1, " instead."), $name)
                return [parse($t,s) for s in x]
            elseif all(y->isa(y,Number), x)
                depwarn(string($name,"(a::AbstractArray) is deprecated, use ", $ex2, " instead."), $name)
                return round($t, x)
            end
            y = similar(x,$t)
            i = 1
            for e in x
                y[i] = ($f)(e)
                i += 1
            end
            y
        end
    end
end

for (f,t) in ((:char, Char), (:bool, Bool), (:float16, Float16), (:float32, Float32),
              (:float64, Float64), (:complex64, Complex64), (:complex128, Complex128))
    @eval begin
        @deprecate $f(x::AbstractArray) map($t, x)
    end
end

const convert_funcs_and_types =
    ((:integer, Integer), (:signed, Signed), (:unsigned, Unsigned), (:int, Int), (:int8, Int8),
     (:int16, Int16), (:int32, Int32), (:int64, Int64), (:int128, Int128), (:uint, UInt),
     (:uint8, UInt8), (:uint16, UInt16), (:uint32, UInt32), (:uint64, UInt64), (:uint128,UInt128),
     (:float16, Float16), (:float32, Float32), (:float64, Float64))

for (f,t) in convert_funcs_and_types
     @eval begin
         @deprecate $f(r::StepRange) map($t, r)
         @deprecate $f(r::UnitRange) map($t, r)
     end
end

for (f,t) in ((:float16,:Float16),(:float32,:Float32),(:float64,:Float64))
    @eval begin
        @deprecate $f(r::FloatRange) map($t, r)
    end
end

@deprecate int(x)  Int(x)
@deprecate uint(x) UInt(x)

@deprecate bool(x::Number)  x!=0

@deprecate char(x)                 Char(x)
@deprecate char(x::AbstractFloat)  Char(round(UInt32,x))
@deprecate integer(x::Char)        Int(x)

@deprecate complex128(r::Real, i::Real)  Complex128(r, i)
@deprecate complex128(z)                 Complex128(z)
@deprecate complex64(r::Real, i::Real)   Complex64(r, i)
@deprecate complex64(z)                  Complex64(z)
@deprecate complex32(r::Real, i::Real)   Complex32(r, i)
@deprecate complex32(z)                  Complex32(z)

for (f,t) in convert_funcs_and_types
    @eval begin
        @deprecate $f(z::Complex)  Complex($t(real(z)), $t(imag(z)))
    end
end

@deprecate float16(x) Float16(x)
@deprecate float32(x) Float32(x)
@deprecate float64(x) Float64(x)

@deprecate int8(x)   Int8(x)
@deprecate int16(x)  Int16(x)
@deprecate int32(x)  Int32(x)
@deprecate int64(x)  Int64(x)
@deprecate int128(x) Int128(x)

@deprecate uint8(x)           UInt8(x)
@deprecate uint8(x::Integer)  x % UInt8
@deprecate uint8(x::Bool)     UInt8(x)

@deprecate uint16(x)  UInt16(x)
@deprecate uint32(x)  UInt32(x)
@deprecate uint64(x)  UInt64(x)
@deprecate uint128(x) UInt128(x)

@deprecate integer(x) Integer(x)

for (f,t) in ((:uint8,:UInt8), (:uint16,:UInt16), (:uint32,:UInt32), (:uint64,:UInt64),
              (:int8,:Int8),   (:int16,:Int16),   (:int32,:Int32),   (:int64,:Int64),
              (:int128,:Int128), (:uint128,:UInt128), (:signed,:Int), (:unsigned,:UInt),
              (:integer,:Int), (:int,:Int), (:uint,:UInt))
    @eval begin
        @deprecate ($f)(x::AbstractFloat)  round($t,x)
        @deprecate ($f)(x::Rational)       round($t,x)
    end
end

@deprecate integer(x::Ptr)   convert(UInt, x)
@deprecate unsigned(x::Ptr)  convert(UInt, x)

for (f,t) in ((:float32, Float32), (:float64, Float64))
    @eval begin
        @deprecate ($f){S<:AbstractString}(a::AbstractArray{S}) [parse($t,s) for s in a]
    end
end

@deprecate flipud(A::AbstractArray) flipdim(A, 1)
@deprecate fliplr(A::AbstractArray) flipdim(A, 2)

@deprecate strftime     Libc.strftime
@deprecate strptime     Libc.strptime
@deprecate flush_cstdio Libc.flush_cstdio
@deprecate c_free       Libc.free
@deprecate c_malloc     Libc.malloc
@deprecate c_calloc     Libc.calloc
@deprecate c_realloc    Libc.realloc
@deprecate errno        Libc.errno
@deprecate strerror     Libc.strerror

@deprecate dlclose      Libdl.dlclose
@deprecate dlopen       Libdl.dlopen
@deprecate dlopen_e     Libdl.dlopen_e
@deprecate dlsym        Libdl.dlsym
@deprecate dlsym_e      Libdl.dlsym_e
@deprecate find_library Libdl.find_library

@deprecate cholfact(A::AbstractMatrix, β::Number) cholfact(A, shift=β)
@deprecate ldltfact(A::AbstractMatrix, β::Number) ldltfact(A, shift=β)

@deprecate with_env(f::Function, key::AbstractString, val) withenv(f, key=>val)

@deprecate ntuple(n::Integer, f::Function) ntuple(f, n)

# 0.4 discontinued functions

@noinline function subtypetree(x::DataType, level=-1)
    depwarn("`subtypetree` is discontinued", :subtypetree)
    (level == 0 ? (x, []) : (x, Any[subtypetree(y, level-1) for y in subtypes(x)]))
end

@noinline function unsafe_convert{P}(::Type{P}, x)
    P<:Ptr || throw(MethodError(unsafe_convert, (P, x)))
    ret = convert(P, x) # attempt the call first, so we only print the depwarn if it can even succeed
    depwarn("convert(::Type{Ptr}, ::$(typeof(x))) methods should be converted to be methods of unsafe_convert", :unsafe_convert)
    return ret
end

@noinline function convert{T}(::Type{Ptr{T}}, x::Integer)
    depwarn("converting integers to pointers is discontinued", :convert)
    box(Ptr{T},unbox(UInt,UInt(x)))
end
@noinline function convert{T}(::Type{Ptr{T}}, x::Signed)
    depwarn("converting signed numbers to pointers is discontinued", :convert)
    box(Ptr{T},unbox(Int,Int(x)))
end

# 8898
@deprecate precision(x::DateTime) eps(x)
@deprecate precision(x::Date) eps(x)

@deprecate names(t::DataType) fieldnames(t)
@deprecate names(v) fieldnames(v)

@noinline function push!(A)
    depwarn("push!(A) has been deprecated", :push!)
    A
end

# 10458
to_index_nodep(i::Real) = convert(Int,i)::Int

@noinline function to_index(i::Real)
    depwarn("Indexing with non-Integer Reals is deprecated.  It may be that your index arose from an integer division of the form i/j, in which case you should consider using i÷j or div(i,j) instead.", :to_index)
    to_index_nodep(i)
end

to_index{T<:Integer}(A::AbstractArray{T}) = A
@noinline function to_index{T<:Real}(A::AbstractArray{T})
    depwarn("indexing with non Integer AbstractArrays is deprecated", :to_index)
    Int[to_index_nodep(x) for x in A]
end

@noinline function to_index(I::Tuple)
    depwarn("to_index(I::Tuple) is deprecated, use to_indexes(I...) instead.", :to_index)
    to_indexes(I...)
end

@deprecate getindex(c::Char, I::Real...) getindex(c, map(Int, I)...)
@deprecate getindex(s::AbstractString, x::Real) getindex(s, Int(x))
@deprecate checkbounds(s::AbstractString, i::Real) checkbounds(s, Int(i))

@noinline function float_isvalid{T<:Union{Float32,Float64}}(s::AbstractString, out::Array{T,1})
    tf = tryparse(T, s)
    isnull(tf) || (out[1] = get(tf))
    !isnull(tf)
end

@noinline function float32_isvalid(s::AbstractString, out::Array{Float32,1})
    depwarn("float32_isvalid is deprecated, use tryparse(Float32,s) instead", :float32_isvalid)
    float_isvalid(s, out)
end

@noinline function float64_isvalid(s::AbstractString, out::Array{Float64,1})
    depwarn("float64_isvalid is deprecated, use tryparse(Float64,s) instead", :float64_isvalid)
    float_isvalid(s, out)
end

export float32_isvalid, float64_isvalid

@deprecate parsefloat(s::AbstractString) parse(Float64,s)
@deprecate parsefloat(T, s)              parse(T, s)

@deprecate parseint(s)                parse(Int, s)
@deprecate parseint(s,base)           parse(Int, s, base)
@deprecate parseint(T::Type, s)       parse(T, s)
@deprecate parseint(T::Type, s, base) parse(T, s, base)

@deprecate linrange linspace

@deprecate BigFloat(s::AbstractString) parse(BigFloat,s)
@deprecate BigInt(s::AbstractString) parse(BigInt,s)

@deprecate (~)(x::Char)           Char(~UInt32(x))
@deprecate (&)(x::Char, y::Char)  Char(UInt32(x) & UInt32(y))
@deprecate (|)(x::Char, y::Char)  Char(UInt32(x) | UInt32(y))
@deprecate ($)(x::Char, y::Char)  Char(UInt32(x) $ UInt32(y))

# 11241
@deprecate is_valid_char(ch::Char)          isvalid(ch)
@deprecate is_valid_utf8(str::String)   isvalid(str)
@deprecate is_valid_utf16(str::UTF16String) isvalid(str)
@deprecate is_valid_utf32(str::UTF32String) isvalid(str)
@deprecate is_valid_char(ch)   isvalid(Char, ch)
@deprecate is_valid_utf8(str)  isvalid(String, str)
@deprecate is_valid_utf16(str) isvalid(UTF16String, str)
@deprecate is_valid_utf32(str) isvalid(UTF32String, str)

# 11379
@deprecate utf32(c::Integer...)   UTF32String(UInt32[c...,0])

# 12087
@deprecate call(P::Base.DFT.ScaledPlan, A) P * A
if Base.USE_GPL_LIBS
    @deprecate call(P::Base.FFTW.DCTPlan, A) P * A
    @deprecate call(P::Base.FFTW.cFFTWPlan, A) P * A
    @deprecate call(P::Base.FFTW.rFFTWPlan, A) P * A
    @deprecate call(P::Base.FFTW.r2rFFTWPlan, A) P * A
end
for f in (:plan_fft, :plan_ifft, :plan_bfft, :plan_fft!, :plan_ifft!, :plan_bfft!, :plan_rfft)
    @eval @deprecate $f(A, dims, flags) $f(A, dims; flags=flags)
    @eval @deprecate $f(A, dims, flags, tlim) $f(A, dims; flags=flags, timelimit=tlim)
end
for f in (:plan_brfft, :plan_irfft)
    @eval @deprecate $f(A, d, dims, flags) $f(A, d, dims; flags=flags)
    @eval @deprecate $f(A, d, dims, flags, tlim) $f(A, d, dims; flags=flags, timelimit=tlim)
end

# 10862

@noinline function chol(A::AbstractMatrix, uplo::Symbol)
    depwarn(string("chol(a::AbstractMatrix, uplo::Symbol) is deprecated, ",
        "use chol(a::AbstractMatrix, uplo::Union{Val{:L},Val{:U}}) instead"), :chol)
    chol(A, Val{uplo})
end

# 11554

read!(from::AbstractIOBuffer, p::Ptr, nb::Integer) = read!(from, p, Int(nb))
function read!(from::AbstractIOBuffer, p::Ptr, nb::Int)
    depwarn("read!(::IOBuffer, ::Ptr) is unsafe and therefore deprecated", :read!)
    from.readable || throw(ArgumentError("read failed, IOBuffer is not readable"))
    avail = nb_available(from)
    adv = min(avail, nb)
    ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, UInt), p, pointer(from.data, from.ptr), adv)
    from.ptr += adv
    if nb > avail
        throw(EOFError())
    end
    p
end

@deprecate gc_enable() gc_enable(true)
@deprecate gc_disable() gc_enable(false)

@deprecate stop_timer close

function Timer(f::Function)
    error("Timer(f) is deprecated. Use Timer(f, delay, repeat) instead.")
end

function start_timer(t, d, r)
    error("start_timer is deprecated. Use Timer(callback, delay, repeat) instead.")
end

@deprecate_binding UnionType Union

@deprecate_binding MathConst Irrational

macro math_const(sym, val, def)
    depwarn("@math_const is deprecated and renamed to @irrational.", Symbol("@math_const"))
    :(@irrational $(esc(sym)) $(esc(val)) $(esc(def)))
end
export @math_const

# 11280, mmap

export msync

"""
    msync(ptr, len, [flags])

Forces synchronization of the [`mmap`](:func:`mmap`)ped memory region from `ptr` to
`ptr+len`. Flags defaults to `MS_SYNC`, but can be a combination of `MS_ASYNC`, `MS_SYNC`,
or `MS_INVALIDATE`. See your platform man page for specifics. The flags argument is not
valid on Windows.

You may not need to call `msync`, because synchronization is performed at intervals
automatically by the operating system. However, you can call this directly if, for example,
you are concerned about losing the result of a long-running calculation.
"""
function msync end

msync{T}(A::Array{T}) = msync(pointer(A), length(A)*sizeof(T))
msync(B::BitArray) = msync(pointer(B.chunks), length(B.chunks)*sizeof(UInt64))

if is_unix()
export mmap
@noinline function mmap(len::Integer, prot::Integer, flags::Integer, fd, offset::Integer)
    depwarn("`mmap` is deprecated, use `Mmap.mmap(io, Array{T,N}, dims, offset)` instead to return an mmapped-array", :mmap)
    const pagesize::Int = ccall(:jl_getpagesize, Clong, ())
    # Check that none of the computations will overflow
    if len < 0
        throw(ArgumentError("requested size must be ≥ 0, got $len"))
    end
    if len > typemax(Int)-pagesize
        throw(ArgumentError("requested size must be ≤ $(typemax(Int)-pagesize), got $len"))
    end
    # Set the offset to a page boundary
    offset_page::Int64 = floor(Integer,offset/pagesize)*pagesize
    len_page::Int = (offset-offset_page) + len
    # Mmap the file
    p = ccall(:jl_mmap, Ptr{Void}, (Ptr{Void}, Csize_t, Cint, Cint, Cint, Int64), C_NULL, len_page, prot, flags, fd, offset_page)
    systemerror("memory mapping failed", reinterpret(Int,p) == -1)
    # Also return a pointer that compensates for any adjustment in the offset
    return p, Int(offset-offset_page)
end

@noinline function munmap(p::Ptr,len::Integer)
    depwarn("`munmap` is deprecated, `mmap` Arrays are automatically munmapped when finalized", :munmap)
    systemerror("munmap", ccall(:munmap,Cint,(Ptr{Void},Int),p,len) != 0)
end

const MS_ASYNC = 1
const MS_INVALIDATE = 2
const MS_SYNC = 4

@doc """
    MS_ASYNC
    MS_SYNC
    MS_INVALIDATE

Enum constants for [`msync`](:func:`msync`). See your platform man page for details.
(not available on Windows).
""" ->
(MS_ASYNC, MS_SYNC, MS_INVALIDATE)


@noinline function msync(p::Ptr, len::Integer, flags::Integer=MS_SYNC)
    depwarn("`msync` is deprecated, use `Mmap.sync!(array)` instead", :msync)
    systemerror("msync", ccall(:msync, Cint, (Ptr{Void}, Csize_t, Cint), p, len, flags) != 0)
end
end


if is_windows()
@noinline function munmap(viewhandle::Ptr, mmaphandle::Ptr)
    depwarn("`munmap` is deprecated, `mmap` Arrays are automatically munmapped when finalized", :munmap)
    status = ccall(:UnmapViewOfFile, stdcall, Cint, (Ptr{Void},), viewhandle)!=0
    status |= ccall(:CloseHandle, stdcall, Cint, (Ptr{Void},), mmaphandle)!=0
    if !status
        error("could not unmap view: $(Libc.FormatMessage())")
    end
end

@noinline function msync(p::Ptr, len::Integer)
    depwarn("`msync` is deprecated, use `Mmap.sync!(array)` instead", :msync)
    status = ccall(:FlushViewOfFile, stdcall, Cint, (Ptr{Void}, Csize_t), p, len)!=0
    if !status
        error("could not msync: $(Libc.FormatMessage())")
    end
end

end

if is_unix()
    @deprecate mmap_array{T,N}(::Type{T}, dims::NTuple{N,Integer}, s::IO, offset=position(s)) Mmap.mmap(s, Array{T,N}, dims, offset)
end

if is_windows()
type SharedMemSpec
    name :: AbstractString
    readonly :: Bool
    create :: Bool
end
export mmap_array
@noinline function mmap_array{T,N}(::Type{T}, dims::NTuple{N,Integer}, s::Union{IO,SharedMemSpec}, offset::Int64)
    depwarn("`mmap_array` is deprecated, use `Mmap.mmap(io, Array{T,N}, dims, offset)` instead to return an mmapped-array", :mmap_array)
    if isa(s,SharedMemSpec)
        a = Mmap.Anonymous(s.name, s.readonly, s.create)
    else
        a = s
    end
    return Mmap.mmap(a, Array{T,N}, dims, offset)
end
end

@deprecate mmap_bitarray{N}(::Type{Bool}, dims::NTuple{N,Integer}, s::IOStream, offset::Int64=position(s)) mmap(s, BitArray, dims, offset)
@deprecate mmap_bitarray{N}(dims::NTuple{N,Integer}, s::IOStream, offset=position(s)) mmap(s, BitArray, dims, offset)


## require ##

function maybe_require_file(name::AbstractString)
    isabspath(name) && return name
    isfile(name) && return abspath(name)
    if !endswith(name,".jl")
        fname = string(name,".jl")
        isfile(fname) && return abspath(fname)
    end
    return name
end

include("require.jl")
@noinline function require(f::AbstractString)
    depwarn("`require` is deprecated, use `using` or `import` instead", :require)
    if endswith(f,".jl") || contains(f,Filesystem.path_separator)
        # specifying file path
        OldRequire.require(f)
    else
        # require("Foo") --- ambiguous. might be file or package
        filename = maybe_require_file(f)
        if filename == f
            mod = Symbol(require_modname(f))
            M = current_module()
            if isdefined(M,mod) && isa(eval(M,mod),Module)
                return
            end
            require(mod)
        else
            OldRequire.require(f)
        end
    end
end
@noinline function require(f::AbstractString, fs::AbstractString...)
    require(f)
    for fn in fs
        require(fn)
    end
end
export require

## ropes for efficient concatenation, etc. ##

immutable RopeString <: AbstractString
    head::AbstractString
    tail::AbstractString
    depth::Int32
    endof::Int

    @inline function _new(h, t, d, e)
        depwarn("`RopeString` is deprecated, use `string` instead", :RopeString)
        new(h, t, d, e)
    end

    @noinline RopeString(h::RopeString, t::RopeString) =
        strdepth(h.tail) + strdepth(t) < strdepth(h.head) ?
            RopeString(h.head, RopeString(h.tail, t)) :
            _new(h, t, max(h.depth,t.depth)+1, endof(h)+endof(t))

    @noinline RopeString(h::RopeString, t::AbstractString) =
        strdepth(h.tail) < strdepth(h.head) ?
            RopeString(h.head, RopeString(h.tail, t)) :
            _new(h, t, h.depth+1, endof(h)+endof(t))

    @noinline RopeString(h::AbstractString, t::RopeString) =
        strdepth(t.head) < strdepth(t.tail) ?
            RopeString(RopeString(h, t.head), t.tail) :
            _new(h, t, t.depth+1, endof(h)+endof(t))

    @noinline RopeString(h::AbstractString, t::AbstractString) =
        _new(h, t, 1, endof(h)+endof(t))
end
RopeString(s::AbstractString) = RopeString(s,"")

strdepth(s::AbstractString) = 0
strdepth(s::RopeString) = s.depth

function next(s::RopeString, i::Int)
    eh = endof(s.head)
    if i <= eh
        return next(s.head, i)
    else
        c, j = next(s.tail, i-eh)
        return c, j+eh
    end
end

endof(s::RopeString) = s.endof
length(s::RopeString) = length(s.head) + length(s.tail)
write(io::IO, s::RopeString) = (write(io, s.head); write(io, s.tail))
sizeof(s::RopeString) = sizeof(s.head) + sizeof(s.tail)

export RopeString

@noinline function complement!(s::IntSet)
    depwarn("complement IntSets are deprecated", :complement!)
    for n = 1:length(s.bits)
        s.bits[n] = ~s.bits[n]
    end
    s.fill1s = !s.fill1s
    s
end
@noinline complement(s::IntSet) = complement!(copy(s))
export complement, complement!


# 11774
# when removing these deprecations, move them to reduce.jl, remove the depwarns and uncomment the errors.

nonboolean_warning(f, op, status) = """

    Using non-boolean collections with $f(itr) is $status, use reduce($op, itr) instead.
    If you are using $f(map(f, itr)) or $f([f(x) for x in itr]), use $f(f, itr) instead.
"""


@noinline function or_bool_only(a, b)
    depwarn(nonboolean_warning(:any, :|, "deprecated"), :or_bool_only)
    a|b
end
or_bool_only(a::Bool, b::Bool) = a|b

@noinline function and_bool_only(a, b)
    depwarn(nonboolean_warning(:all, :&, "deprecated"), :and_bool_only)
    a&b
end
and_bool_only(a::Bool, b::Bool) = a&b

@deprecate iseltype(x,T)  eltype(x) <: T

@deprecate_binding FloatingPoint AbstractFloat

# 11447

@noinline function Regex(pattern::AbstractString, options::Integer)
    flags = string([opt & options != 0? flag : ""
        for (opt,flag) in [
            (PCRE.CASELESS,  "i"),
            (PCRE.MULTILINE, "m"),
            (PCRE.DOTALL,    "s"),
            (PCRE.EXTENDED,  "x")
        ]
    ]...)
    depwarn("Constructing regexes with integer flags is deprecated, "*
            "use string flags instead: Regex(\"$pattern\", \"$flags\").", :Regex)
    Regex(pattern, flags)
end

@deprecate cartesianmap(f, dims) for idx in CartesianRange(dims); f(idx.I...); end

@deprecate Union(args...) Union{args...}

# 0.5 deprecations

# 12839
const AsyncStream = IO
deprecate(:AsyncStream)

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

@deprecate utf8(p::Ptr{UInt8}, len::Integer) String(p, len)
@deprecate utf8(p::Ptr{UInt8}) String(p)
@deprecate utf8(v::Vector{UInt8}) String(v)
@deprecate utf8(s::AbstractString) String(s)
@deprecate utf8(x) convert(String, x)

@deprecate ascii(p::Ptr{UInt8}, len::Integer) ascii(String(p, len))
@deprecate ascii(p::Ptr{UInt8}) ascii(String(p))
@deprecate ascii(v::Vector{UInt8}) ascii(String(v))
@deprecate ascii(x) ascii(convert(String, x))

@deprecate bytestring(s::Cstring) String(s)
@deprecate bytestring(v::Vector{UInt8}) String(v)
@deprecate bytestring(io::Base.AbstractIOBuffer) String(io)
@deprecate bytestring(p::Union{Ptr{Int8},Ptr{UInt8}}) String(p)
@deprecate bytestring(p::Union{Ptr{Int8},Ptr{UInt8}}, len::Integer) String(p,len)
@deprecate bytestring(s::AbstractString...) string(s...)

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
    for i = 1:size(v,1)             # fixme (iter): update when #15459 is done
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



# During the 0.5 development cycle, do not add any deprecations below this line
# To be deprecated in 0.6

const _oldstyle_array_vcat_ = false

# End deprecations scheduled for 0.6
