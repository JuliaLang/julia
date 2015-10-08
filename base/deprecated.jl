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

remove_linenums!(ex) = ex
function remove_linenums!(ex::Expr)
    filter!(x->!((isa(x,Expr) && is(x.head,:line)) || isa(x,LineNumberNode)), ex.args)
    for subex in ex.args
        remove_linenums!(subex)
    end
    ex
end

function depwarn(msg, funcsym)
    opts = JLOptions()
    if opts.depwarn > 0
        ln = unsafe_load(cglobal(:jl_lineno, Int))
        fn = bytestring(unsafe_load(cglobal(:jl_filename, Ptr{Cchar})))
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
        lkup = ccall(:jl_lookup_code_address, Any, (Ptr{Void},Cint), bt[i], true)
        i += 1
        if lkup === ()
            continue
        end
        fname, file, line, inlinedat_file, inlinedat_line, fromC = lkup
        if fname == funcsym
            break
        end
    end
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
    return Core._apply(call, f, args...)
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

@deprecate_binding String AbstractString

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

for (f,t) in ((:uint8,:UInt8), (:uint16,:UInt16), (:uint32,:UInt32), (:uint64,:Uint64),
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

@deprecate sub2ind{T<:Integer}(dims::Array{T}, sub::Array{T}) sub2ind(tuple(dims...), sub...)
@deprecate ind2sub!{T<:Integer}(sub::Array{T}, dims::Array{T}, ind::T) ind2sub!(sub, tuple(dims...), ind)

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
    P<:Ptr || throw(MethodError(unsafe_convert, (Type{P}, x)))
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
    depwarn("indexing with non Integer Reals is deprecated", :to_index)
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
@deprecate is_valid_ascii(str::ASCIIString) isvalid(str)
@deprecate is_valid_utf8(str::UTF8String)   isvalid(str)
@deprecate is_valid_utf16(str::UTF16String) isvalid(str)
@deprecate is_valid_utf32(str::UTF32String) isvalid(str)
@deprecate is_valid_char(ch)   isvalid(Char, ch)
@deprecate is_valid_ascii(str) isvalid(ASCIIString, str)
@deprecate is_valid_utf8(str)  isvalid(UTF8String, str)
@deprecate is_valid_utf16(str) isvalid(UTF16String, str)
@deprecate is_valid_utf32(str) isvalid(UTF32String, str)

# 11379
@deprecate utf32(c::Integer...)   UTF32String(UInt32[c...,0])

# 12087
@deprecate call(P::Base.DFT.Plan, A) P * A
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
    depwarn("@math_const is deprecated and renamed to @irrational.", symbol("@math_const"))
    :(@irrational $(esc(sym)) $(esc(val)) $(esc(def)))
end
export @math_const

# 11280, mmap

export msync
msync{T}(A::Array{T}) = msync(pointer(A), length(A)*sizeof(T))
msync(B::BitArray) = msync(pointer(B.chunks), length(B.chunks)*sizeof(UInt64))

@unix_only begin
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
    offset_page::FileOffset = floor(Integer,offset/pagesize)*pagesize
    len_page::Int = (offset-offset_page) + len
    # Mmap the file
    p = ccall(:jl_mmap, Ptr{Void}, (Ptr{Void}, Csize_t, Cint, Cint, Cint, FileOffset), C_NULL, len_page, prot, flags, fd, offset_page)
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
@noinline function msync(p::Ptr, len::Integer, flags::Integer=MS_SYNC)
    depwarn("`msync` is deprecated, use `Mmap.sync!(array)` instead", :msync)
    systemerror("msync", ccall(:msync, Cint, (Ptr{Void}, Csize_t, Cint), p, len, flags) != 0)
end
end


@windows_only begin
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

@unix_only @deprecate mmap_array{T,N}(::Type{T}, dims::NTuple{N,Integer}, s::IO, offset=position(s)) Mmap.mmap(s, Array{T,N}, dims, offset)

@windows_only begin
type SharedMemSpec
    name :: AbstractString
    readonly :: Bool
    create :: Bool
end
export mmap_array
@noinline function mmap_array{T,N}(::Type{T}, dims::NTuple{N,Integer}, s::Union{IO,SharedMemSpec}, offset::FileOffset)
    depwarn("`mmap_array` is deprecated, use `Mmap.mmap(io, Array{T,N}, dims, offset)` instead to return an mmapped-array", :mmap_array)
    if isa(s,SharedMemSpec)
        a = Mmap.Anonymous(s.name, s.readonly, s.create)
    else
        a = s
    end
    return Mmap.mmap(a, Array{T,N}, dims, offset)
end
end

@deprecate mmap_bitarray{N}(::Type{Bool}, dims::NTuple{N,Integer}, s::IOStream, offset::FileOffset=position(s)) mmap(s, BitArray, dims, offset)
@deprecate mmap_bitarray{N}(dims::NTuple{N,Integer}, s::IOStream, offset=position(s)) mmap(s, BitArray, dims, offset)

# T[a:b] and T[a:s:b]
@noinline function getindex{T<:Union{Char,Number}}(::Type{T}, r::Range)
    depwarn("T[a:b] concatenation is deprecated; use T[a:b;] instead", :getindex)
    copy!(Array(T,length(r)), r)
end

@noinline function getindex{T<:Union{Char,Number}}(::Type{T}, r1::Range, rs::Range...)
    depwarn("T[a:b,...] concatenation is deprecated; use T[a:b;...] instead", :getindex)
    a = Array(T,length(r1)+sum(length,rs))
    o = 1
    copy!(a, o, r1)
    o += length(r1)
    for r in rs
        copy!(a, o, r)
        o += length(r)
    end
    return a
end

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
    if endswith(f,".jl") || contains(f,path_separator)
        # specifying file path
        OldRequire.require(f)
    else
        # require("Foo") --- ambiguous. might be file or package
        filename = maybe_require_file(f)
        if filename == f
            mod = symbol(require_modname(f))
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
    depwarn("complement IntSets are deprecated", :complement!);
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


@noinline function nonboolean_any(itr)
    depwarn(nonboolean_warning(:any, :|, "deprecated"), :nonboolean_any)
    #throw(ArgumentError(nonboolean_warning(:any, :|, "not supported")))
    reduce(|, itr)
end

@noinline function nonboolean_all(itr)
    depwarn(nonboolean_warning(:all, :&, "deprecated"), :nonboolean_all)
    #throw(ArgumentError(nonboolean_warning(:all, :&, "not supported")))
    reduce(&, itr)
end

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

#13465
@deprecate cov(x::AbstractVector; corrected=true, mean=Base.mean(x)) covm(x, mean, corrected)
@deprecate cov(X::AbstractMatrix; vardim=1, corrected=true, mean=Base.mean(X, vardim)) covm(X, mean, vardim, corrected)
@deprecate cov(x::AbstractVector, y::AbstractVector; corrected=true, mean=(Base.mean(x), Base.mean(y))) covm(x, mean[1], y, mean[2], corrected)
@deprecate cov(X::AbstractVecOrMat, Y::AbstractVecOrMat; vardim=1, corrected=true, mean=(Base.mean(X, vardim), Base.mean(Y, vardim))) covm(X, mean[1], Y, mean[2], vardim, corrected)

@deprecate cor(x::AbstractVector; mean=Base.mean(x)) corm(x, mean)
@deprecate cor(X::AbstractMatrix; vardim=1, mean=Base.mean(X, vardim)) corm(X, mean, vardim)
@deprecate cor(x::AbstractVector, y::AbstractVector; mean=(Base.mean(x), Base.mean(y))) corm(x, mean[1], y, mean[2])
@deprecate cor(X::AbstractVecOrMat, Y::AbstractVecOrMat; vardim=1, mean=(Base.mean(X, vardim), Base.mean(Y, vardim))) corm(X, mean[1], Y, mean[2], vardim)

@deprecate_binding SparseMatrix SparseArrays

#13496
@deprecate A_ldiv_B!(A::SparseMatrixCSC, B::StridedVecOrMat) A_ldiv_B!(factorize(A), B)
