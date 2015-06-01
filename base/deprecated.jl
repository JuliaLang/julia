# This file is a part of Julia. License is MIT: http://julialang.org/license
#
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
    if JLOptions().depwarn != 0
        ln = unsafe_load(cglobal(:jl_lineno, Int))
        fn = bytestring(unsafe_load(cglobal(:jl_filename, Ptr{Cchar})))
        bt = backtrace()
        caller = firstcaller(bt, funcsym)
        warn(msg, once=(caller != C_NULL), key=caller, bt=bt,
             filename=fn, lineno=ln)
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
        fname, file, line, fromC = lkup
        if fname == funcsym
            break
        end
    end
    if i <= length(bt)
        return bt[i]
    end
    return C_NULL
end

# 0.4 deprecations

@deprecate split(x,y,l::Integer,k::Bool) split(x,y;limit=l,keep=k)
@deprecate split(x,y,l::Integer) split(x,y;limit=l)
@deprecate split(x,y,k::Bool) split(x,y;keep=k)

@deprecate rsplit(x,y,l::Integer,k::Bool) rsplit(x,y;limit=l,keep=k)
@deprecate rsplit(x,y,l::Integer) rsplit(x,y;limit=l)
@deprecate rsplit(x,y,k::Bool) rsplit(x,y;keep=k)

export UdpSocket
const TcpSocket = TCPSocket
const UdpSocket = UDPSocket
const IpAddr = IPAddr

@deprecate isblank(c::Char) c == ' ' || c == '\t'
@deprecate isblank(s::AbstractString) all(c -> c == ' ' || c == '\t', s)

export Nothing
const Nothing = Void

export None
const None = Union()

export apply
function apply(f, args...)
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

@deprecate inf(x::FloatingPoint)  oftype(x,Inf)
@deprecate nan(x::FloatingPoint)  oftype(x,NaN)
@deprecate inf{T<:FloatingPoint}(::Type{T})  convert(T,Inf)
@deprecate nan{T<:FloatingPoint}(::Type{T})  convert(T,NaN)

export String
const String = AbstractString

export Uint, Uint8, Uint16, Uint32, Uint64, Uint128
const Uint = UInt
const Uint8 = UInt8
const Uint16 = UInt16
const Uint32 = UInt32
const Uint64 = UInt64
const Uint128 = UInt128

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

export Base64Pipe
const Base64Pipe = Base64EncodePipe
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

export MemoryError
const MemoryError = OutOfMemoryError

@deprecate map!(f::Callable, dest::StridedArray, A::StridedArray, B::Number) broadcast!(f, dest, A, B)
@deprecate map!(f::Callable, dest::StridedArray, A::Number, B::StridedArray) broadcast!(f, dest, A, B)

#9295
@deprecate push!(t::Associative, key, v)  setindex!(t, v, key)

@deprecate (|>)(src::AbstractCmd,    dest::AbstractCmd)    pipe(src, dest)
@deprecate (.>)(src::AbstractCmd,    dest::AbstractCmd)    pipe(src, stderr=dest)
@deprecate (|>)(src::Redirectable,   dest::AbstractCmd)    pipe(src, dest)
@deprecate (|>)(src::AbstractCmd,    dest::Redirectable)   pipe(src, dest)
@deprecate (.>)(src::AbstractCmd,    dest::Redirectable)   pipe(src, stderr=dest)
@deprecate (|>)(src::AbstractCmd,    dest::AbstractString) pipe(src, dest)
@deprecate (|>)(src::AbstractString, dest::AbstractCmd)    pipe(src, dest)
@deprecate (.>)(src::AbstractCmd,    dest::AbstractString) pipe(src, stderr=dest)
@deprecate (>>)(src::AbstractCmd,    dest::AbstractString) pipe(src, stdout=dest, append=true)
@deprecate (.>>)(src::AbstractCmd,   dest::AbstractString) pipe(src, stderr=dest, append=true)

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

for (f,t) in ((:integer, Integer), (:signed, Signed),
              (:unsigned, Unsigned), (:int, Int), (:int8, Int8), (:int16, Int16),
              (:int32, Int32), (:int64, Int64), (:int128, Int128), (:uint, UInt),
              (:uint8, UInt8), (:uint16, UInt16), (:uint32, UInt32), (:uint64, UInt64),
              (:uint128, UInt128))
    @eval begin
        @deprecate $f(x::AbstractArray) round($t, x)
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
@deprecate char(x::FloatingPoint)  Char(round(UInt32,x))
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
        @deprecate ($f)(x::FloatingPoint)  round($t,x)
        @deprecate ($f)(x::Rational)       round($t,x)
    end
end

@deprecate integer(x::Ptr)   convert(UInt, x)
@deprecate unsigned(x::Ptr)  convert(UInt, x)

for (f,t) in ((:int,    Int), (:int8,   Int8), (:int16,  Int16), (:int32,  Int32),
              (:int64,  Int64), (:int128, Int128), (:uint,   UInt), (:uint8,  UInt8),
              (:uint16, UInt16), (:uint32, UInt32), (:uint64, UInt64), (:uint128,UInt128))
    @eval begin
        @deprecate ($f){S<:AbstractString}(a::AbstractArray{S}) [parse($t,s) for s in a]
    end
end
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
@deprecate mmap         Libc.mmap
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

function push!(A)
    depwarn("push!(A) has been deprecated", :push!)
    A
end

# 10458
to_index_nodep(i::Real) = convert(Int,i)::Int

function to_index(i::Real)
    depwarn("indexing with non Integer Reals is deprecated", :to_index)
    to_index_nodep(i)
end

function to_index{T<:Real}(r::UnitRange{T})
    depwarn("indexing with non Integer UnitRanges is deprecated", :to_index)
    to_index_nodep(first(r)):to_index_nodep(last(r))
end

function to_index{T<:Real}(r::StepRange{T})
    depwarn("indexing with non Integer StepRanges is deprecated", :to_index)
    to_index_nodep(first(r)):to_index_nodep(step(r)):to_index_nodep(last(r))
end

function to_index{T<:Real}(A::AbstractArray{T})
    depwarn("indexing with non Integer AbstractArrays is deprecated", :to_index)
    Int[to_index_nodep(x) for x in A]
end

function float_isvalid{T<:Union(Float32,Float64)}(s::AbstractString, out::Array{T,1})
    tf = tryparse(T, s)
    isnull(tf) || (out[1] = get(tf))
    !isnull(tf)
end

function float32_isvalid(s::AbstractString, out::Array{Float32,1})
    depwarn("float32_isvalid is deprecated, use tryparse(Float32,s) instead", :float32_isvalid)
    float_isvalid(s, out)
end

function float64_isvalid(s::AbstractString, out::Array{Float64,1})
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
