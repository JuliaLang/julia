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
    if bool(compileropts().depwarn)
        bt = backtrace()
        caller = firstcaller(bt, funcsym)
        warn(msg, once=(caller!=C_NULL), key=caller, bt=bt)
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

# 0.3 deprecations

function nfilled(X)
    depwarn("nfilled has been renamed to nnz", :nfilled)
    nnz(X)
end
export nfilled

@deprecate nonzeros(A::StridedArray) A[find(A)]
@deprecate nonzeros(B::BitArray) trues(countnz(B))
@deprecate nnz(A::StridedArray) countnz(A)

@deprecate dense  full

export Stat
const Stat = StatStruct

export CharString
const CharString = UTF32String
@deprecate UTF32String(c::Integer...) utf32(c...)
@deprecate UTF32String(s::AbstractString) utf32(s)

export Ranges
const Ranges = Range

export Range1
const Range1 = UnitRange

@deprecate clear_malloc_data() Profile.clear_malloc_data()

@deprecate set_rounding(r::RoundingMode) set_rounding(Float64,r)
@deprecate get_rounding() get_rounding(Float64)
@deprecate with_rounding(f::Function, r::RoundingMode) with_rounding(f::Function, Float64, r)

@deprecate set_bigfloat_rounding(r::RoundingMode) set_rounding(BigFloat,r)
@deprecate get_bigfloat_rounding() get_rounding(BigFloat)
@deprecate with_bigfloat_rounding(f::Function, r::RoundingMode) with_rounding(f::Function, BigFloat, r)
eval(Sys, :(@deprecate shlib_list dllist))
# Sys.shlib_ext is deprecated, renamed to Sys.dlext. Remove alias before release

@deprecate degrees2radians deg2rad
@deprecate radians2degrees rad2deg

@deprecate spzeros(m::Integer) spzeros(m, m)
@deprecate spzeros(Tv::Type, m::Integer) spzeros(Tv, m, m)

@deprecate myindexes localindexes

@deprecate setfield setfield!
@deprecate put      put!
@deprecate take     take!

@deprecate Set(a, b...) Set(Any[a, b...])
# for a bit of backwards compatibility
IntSet(xs::Integer...) = (s=IntSet(); for a in xs; push!(s,a); end; s)
Set{T<:Number}(xs::T...) = Set{T}(xs)

@deprecate normfro(A) vecnorm(A)

@deprecate convert{T}(p::Type{Ptr{T}}, a::Array) convert(p, pointer(a))

@deprecate read(from::IOBuffer, a::Array)            read!(from, a)
@deprecate read(from::IOBuffer, p::Ptr, nb::Integer) read!(from, p, nb)
@deprecate read(s::IOStream, a::Array)               read!(s, a)
@deprecate read(this::AsyncStream, a::Array)         read!(this, a)
@deprecate read(f::File, a::Array, nel)              read!(f, a, nel)
@deprecate read(f::File, a::Array)                   read!(f, a)
@deprecate read(s::IO, a::Array)                     read!(s, a)
@deprecate read(s::IO, B::BitArray)                  read!(s, B)

@deprecate nans{T}(::Type{T}, dims...)   fill(convert(T,NaN), dims)
@deprecate nans(dims...)                 fill(NaN, dims)
@deprecate nans{T}(x::AbstractArray{T})  fill(convert(T,NaN), size(x))
@deprecate infs{T}(::Type{T}, dims...)   fill(convert(T,Inf), dims)
@deprecate infs(dims...)                 fill(Inf, dims)
@deprecate infs{T}(x::AbstractArray{T})  fill(convert(T,Inf), size(x))

@deprecate bitmix(x, y::UInt)                 hash(x, y)
@deprecate bitmix(x, y::Int)                  hash(x, uint(y))
@deprecate bitmix(x, y::Union(UInt32, Int32)) convert(UInt32, hash(x, uint(y)))
@deprecate bitmix(x, y::Union(UInt64, Int64)) convert(UInt64, hash(x, hash(y)))

@deprecate readsfrom(cmd, args...)      open(cmd, "r", args...)
@deprecate writesto(cmd, args...)      open(cmd, "w", args...)

function tty_rows()
    depwarn("tty_rows() is deprecated, use tty_size() instead", :tty_rows)
    tty_size()[1]
end
function tty_cols()
    depwarn("tty_cols() is deprecated, use tty_size() instead", :tty_cols)
    tty_size()[2]
end

@deprecate pointer{T}(::Type{T}, x::UInt) convert(Ptr{T}, x)
@deprecate pointer{T}(::Type{T}, x::Ptr) convert(Ptr{T}, x)

# 0.3 discontinued functions

scale!{T<:Base.LinAlg.BlasReal}(X::Array{T}, s::Complex) = error("scale!: Cannot scale a real array by a complex value in-place.  Use scale(X::Array{Real}, s::Complex) instead.")

@deprecate which(f, args...) @which f(args...)
@deprecate rmdir rm

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
@deprecate Dict{K,V}(ks::(K...), vs::(V...))                     Dict{K,V}(zip(ks, vs))
@deprecate Dict{K}(ks::(K...), vs::Tuple)                        Dict{K,Any}(zip(ks, vs))
@deprecate Dict{V}(ks::Tuple, vs::(V...))                        Dict{Any,V}(zip(ks, vs))
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

export Base64Pipe, base64
const Base64Pipe = Base64EncodePipe
const base64 = base64encode

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

@deprecate map!(f::Callable, dest::StridedArray, A::StridedArray, B::Number) broadcast!(f, dest, A, B)
@deprecate map!(f::Callable, dest::StridedArray, A::Number, B::StridedArray) broadcast!(f, dest, A, B)

#9295
@deprecate push!(t::Associative, key, v)  setindex!(t, v, key)

# 0.4 discontinued functions

function subtypetree(x::DataType, level=-1)
    depwarn("`subtypetree` is discontinued", :subtypetree)
    (level == 0 ? (x, []) : (x, Any[subtypetree(y, level-1) for y in subtypes(x)]))
end

# 8898
@deprecate precision(x::DateTime) eps(x)
@deprecate precision(x::Date) eps(x)

# Histogram: moved to StatsBase (#6842)
function histrange{T<:FloatingPoint,N}(v::AbstractArray{T,N}, n::Integer)
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
    nv = length(v)
    if nv == 0 && n < 0
        throw(ArgumentError("number of bins must be ≥ 0 for an empty array, got $n"))
    elseif nv > 0 && n < 1
        throw(ArgumentError("number of bins must be ≥ 1 for a non-empty array, got $n"))
    end
    if nv == 0
        return 0:1:0
    end
    lo, hi = extrema(v)
    if hi == lo
        step = 1
    else
        bw = (hi - lo) / n
        e = 10^max(0,floor(Int,log10(bw)))
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
midpoints(r::Range) = (depwarn("midpoints(x) is deprecated. Method now in StatsBase.jl"); r[1:length(r)-1] + 0.5*step(r))
midpoints(v::AbstractVector) = (depwarn("midpoints(x) is deprecated. Method now in StatsBase.jl"); [0.5*(v[i] + v[i+1]) for i in 1:length(v)-1])

## hist ##
function sturges(n)  # Sturges' formula
    n==0 && return one(n)
    ceil(Int,log2(n))+1
end

function hist!{HT}(h::AbstractArray{HT}, v::AbstractVector, edg::AbstractVector; init::Bool=true)
    depwarn("hist(...) and hist!(...) are deprecated. Use fit(Histogram,...) in StatsBase.jl instead.")
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
    depwarn("hist(...) and hist!(...) are deprecated. Use fit(Histogram,...) in StatsBase.jl instead.")
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
    depwarn("hist2d(...) is deprecated. Use fit(Histogram,...) in StatsBase.jl instead.")
    size(v,2) == 2 || throw(DimensionMismatch("hist2d requires an Nx2 matrix"))
    n = length(edg1) - 1
    m = length(edg2) - 1
    size(H) == (n, m) || throw(DimensionMismatch("incorrect size of histogram"))
    if init
        fill!(H, zero(HT))
    end
    for i = 1:size(v,1)
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
