macro deprecate(old,new)
    if isa(old,Symbol)
        oldname = Expr(:quote,old)
        newname = Expr(:quote,new)
        Expr(:toplevel,
            Expr(:export,esc(old)),
            :(function $(esc(old))(args...)
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
                  depwarn(string($oldcall," is deprecated, use ",$newcall," instead."),
                          $oldname)
                  $(esc(new))
              end))
    else
        error("invalid usage of @deprecate")
    end
end

function depwarn(msg, funcsym)
    bt = backtrace()
    caller = firstcaller(bt, funcsym)
    warn(msg, once=(caller!=C_NULL), key=caller, bt=bt)
end

function firstcaller(bt::Array{Ptr{None},1}, funcsym::Symbol)
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
@deprecate UTF32String(s::String) utf32(s)

export Ranges
const Ranges = Range

export Range1
const Range1 = UnitRange

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

@deprecate Set(a, b...) Set({a, b...})
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

@deprecate bitmix(x, y::Uint)                 hash(x, y)
@deprecate bitmix(x, y::Int)                  hash(x, uint(y))
@deprecate bitmix(x, y::Union(Uint32, Int32)) convert(Uint32, hash(x, uint(y)))
@deprecate bitmix(x, y::Union(Uint64, Int64)) convert(Uint64, hash(x, hash(y)))

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

@deprecate pointer{T}(::Type{T}, x::Uint) convert(Ptr{T}, x)
@deprecate pointer{T}(::Type{T}, x::Ptr) convert(Ptr{T}, x)

# 0.3 discontinued functions

scale!{T<:Base.LinAlg.BlasReal}(X::Array{T}, s::Complex) = error("scale!: Cannot scale a real array by a complex value in-place.  Use scale(X::Array{Real}, s::Complex) instead.")

@deprecate which(f::Callable, args...) @which f(args...)
@deprecate rmdir rm
