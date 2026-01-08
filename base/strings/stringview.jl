const DenseStringView = StringView{<:Union{DenseVector{UInt8}, <:FastContiguousSubArray{UInt8, 1, <:DenseVector{UInt8}}}}
const StringAndSub = Union{String, SubString{String}}
const StringViewAndSub = Union{StringView, SubString{<:StringView}}
const DenseStringViewAndSub = Union{DenseStringView, SubString{<:DenseStringView}}
const DenseUTF8String = Union{DenseStringViewAndSub, StringAndSub}
const UTF8String = Union{StringAndSub, StringViewAndSub}

StringView(v::AbstractVector{UInt8}) = StringView{typeof(v)}(v)
Vector{UInt8}(s::StringViewAndSub) = Vector{UInt8}(codeunits(s))
Array{UInt8}(s::StringViewAndSub) = Vector{UInt8}(s)
String(s::StringViewAndSub) = String(copyto!(StringVector(ncodeunits(s)), codeunits(s)))
copy(s::StringView) = StringView(copy(s.data))

function Symbol(s::DenseStringViewAndSub)
    return ccall(:jl_symbol_n, Ref{Symbol}, (Ptr{UInt8}, Int), s, ncodeunits(s))
end

pointer(s::DenseStringView) = pointer(s.data)
pointer(s::DenseStringView, i::Integer) = pointer(s.data, i)
pointer(x::SubString{<:DenseStringView}) = pointer(x.string) + x.offset
pointer(x::SubString{<:DenseStringView}, i::Integer) = pointer(x.string) + x.offset + (i - 1)

unsafe_convert(::Type{Ptr{UInt8}}, s::DenseStringViewAndSub) = pointer(s)
unsafe_convert(::Type{Ptr{Int8}}, s::DenseStringViewAndSub) = convert(Ptr{Int8}, pointer(s))

cconvert(::Type{Ptr{UInt8}}, s::DenseStringViewAndSub) = s
cconvert(::Type{Ptr{Int8}}, s::DenseStringViewAndSub) = s

function Base.reverse(s::StringViewAndSub)
    mem = Memory{UInt8}(undef, ncodeunits(s))
    offs = sizeof(s) + 1
    for c in s
        offs -= ncodeunits(c)
        # Since StringView is generic over the wrapped array, we could invoke UB
        # if we don't validate the array behaves as expected.
        offs < 1 && error("Invalid implementation of vector length")
        __unsafe_string!(mem, c, offs)
    end
    return StringView(mem)
end

sizeof(s::StringView) = length(s.data)
ncodeunits(s::StringView) = length(s.data)
codeunit(::StringView) = UInt8
@propagate_inbounds codeunit(s::StringView, i::Integer) = s.data[i]
codeunits(s::StringView) = s.data
codeunits(s::SubString{<:StringView}) = @view s.string.data[(1 + s.offset):(s.offset + s.ncodeunits)]

# For UTF8 encoded strings, we can operate on codeunits directly.
# For non-UTF8 strings, we use the AbstractString fallback
cmp(a::UTF8String, b::UTF8String) = cmp(codeunits(a), codeunits(b))
==(a::UTF8String, b::UTF8String) = codeunits(a) == codeunits(b)

# Typemin and one is the empty string (multiplicative identity)
typemin(::Type{StringView{Vector{UInt8}}}) = StringView(Vector{UInt8}(undef, 0))
typemin(::Type{StringView{CodeUnits{UInt8, String}}}) = StringView(CodeUnits(""))
typemin(::T) where {T <: StringView} = typemin(T)
one(::Union{T, Type{T}}) where {T <: StringView} = typemin(T)
oneunit(::Union{T, Type{T}}) where {T <: StringView} = typemin(T)

# Forward to optimised isascii(::AbstractVector{UInt8})
isascii(s::StringViewAndSub) = isascii(codeunits(s))

# For dense string views, pointer-based hashing is faster than array based.
function hash(s::DenseStringViewAndSub, h::UInt)
    GC.@preserve s hash_bytes(pointer(s), ncodeunits(s), UInt64(h), HASH_SECRET) % UInt
end

# The canonical binary representation of strings is simply their byte content.
write(io::IO, s::StringViewAndSub) = write(io, codeunits(s))::Int
print(io::IO, s::StringViewAndSub) = (write(io, s); nothing)

@propagate_inbounds thisind(s::StringViewAndSub, i::Integer) = _thisind_str(s, Int(i)::Int)
@propagate_inbounds thisind(s::StringViewAndSub, i::Int) = _thisind_str(s, i)

@propagate_inbounds nextind(s::StringViewAndSub, i::Integer) = _nextind_str(s, Int(i)::Int)
@propagate_inbounds nextind(s::StringViewAndSub, i::Int) = _nextind_str(s, i)

isvalid(s::StringViewAndSub, i::Int) = checkbounds(Bool, s, i) && thisind(s, i) == i

# This is different from the String implementation, because when r is empty,
# we cannot just return the constant "".
@inline function getindex(s::StringView, r::UnitRange{Int})
    cu = codeunits(s)
    isempty(r) && return StringView(cu[1:0])
    i, j = first(r), last(r)
    @boundscheck begin
        checkbounds(cu, r)
        @inbounds isvalid(s, i) || string_index_err(s, i)
        @inbounds isvalid(s, j) || string_index_err(s, j)
    end
    j = nextind(s, j) - 1
    return StringView(cu[i:j])
end

function chomp(s::StringViewAndSub)
    cu = codeunits(s)
    ncu = length(cu)
    len = if iszero(ncu)
        0
    else
        has_lf = cu[ncu] == 0x0a
        two_bytes = ncu > 1
        has_cr = has_lf & two_bytes & (cu[ncu - two_bytes] == 0x0d)
        ncu - (has_lf + has_cr)
    end
    off = s isa StringView ? 0 : s.offset
    par = s isa StringView ? s : s.string
    T = s isa SubString ? typeof(s) : SubString{typeof(s)}
    return @inbounds @inline T(par, off, len, Val{:noshift}())
end

function replace(io::IO, s::DenseStringViewAndSub, pat_f::Pair...; count = typemax(Int))
    return _replace_(io, s, pat_f, Int(count))
end
