
const DenseStringView = StringView{<:Union{DenseVector{UInt8}, <:Base.FastContiguousSubArray{UInt8, 1, <:DenseVector{UInt8}}}}
const StringAndSub = Union{String, SubString{String}}
const StringViewAndSub = Union{StringView, SubString{<:StringView}}
const DenseStringViewAndSub = Union{DenseStringView, SubString{<:DenseStringView}}
const DenseString = Union{StringViewAndSub, StringAndSub}

Vector{UInt8}(s::StringViewAndSub) = Vector{UInt8}(codeunits(s))
Array{UInt8}(s::StringViewAndSub) = Vector{UInt8}(s)
String(s::StringViewAndSub) = String(copyto!(StringVector(ncodeunits(s)), codeunits(s)))
StringView(s::StringView) = s
StringView{S1}(s::StringView{S2}) where {S1 <: AbstractVector{UInt8}, S2 <: S1} = s
copy(s::StringView) = StringView(copy(s.data))

function Symbol(s::DenseStringViewAndSub)
    ccall(:jl_symbol_n, Ref{Symbol}, (Ptr{UInt8}, Int), s, ncodeunits(s))
end

pointer(s::DenseStringView) = pointer(s.data)
pointer(s::DenseStringView, i::Integer) = pointer(s.data, i)
pointer(x::SubString{<:DenseStringView}) = pointer(x.string) + x.offset
pointer(x::SubString{<:DenseStringView}, i::Integer) = pointer(x.string) + x.offset + (i - 1)

unsafe_convert(::Type{Ptr{UInt8}}, s::DenseStringViewAndSub) = pointer(s)
unsafe_convert(::Type{Ptr{Int8}}, s::DenseStringViewAndSub) = convert(Ptr{Int8}, pointer(s))

cconvert(::Type{Ptr{UInt8}}, s::DenseStringViewAndSub) = s
cconvert(::Type{Ptr{Int8}}, s::DenseStringViewAndSub) = s

sizeof(s::StringView) = length(s.data)
ncodeunits(s::StringView) = length(s.data)
codeunit(::StringView) = UInt8
@propagate_inbounds codeunit(s::StringView, i::Integer) = s.data[i]
codeunits(s::StringView) = s.data
codeunits(s::SubString{<:StringView}) = @view s.string.data[(1 + s.offset):(s.offset + s.ncodeunits)]

# TODO: cmp here must be refactored
function _cmp(a, b)
    al, bl = sizeof(a), sizeof(b)
    c = GC.@preserve a b memcmp(pointer(a), pointer(b), min(al, bl))
    return c < 0 ? -1 : c > 0 ? +1 : cmp(al, bl)
end

cmp(a::DenseStringViewAndSub, b::DenseStringViewAndSub) = _cmp(a, b)
cmp(a::DenseStringViewAndSub, b::StringAndSub) = _cmp(a, b)
cmp(a::StringAndSub, b::DenseStringViewAndSub) = _cmp(a, b)

==(s1::StringViewAndSub, s2::StringViewAndSub) = codeunits(s1) == codeunits(s2)
==(s1::StringAndSub, s2::StringViewAndSub) = codeunits(s1) == codeunits(s2)
function ==(a::StringAndSub, b::DenseStringViewAndSub)
    al = sizeof(a)
    al == sizeof(b) || return false
    return al == sizeof(b) && 0 == memcmp(a, b, al)
end
==(s1::StringViewAndSub, s2::StringAndSub) = s2 == s1

typemin(::Type{StringView{Vector{UInt8}}}) = StringView(Vector{UInt8}(undef, 0))
typemin(::Type{StringView{Base.CodeUnits{UInt8, String}}}) = StringView("")
typemin(::T) where {T <: StringView} = typemin(T)
one(::Union{T, Type{T}}) where {T <: StringView} = typemin(T)

# Forward to optimised Base.isascii(::AbstractVector{UInt8})
isascii(s::StringViewAndSub) = isascii(codeunits(s))

# The canonical binary representation of strings is simply their byte content.
write(io::IO, s::StringViewAndSub) = write(io, codeunits(s))
print(io::IO, s::StringViewAndSub) = (write(io, s); nothing)

@propagate_inbounds thisind(s::StringViewAndSub, i::Int) = _thisind_str(s, i)
@propagate_inbounds nextind(s::StringViewAndSub, i::Int) = _nextind_str(s, i)
isvalid(s::StringViewAndSub, i::Int) = checkbounds(Bool, s, i) && thisind(s, i) == i

# each string type must implement its own reverse because it is generally
# encoding-dependent
function reverse(s::StringViewAndSub)::String
    # Read characters forwards from `s` and write backwards to `out`
    out = _string_n(sizeof(s))
    offs = sizeof(s) + 1
    for c in s
        offs -= ncodeunits(c)
        __unsafe_string!(out, c, offs)
    end
    return out
end

# This is different from the String implementation, because when r is empty,
# we cannot just return the constant "".
@inline function Base.getindex(s::StringView, r::UnitRange{Int})
    cu = codeunits(s)
    isempty(r) && return StringView(cu[1:0])
    i, j = first(r), last(r)
    @boundscheck begin
        checkbounds(cu, r)
        @inbounds isvalid(s, i) || Base.string_index_err(s, i)
        @inbounds isvalid(s, j) || Base.string_index_err(s, j)
    end
    j = nextind(s, j) - 1
    return StringView(s.data[i:j])
end

### UTIL
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
    off = s isa String ? 0 : s.offset
    par = s isa String ? s : s.string
    @inbounds @inline SubString{String}(par, off, len, Val{:noshift}())
end

function replace(io::IO, s::DenseStringViewAndSub, pat_f::Pair...; count=typemax(Int))
    _replace_(io, s, pat_f, Int(count))
end

### SEARCH
# TODO: Have not processed functions below this line yet

nothing_sentinel(x) = iszero(x) ? nothing : x

function first_utf8_byte(x::Char)::UInt8
    u = reinterpret(UInt32, x)
    (u >> 24) % UInt8
end

function Base.findnext(pred::Base.Fix2{<:Union{typeof(isequal),typeof(==)},<:AbstractChar},
                  s::StringViewAndSub, i::Integer)
    if i < 1 || i > sizeof(s)
        i == sizeof(s) + 1 && return nothing
        throw(BoundsError(s, i))
    end
    @inbounds isvalid(s, i) || Base.string_index_err(s, i)
    c = Char(pred.x)::Char
    c ≤ '\x7f' && return nothing_sentinel(Base._search(s, c % UInt8, i))
    while true
        i = Base._search(s, first_utf8_byte(c), i)
        i == 0 && return nothing
        pred(s[i]) && return i
        i = nextind(s, i)
    end
end

function Base._search(a::StringViewAndSub, b::Union{Int8,UInt8}, i::Integer = 1)
    if i < 1
        throw(BoundsError(a, i))
    end
    n = sizeof(a)
    if i > n
        return i == n+1 ? 0 : throw(BoundsError(a, i))
    end
    if a isa DenseStringViewAndSub
        p = pointer(a)
        q = GC.@preserve a ccall(:memchr, Ptr{UInt8}, (Ptr{UInt8}, Int32, Csize_t), p+i-1, b, n-i+1)
        return q == C_NULL ? 0 : Int(q-p+1)
    else
        _i = Int(i)
        while true
            codeunit(a,_i) == b && return _i
            (_i += 1) > n && break
        end
        return 0
    end
end

function Base.findprev(pred::Base.Fix2{<:Union{typeof(isequal),typeof(==)},<:AbstractChar},
                  s::StringViewAndSub, i::Integer)
    c = pred.x
    c ≤ '\x7f' && return nothing_sentinel(Base._rsearch(s, c % UInt8, i))
    b = first_utf8_byte(c)
    while true
        i = Base._rsearch(s, b, i)
        i == 0 && return nothing
        pred(s[i]) && return i
        i = prevind(s, i)
    end
end

function Base._rsearch(a::StringViewAndSub, b::Union{Int8,UInt8}, i::Integer = sizeof(a))
    if i < 1
        return i == 0 ? 0 : throw(BoundsError(a, i))
    end
    n = sizeof(a)
    if i > n
        return i == n+1 ? 0 : throw(BoundsError(a, i))
    end
    if a isa DenseStringViewAndSub
        p = pointer(a)
        q = GC.@preserve a ccall(:memrchr, Ptr{UInt8}, (Ptr{UInt8}, Int32, Csize_t), p, b, i)
        return q == C_NULL ? 0 : Int(q-p+1)
    else
        _i = Int(i)
        while true
            codeunit(a,_i) == b && return _i
            (_i -= 1) < 1 && break
        end
        return 0
    end
end

# Split into two identical methods to avoid pirating Base's methods, leading to ~370 invalidations.
function Base._searchindex(s::StringViewAndSub, t::Union{StringViewAndSub,StringAndSub}, i::Integer)
    searchindex_internal(s, t, i)
end

function Base._searchindex(s::Union{StringViewAndSub,StringAndSub}, t::StringViewAndSub, i::Integer)
    searchindex_internal(s, t, i)
end

function searchindex_internal(s::Union{StringViewAndSub,StringAndSub}, t::Union{StringViewAndSub,StringAndSub}, i::Integer)
    # Check for fast case of a single byte
    lastindex(t) == 1 && return something(findnext(isequal(t[1]), s, i), 0)
    Base._searchindex(codeunits(s), codeunits(t), i)
end

function Base._rsearchindex(s::StringViewAndSub, t::Union{StringViewAndSub,StringAndSub}, i::Integer)
    rsearchindex_internal(s, t, i)
end

function Base._rsearchindex(s::Union{StringViewAndSub,StringAndSub}, t::StringViewAndSub, i::Integer)
    rsearchindex_internal(s, t, i)
end

function rsearchindex_internal(s::Union{StringViewAndSub,StringAndSub}, t::Union{StringViewAndSub,StringAndSub}, i::Integer)
    # Check for fast case of a single byte
    if lastindex(t) == 1
        return something(findprev(isequal(t[1]), s, i), 0)
    elseif lastindex(t) != 0
        j = i ≤ ncodeunits(s) ? nextind(s, i)-1 : i
        return Base._rsearchindex(codeunits(s), codeunits(t), j)
    elseif i > sizeof(s)
        return 0
    elseif i == 0
        return 1
    else
        return i
    end
end