# A simple abstract string with fixed indicies
struct CharString <: AbstractString
    chars::Vector{Char}
end
CharString(s::AbstractString) = CharString(Vector{Char}(s))

Base.ncodeunits(c::CharString) = length(c.chars)
Base.thisind(c::CharString, i::Int) = i
Base.isvalid(c::CharString, i::Int) = 1 <= i <= ncodeunits(c)
function Base.next(sp::Base.StringNext{CharString}, i::Int)
    (sp.data.chars[i], i+1), i+1
end
const CharStr = CharString

# An abstract string with efficient non-linear indicies
struct RopeString <: AbstractString
    strs::Vector{String}
end

function lin2trip(rs::RopeString, idx)
    linear = idx
    element = 1
    while ncodeunits(rs.strs[element]) < idx
        idx -= ncodeunits(rs.strs[element])
        element += 1
    end
    (linear, element, idx)
end

function Base.Next(rs::RopeString, idx::Int, itr::Base.EachStringIndex{RopeString})
    Base.Iterators.Rest(Base.StringNext(rs), lin2trip(rs, idx))
end
Base.ncodeunits(rs::RopeString) = sum(ncodeunits, rs.strs)
function Base.isvalid(rs::RopeString, idx::Int)
    (lin, el, eidx) = lin2trip(rs, idx)
    Base.isvalid(rs.strs[el], eidx)
end
function Base.thisind(rs::RopeString, idx::Int)
    (lin, el, eidx) = lin2trip(rs, idx)
    lin - (eidx - Base.thisind(rs.strs[el], eidx))
end
function Base.start(rs::Base.StringNext{RopeString})
    element = 1
    local ni
    while element <= length(rs.data.strs)
        el = rs.data.strs[element]
        ni = start(el)
        done(el, ni) || break
        element += 1
    end
    (1, element, ni)
end
Base.done(rs::Base.StringNext{RopeString}, (linear, element, ni)::Tuple{Int, Int, Int}) = element > length(rs.data.strs)
function Base.next(rs::Base.StringNext{RopeString}, (linear, element, i)::Tuple{Int, Int, Int})
    el = rs.data.strs[element]
    (c, nexti), _ = next(Base.StringNext(el), i)
    linear += nexti - i
    while done(el, nexti)
        element += 1
        element <= length(rs.data.strs) || break
        el = rs.data.strs[element]
        nexti = start(el)
    end
    (c, linear), (linear, element, nexti)
end
Base.start(rs::Base.StringNext{SubString{RopeString}}) = lin2trip(rs.data.string, rs.data.offset+firstindex(rs.data.string))
function Base.done(rs::Base.StringNext{SubString{RopeString}}, (linear, element, ni)::Tuple)
    (linear - rs.data.offset) > rs.data.ncodeunits && return true
    done(Base.StringNext(rs.data.string), (linear, element, ni))
end
function Base.next(rs::Base.StringNext{SubString{RopeString}}, state::Tuple)
    (c, linear), state = next(Base.StringNext(rs.data.string), state)
    (c, linear - rs.data.offset), state
end

# DecodeString
struct DecodeString{S<:AbstractString} <: AbstractString
    s::S
end
function Base.Next(s::T, idx, itr::Base.EachStringIndex{T}) where {T<:DecodeString}
    n = Base.Next(s.s, idx, eachindex(s.s))
    (n isa Base.Next) && return Base.Next(s)
    (n isa Base.Iterators.Rest) && return Base.Iterators.Rest(Base.Next(s), n.st)
    error("Result of `Next` must be `Next` or `Rest`, not $(typeof(n))")
end
Base.ncodeunits(s::DecodeString) = ncodeunits(s.s)
Base.start(sp::Base.StringNext{<:DecodeString}) = start(Base.StringNext(sp.data.s))
Base.done(sp::Base.StringNext{<:DecodeString}, state::Int) = done(Base.StringNext(sp.data.s), state::Int)
Base.done(sp::Base.StringNext{<:DecodeString}, state) = done(Base.StringNext(sp.data.s), state)
Base.SubString(d::DecodeString, start::Int, ncodeunits::Int) = DecodeString(SubString(d.s, start, ncodeunits))
function Base.next(sp::Base.StringNext{<:DecodeString}, state)
    sps = Base.StringNext(sp.data.s)
    (c, i), state = next(sps, state)
    if c != '\\' || done(sps, state)
        return (c, i), state
    end
    (c, i), state = next(sps, state)
    if c == 'x' || c == 'u' || c == 'U'
        n = k = 0
        m = c == 'x' ? 2 :
            c == 'u' ? 4 : 8
        while (k += 1) <= m && !done(sps, state)
            (c′, i′), state′ = next(sps, state)
            n = '0' <= c′ <= '9' ? n<<4 + (c′-'0') :
                'a' <= c′ <= 'f' ? n<<4 + (c′-'a'+10) :
                'A' <= c′ <= 'F' ? n<<4 + (c′-'A'+10) : break
            (i, state) = (i′, state′)
        end
        if k == 1 || n > 0x10ffff
            u = m == 4 ? 'u' : 'U'
            throw(ArgumentError("invalid $(m == 2 ? "hex (\\x)" :
                                "unicode (\\$u)") escape sequence"))
        end
        return (Char(n), i), state
    elseif '0' <= c <= '7'
        k = 1
        n = c-'0'
        while (k += 1) <= 3 && !done(sps, state)
            (c′, i′), state′ = next(sps, state)
            n = ('0' <= c <= '7') ? n<<3 + c-'0' : break
            (i, state) = (i′, state′)
        end
        if n > 255
            throw(ArgumentError("octal escape sequence out of range"))
        end
        return (Char(n), i), state
    else
        return (
            c == 'a' ? '\a' :
            c == 'b' ? '\b' :
            c == 't' ? '\t' :
            c == 'n' ? '\n' :
            c == 'v' ? '\v' :
            c == 'f' ? '\f' :
            c == 'r' ? '\r' :
            c == 'e' ? '\e' : c, i), state
    end
end
is_octal_char(c) = c in '0':'7'
is_hex_char(c) = (c in '0':'9') || (c in 'A':'F') || (c in 'a':'f')
function Base.thisind(s::DecodeString, ind::Int)
    si = thisind(s.s, ind)
    c = s.s[si]
    if c == '\\'
        return si
    end
    could_be_single_char = c in ('a', 'b', 'c', 'n', 'v', 'f', 'r', 'e')
    could_be_unicode = is_hex_char(c)
    could_be_octal = is_octal_char(c)
    nchars = 1
    pi = si
    while pi >= 1 && (could_be_single_char || could_be_unicode || could_be_octal)
        nchars += 1
        (c, pi) = Base.prev(s.s, pi)
        if c == '\\'
            return (could_be_single_char || could_be_octal) ? pi : si
        elseif c in ('u', 'U', 'x')
            (c, pi) = Base.prev(s.s, pi)
            m = c == 'U' ? 8 :
                c == 'u' ? 4 :
                           2
            if c == '\\'
                return nchars < m ? pi : si
            end
            return si
        else
            could_be_octal |= nchars <= 3 && is_octal_char(c)
            could_be_unicode |= nchars <= 8 && is_hex_char(c)
        end
        could_be_single_char = false
    end
    return si
end
Base.isvalid(s::DecodeString, ind::Int) = thisind(s, ind) == ind
