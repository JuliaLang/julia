# This file is a part of Julia. License is MIT: http://julialang.org/license

# starts with and ends with predicates

function startswith(a::AbstractString, b::AbstractString)
    i = start(a)
    j = start(b)
    while !done(a,i) && !done(b,i)
        c, i = next(a,i)
        d, j = next(b,j)
        if c != d return false end
    end
    done(b,i)
end
startswith(str::AbstractString, chars::Chars) = !isempty(str) && first(str) in chars

function endswith(a::AbstractString, b::AbstractString)
    i = endof(a)
    j = endof(b)
    a1 = start(a)
    b1 = start(b)
    while a1 <= i && b1 <= j
        c = a[i]
        d = b[j]
        if c != d return false end
        i = prevind(a,i)
        j = prevind(b,j)
    end
    j < b1
end
endswith(str::AbstractString, chars::Chars) = !isempty(str) && last(str) in chars

startswith(a::String, b::String) = startswith(a.data, b.data)
startswith(a::Vector{UInt8}, b::Vector{UInt8}) =
    (length(a) >= length(b) && ccall(:memcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}, UInt), a, b, length(b)) == 0)

# TODO: fast endswith

chop(s::AbstractString) = s[1:end-1]

function chomp(s::AbstractString)
    i = endof(s)
    if (i < 1 || s[i] != '\n') return s end
    j = prevind(s,i)
    if (j < 1 || s[j] != '\r') return s[1:i-1] end
    return s[1:j-1]
end
chomp(s::String) =
    (endof(s) < 1 || s.data[end]   != 0x0a) ? s :
    (endof(s) < 2 || s.data[end-1] != 0x0d) ? s[1:end-1] : s[1:end-2]

# NOTE: use with caution -- breaks the immutable string convention!
function chomp!(s::String)
    if !isempty(s) && s.data[end] == 0x0a
        n = (endof(s) < 2 || s.data[end-1] != 0x0d) ? 1 : 2
        ccall(:jl_array_del_end, Void, (Any, UInt), s.data, n)
    end
    return s
end
chomp!(s::AbstractString) = chomp(s) # copying fallback for other string types

const _default_delims = [' ','\t','\n','\v','\f','\r']

function lstrip(s::AbstractString, chars::Chars=_default_delims)
    i = start(s)
    while !done(s,i)
        c, j = next(s,i)
        if !(c in chars)
            return s[i:end]
        end
        i = j
    end
    s[end+1:end]
end

function rstrip(s::AbstractString, chars::Chars=_default_delims)
    r = RevString(s)
    i = start(r)
    while !done(r,i)
        c, j = next(r,i)
        if !(c in chars)
            return s[1:end-i+1]
        end
        i = j
    end
    s[1:0]
end

strip(s::AbstractString) = lstrip(rstrip(s))
strip(s::AbstractString, chars::Chars) = lstrip(rstrip(s, chars), chars)

## string padding functions ##

function lpad(s::AbstractString, n::Integer, p::AbstractString=" ")
    m = n - strwidth(s)
    if m <= 0; return s; end
    l = strwidth(p)
    if l==1
        return bytestring(p^m * s)
    end
    q = div(m,l)
    r = m - q*l
    i = r != 0 ? chr2ind(p, r) : -1
    bytestring(p^q*p[1:i]*s)
end

function rpad(s::AbstractString, n::Integer, p::AbstractString=" ")
    m = n - strwidth(s)
    if m <= 0; return s; end
    l = strwidth(p)
    if l==1
        return bytestring(s * p^m)
    end
    q = div(m,l)
    r = m - q*l
    i = r != 0 ? chr2ind(p, r) : -1
    bytestring(s*p^q*p[1:i])
end

lpad(s, n::Integer, p=" ") = lpad(string(s),n,string(p))
rpad(s, n::Integer, p=" ") = rpad(string(s),n,string(p))
cpad(s, n::Integer, p=" ") = rpad(lpad(s,div(n+strwidth(s),2),p),n,p)

# splitter can be a Char, Vector{Char}, AbstractString, Regex, ...
# any splitter that provides search(s::AbstractString, splitter)
split{T<:SubString}(str::T, splitter; limit::Integer=0, keep::Bool=true) = _split(str, splitter, limit, keep, T[])
split{T<:AbstractString}(str::T, splitter; limit::Integer=0, keep::Bool=true) = _split(str, splitter, limit, keep, SubString{T}[])
function _split{T<:AbstractString,U<:Array}(str::T, splitter, limit::Integer, keep_empty::Bool, strs::U)
    i = start(str)
    n = endof(str)
    r = search(str,splitter,i)
    j, k = first(r), nextind(str,last(r))
    while 0 < j <= n && length(strs) != limit-1
        if i < k
            if keep_empty || i < j
                push!(strs, SubString(str,i,prevind(str,j)))
            end
            i = k
        end
        if k <= j; k = nextind(str,j) end
        r = search(str,splitter,k)
        j, k = first(r), nextind(str,last(r))
    end
    if keep_empty || !done(str,i)
        push!(strs, SubString(str,i))
    end
    return strs
end

# a bit oddball, but standard behavior in Perl, Ruby & Python:
split(str::AbstractString) = split(str, _default_delims; limit=0, keep=false)

rsplit{T<:SubString}(str::T, splitter; limit::Integer=0, keep::Bool=true) = _rsplit(str, splitter, limit, keep, T[])
rsplit{T<:AbstractString}(str::T, splitter   ; limit::Integer=0, keep::Bool=true) = _rsplit(str, splitter, limit, keep, SubString{T}[])
function _rsplit{T<:AbstractString,U<:Array}(str::T, splitter, limit::Integer, keep_empty::Bool, strs::U)
    i = start(str)
    n = endof(str)
    r = rsearch(str,splitter)
    j = first(r)-1
    k = last(r)
    while((0 <= j < n) && (length(strs) != limit-1))
        if i <= k
            (keep_empty || (k < n)) && unshift!(strs, SubString(str,k+1,n))
            n = j
        end
        (k <= j) && (j = prevind(str,j))
        r = rsearch(str,splitter,j)
        j = first(r)-1
        k = last(r)
    end
    (keep_empty || (n > 0)) && unshift!(strs, SubString(str,1,n))
    return strs
end
#rsplit(str::AbstractString) = rsplit(str, _default_delims, 0, false)

_replace(io, repl, str, r, pattern) = print(io, repl)
_replace(io, repl::Function, str, r, pattern) =
    print(io, repl(SubString(str, first(r), last(r))))

function replace(str::String, pattern, repl, limit::Integer)
    n = 1
    e = endof(str)
    i = a = start(str)
    r = search(str,pattern,i)
    j, k = first(r), last(r)
    out = IOBuffer()
    ensureroom(out, floor(Int, 1.2sizeof(str)))
    while j != 0
        if i == a || i <= k
            write_sub(out, str.data, i, j-i)
            _replace(out, repl, str, r, pattern)
        end
        if k<j
            i = j
            k = nextind(str, j)
        else
            i = k = nextind(str, k)
        end
        if j > e
            break
        end
        r = search(str,pattern,k)
        j, k = first(r), last(r)
        n == limit && break
        n += 1
    end
    write(out, SubString(str,i))
    takebuf_string(out)
end
replace(s::AbstractString, pat, f, n::Integer) = replace(bytestring(s), pat, f, n)
replace(s::AbstractString, pat, r) = replace(s, pat, r, 0)

# hex <-> bytes conversion

function hex2bytes(s::AbstractString)
    a = zeros(UInt8, div(endof(s), 2))
    i, j = start(s), 0
    while !done(s, i)
        c, i = next(s, i)
        n = '0' <= c <= '9' ? c - '0' :
            'a' <= c <= 'f' ? c - 'a' + 10 :
            'A' <= c <= 'F' ? c - 'A' + 10 :
            throw(ArgumentError("not a hexadecimal string: $(repr(s))"))
        done(s, i) &&
            throw(ArgumentError("string length must be even: length($(repr(s))) == $(length(s))"))
        c, i = next(s, i)
        n = '0' <= c <= '9' ? n << 4 + c - '0' :
            'a' <= c <= 'f' ? n << 4 + c - 'a' + 10 :
            'A' <= c <= 'F' ? n << 4 + c - 'A' + 10 :
            throw(ArgumentError("not a hexadecimal string: $(repr(s))"))
        a[j += 1] = n
    end
    resize!(a, j)
    return a
end

function bytes2hex(a::AbstractArray{UInt8})
    b = Vector{UInt8}(2*length(a))
    i = 0
    for x in a
        b[i += 1] = hex_chars[1 + x >> 4]
        b[i += 1] = hex_chars[1 + x & 0xf]
    end
    return String(b)
end
