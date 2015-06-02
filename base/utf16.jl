# This file is a part of Julia. License is MIT: http://julialang.org/license

function length(s::UTF16String)
    d = s.data
    len = length(d) - 1
    len == 0 && return 0
    cnum = 0
    for i = 1:len
        @inbounds cnum += !is_surrogate_trail(d[i])
    end
    cnum
end

function endof(s::UTF16String)
    d = s.data
    i = length(d) - 1
    i == 0 && return i
    return is_surrogate_codepoint(d[i]) ? i-1 : i
end

get_supplementary(lead::Unsigned, trail::Unsigned) = (UInt32(lead-0xd7f7)<<10 + trail)

function next(s::UTF16String, i::Int)
    ch = s.data[i]
    !is_surrogate_codepoint(ch) && return (Char(ch), i+1)
    # check length, account for terminating \0
    i >= (length(s.data)-1) && utf_errfunc(UTF_ERR_MISSING_SURROGATE, i, UInt32(ch))
    !is_surrogate_lead(ch) && utf_errfunc(UTF_ERR_NOT_LEAD, i, ch)
    ct = s.data[i+1]
    !is_surrogate_trail(ct) && utf_errfunc(UTF_ERR_NOT_TRAIL, i, ch)
    Char(get_supplementary(ch, ct)), i+2
end

function reverseind(s::UTF16String, i::Integer)
    j = length(s.data) - i
    return is_surrogate_trail(s.data[j]) ? j-1 : j
end

lastidx(s::UTF16String) = length(s.data) - 1 # s.data includes NULL terminator

function reverse(s::UTF16String)
    d = s.data
    out = similar(d)
    out[end] = 0 # NULL termination
    n = length(d)
    @inbounds for i = 1:n-1
        ch = d[n-i]
        if is_surrogate_lead(ch)
            out[i],out[i-1] = out[i-1],ch
        else
            out[i] = ch
        end
    end
    UTF16String(out)
end

sizeof(s::UTF16String) = sizeof(s.data) - sizeof(UInt16)

function isvalid(::Type{UTF16String}, data::AbstractArray{UInt16})
    i = 1
    n = length(data) # this may include NULL termination; that's okay
    @inbounds while i < n # check for unpaired surrogates
        if is_surrogate_lead(data[i]) && is_surrogate_trail(data[i+1])
            i += 2
        elseif is_surrogate_codepoint(data[i])
            return false
        else
            i += 1
        end
    end
    return i > n || !is_surrogate_codepoint(data[i])
end
