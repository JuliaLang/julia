# This file is a part of Julia. License is MIT: http://julialang.org/license

function lstrip(s::AbstractString, chars::Chars=_default_delims)
    i = start(s)
    while !done(s,i)
        c, j = next(s,i)
        if !(c in chars)
            return s[i:end]
        end
        i = j
    end
    ""
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
    ""
end

strip(s::AbstractString) = lstrip(rstrip(s))
strip(s::AbstractString, chars::Chars) = lstrip(rstrip(s, chars), chars)
