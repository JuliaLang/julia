# This file is a part of Julia. License is MIT: http://julialang.org/license

## String padding functions ##

function lpad(s::AbstractString, n::Integer, p::AbstractString=" ")
    m = n - strwidth(s)
    if m <= 0; return s; end
    l = strwidth(p)
    if l==1
        return bytestring(p^m * s)
    end
    q = div(m,l)
    r = m - q*l
    bytestring(p^q*p[1:chr2ind(p,r)]*s)
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
    bytestring(s*p^q*p[1:chr2ind(p,r)])
end

lpad(s, n::Integer, p=" ") = lpad(string(s),n,string(p))
rpad(s, n::Integer, p=" ") = rpad(string(s),n,string(p))
cpad(s, n::Integer, p=" ") = rpad(lpad(s,div(n+strwidth(s),2),p),n,p)
