# This file is a part of Julia. License is MIT: http://julialang.org/license

function replace(str::ByteString, pattern, repl::Function, limit::Integer)
    n = 1
    e = endof(str)
    i = a = start(str)
    r = search(str,pattern,i)
    j, k = first(r), last(r)
    out = IOBuffer()
    while j != 0
        if i == a || i <= k
            write(out, SubString(str,i,prevind(str,j)))
            write(out, string(repl(SubString(str,j,k))))
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
replace(s::AbstractString, pat, f::Function, n::Integer) = replace(bytestring(s), pat, f, n)
replace(s::AbstractString, pat, r, n::Integer) = replace(s, pat, x->r, n)
replace(s::AbstractString, pat, r) = replace(s, pat, r, 0)
