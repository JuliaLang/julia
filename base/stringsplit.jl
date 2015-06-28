# This file is a part of Julia. License is MIT: http://julialang.org/license

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
