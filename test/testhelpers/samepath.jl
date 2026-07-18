"""
    samepath(path_a, path_b)

Check if two paths would refer to the same file or directory, even if the full path does
not exist yet. Each path is split into the longest prefix that exists on the filesystem
and the remaining suffix. The paths are considered equivalent when the existing prefixes
refer to the same file (`samefile`) and the suffixes are equal after `normpath`
normalization.

If both paths exist, this is equivalent to `samefile`.
"""
function samepath(a::AbstractString, b::AbstractString)
    a_pre, a_suf = _split_at_existing(a)
    b_pre, b_suf = _split_at_existing(b)
    return samefile(a_pre, b_pre) && normpath(a_suf) == normpath(b_suf)
end

function _split_at_existing(path::AbstractString)
    p = path
    while true
        ispath(p) && break
        parent = dirname(p)
        parent == p && break
        p = parent
    end
    p == path && return (path, "")
    # skip the separator between the existing prefix and the rest
    i = nextind(path, lastindex(p))
    if i <= lastindex(path) && Base.Filesystem.isseparator(path[i])
        i = nextind(path, i)
    end
    return (String(p), String(SubString(path, i)))
end
