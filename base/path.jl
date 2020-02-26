# This file is a part of Julia. License is MIT: https://julialang.org/license

export
    abspath,
    basename,
    dirname,
    expanduser,
    contractuser,
    homedir,
    isabspath,
    isdirpath,
    joinpath,
    normpath,
    realpath,
    relpath,
    splitdir,
    splitdrive,
    splitext,
    splitpath

if Sys.isunix()
    const path_separator    = "/"
    const path_separator_re = r"/+"
    const path_directory_re = r"(?:^|/)\.{0,2}$"
    const path_dir_splitter = r"^(.*?)(/+)([^/]*)$"
    const path_ext_splitter = r"^((?:.*/)?(?:\.|[^/\.])[^/]*?)(\.[^/\.]*|)$"

    splitdrive(path::String) = ("",path)
elseif Sys.iswindows()
    const path_separator    = "\\"
    const path_separator_re = r"[/\\]+"
    const path_absolute_re  = r"^(?:[A-Za-z]+:)?[/\\]"
    const path_directory_re = r"(?:^|[/\\])\.{0,2}$"
    const path_dir_splitter = r"^(.*?)([/\\]+)([^/\\]*)$"
    const path_ext_splitter = r"^((?:.*[/\\])?(?:\.|[^/\\\.])[^/\\]*?)(\.[^/\\\.]*|)$"

    function splitdrive(path::String)
        m = match(r"^([^\\]+:|\\\\[^\\]+\\[^\\]+|\\\\\?\\UNC\\[^\\]+\\[^\\]+|\\\\\?\\[^\\]+:|)(.*)$"s, path)
        String(m.captures[1]), String(m.captures[2])
    end
else
    error("path primitives for this OS need to be defined")
end


"""
    splitdrive(path::AbstractString) -> (AbstractString, AbstractString)

On Windows, split a path into the drive letter part and the path part. On Unix systems, the
first component is always the empty string.
"""
splitdrive(path::AbstractString)

"""
    homedir() -> String

Return the current user's home directory.

!!! note
    `homedir` determines the home directory via `libuv`'s `uv_os_homedir`. For details
    (for example on how to specify the home directory via environment variables), see the
    [`uv_os_homedir` documentation](http://docs.libuv.org/en/v1.x/misc.html#c.uv_os_homedir).
"""
function homedir()
    buf = Base.StringVector(AVG_PATH - 1) # space for null-terminator implied by StringVector
    sz = RefValue{Csize_t}(length(buf) + 1) # total buffer size including null
    while true
        rc = ccall(:uv_os_homedir, Cint, (Ptr{UInt8}, Ptr{Csize_t}), buf, sz)
        if rc == 0
            resize!(buf, sz[])
            return String(buf)
        elseif rc == Base.UV_ENOBUFS
            resize!(buf, sz[] - 1) # space for null-terminator implied by StringVector
        else
            uv_error(:homedir, rc)
        end
    end
end


if Sys.iswindows()
    isabspath(path::AbstractString) = occursin(path_absolute_re, path)
else
    isabspath(path::AbstractString) = startswith(path, '/')
end

"""
    isabspath(path::AbstractString) -> Bool

Determine whether a path is absolute (begins at the root directory).

# Examples
```jldoctest
julia> isabspath("/home")
true

julia> isabspath("home")
false
```
"""
isabspath(path::AbstractString)

"""
    isdirpath(path::AbstractString) -> Bool

Determine whether a path refers to a directory (for example, ends with a path separator).

# Examples
```jldoctest
julia> isdirpath("/home")
false

julia> isdirpath("/home/")
true
```
"""
isdirpath(path::String) = occursin(path_directory_re, splitdrive(path)[2])

"""
    splitdir(path::AbstractString) -> (AbstractString, AbstractString)

Split a path into a tuple of the directory name and file name.

# Examples
```jldoctest
julia> splitdir("/home/myuser")
("/home", "myuser")
```
"""
function splitdir(path::String)
    a, b = splitdrive(path)
    _splitdir_nodrive(a,b)
end

# Common splitdir functionality without splitdrive, needed for splitpath.
_splitdir_nodrive(path::String) = _splitdir_nodrive("", path)
function _splitdir_nodrive(a::String, b::String)
    m = match(path_dir_splitter,b)
    m === nothing && return (a,b)
    a = string(a, isempty(m.captures[1]) ? m.captures[2][1] : m.captures[1])
    a, String(m.captures[3])
end

"""
    dirname(path::AbstractString) -> AbstractString

Get the directory part of a path. Trailing characters ('/' or '\\') in the path are
counted as part of the path.

# Examples
```jldoctest
julia> dirname("/home/myuser")
"/home"

julia> dirname("/home/myuser/")
"/home/myuser"
```

See also: [`basename`](@ref)
"""
 dirname(path::AbstractString) = splitdir(path)[1]

"""
    basename(path::AbstractString) -> AbstractString

Get the file name part of a path.

# Examples
```jldoctest
julia> basename("/home/myuser/example.jl")
"example.jl"
```

See also: [`dirname`](@ref)
"""
basename(path::AbstractString) = splitdir(path)[2]

"""
    splitext(path::AbstractString) -> (AbstractString, AbstractString)

If the last component of a path contains a dot, split the path into everything before the
dot and everything including and after the dot. Otherwise, return a tuple of the argument
unmodified and the empty string.

# Examples
```jldoctest
julia> splitext("/home/myuser/example.jl")
("/home/myuser/example", ".jl")

julia> splitext("/home/myuser/example")
("/home/myuser/example", "")
```
"""
function splitext(path::String)
    a, b = splitdrive(path)
    m = match(path_ext_splitter, b)
    m === nothing && return (path,"")
    a*m.captures[1], String(m.captures[2])
end

# NOTE: deprecated in 1.4
pathsep() = path_separator

"""
    splitpath(path::AbstractString) -> Vector{String}

Split a file path into all its path components. This is the opposite of
`joinpath`. Returns an array of substrings, one for each directory or file in
the path, including the root directory if present.

!!! compat "Julia 1.1"
    This function requires at least Julia 1.1.

# Examples
```jldoctest
julia> splitpath("/home/myuser/example.jl")
4-element Array{String,1}:
 "/"
 "home"
 "myuser"
 "example.jl"
```
"""
splitpath(p::AbstractString) = splitpath(String(p))

function splitpath(p::String)
    drive, p = splitdrive(p)
    out = String[]
    isempty(p) && (pushfirst!(out,p))  # "" means the current directory.
    while !isempty(p)
        dir, base = _splitdir_nodrive(p)
        dir == p && (pushfirst!(out, dir); break)  # Reached root node.
        if !isempty(base)  # Skip trailing '/' in basename
            pushfirst!(out, base)
        end
        p = dir
    end
    if !isempty(drive)  # Tack the drive back on to the first element.
        out[1] = drive*out[1]  # Note that length(out) is always >= 1.
    end
    return out
end

joinpath(path::AbstractString)::String = path

if Sys.iswindows()

function joinpath(path::AbstractString, paths::AbstractString...)::String
    result_drive, result_path = splitdrive(path)

    local p_drive, p_path
    for p in paths
        p_drive, p_path = splitdrive(p)

        if startswith(p_path, ('\\', '/'))
            # second path is absolute
            if !isempty(p_drive) || !isempty(result_drive)
                result_drive = p_drive
            end
            result_path = p_path
            continue
        elseif !isempty(p_drive) && p_drive != result_drive
            if lowercase(p_drive) != lowercase(result_drive)
                # different drives, ignore the first path entirely
                result_drive = p_drive
                result_path = p_path
                continue
            end
        end

        # second path is relative to the first
        if !isempty(result_path) && result_path[end] ∉ ('\\', '/')
            result_path *= "\\"
        end

        result_path = result_path * p_path
    end

    # add separator between UNC and non-absolute path
    if !isempty(p_path) && result_path[1] ∉ ('\\', '/') && !isempty(result_drive) && result_drive[end] != ':'
        return result_drive * "\\" * result_path
    end

    return result_drive * result_path
end

else

function joinpath(path::AbstractString, paths::AbstractString...)::String
    for p in paths
        if isabspath(p)
            path = p
        elseif isempty(path) || path[end] == '/'
            path *= p
        else
            path *= "/" * p
        end
    end
    return path
end

end # os-test

"""
    joinpath(parts::AbstractString...) -> String

Join path components into a full path. If some argument is an absolute path or
(on Windows) has a drive specification that doesn't match the drive computed for
the join of the preceding paths, then prior components are dropped.

Note on Windows since there is a current directory for each drive, `joinpath("c:", "foo")`
represents a path relative to the current directory on drive "c:" so this is equal to "c:foo",
not "c:\\foo". Furthermore, `joinpath` treats this as a non-absolute path and ignores the drive
letter casing, hence `joinpath("C:\\A","c:b") = "C:\\A\\b"`.

# Examples
```jldoctest
julia> joinpath("/home/myuser", "example.jl")
"/home/myuser/example.jl"
```
"""
joinpath

"""
    normpath(path::AbstractString) -> String

Normalize a path, removing "." and ".." entries.

# Examples
```jldoctest
julia> normpath("/home/myuser/../example.jl")
"/home/example.jl"
```
"""
function normpath(path::String)
    isabs = isabspath(path)
    isdir = isdirpath(path)
    drive, path = splitdrive(path)
    parts = split(path, path_separator_re)
    filter!(x->!isempty(x) && x!=".", parts)
    while true
        clean = true
        for j = 1:length(parts)-1
            if parts[j] != ".." && parts[j+1] == ".."
                deleteat!(parts, j:j+1)
                clean = false
                break
            end
        end
        clean && break
    end
    if isabs
        while !isempty(parts) && parts[1] == ".."
            popfirst!(parts)
        end
    elseif isempty(parts)
        push!(parts, ".")
    end
    path = join(parts, path_separator)
    if isabs
        path = path_separator*path
    end
    if isdir && !isdirpath(path)
        path *= path_separator
    end
    string(drive,path)
end

"""
    normpath(path::AbstractString, paths::AbstractString...) -> String

Convert a set of paths to a normalized path by joining them together and removing
"." and ".." entries. Equivalent to `normpath(joinpath(path, paths...))`.
"""
normpath(a::AbstractString, b::AbstractString...) = normpath(joinpath(a,b...))

"""
    abspath(path::AbstractString) -> String

Convert a path to an absolute path by adding the current directory if necessary.
Also normalizes the path as in [`normpath`](@ref).
"""
abspath(a::String) = normpath(isabspath(a) ? a : joinpath(pwd(),a))

"""
    abspath(path::AbstractString, paths::AbstractString...) -> String

Convert a set of paths to an absolute path by joining them together and adding the
current directory if necessary. Equivalent to `abspath(joinpath(path, paths...))`.
"""
abspath(a::AbstractString, b::AbstractString...) = abspath(joinpath(a,b...))

if Sys.iswindows()

function longpath(path::AbstractString)
    p = cwstring(path)
    buf = zeros(UInt16, length(p))
    while true
        n = ccall((:GetLongPathNameW, "kernel32"), stdcall,
            UInt32, (Ptr{UInt16}, Ptr{UInt16}, UInt32),
            p, buf, length(buf))
        windowserror(:longpath, n == 0)
        x = n < length(buf) # is the buffer big enough?
        resize!(buf, n) # shrink if x, grow if !x
        x && return transcode(String, buf)
    end
end

end # os-test


"""
    realpath(path::AbstractString) -> String

Canonicalize a path by expanding symbolic links and removing "." and ".." entries.
On case-insensitive case-preserving filesystems (typically Mac and Windows), the
filesystem's stored case for the path is returned.

(This function throws an exception if `path` does not exist in the filesystem.)
"""
function realpath(path::AbstractString)
    req = Libc.malloc(_sizeof_uv_fs)
    try
        ret = ccall(:uv_fs_realpath, Cint,
                    (Ptr{Cvoid}, Ptr{Cvoid}, Cstring, Ptr{Cvoid}),
                    C_NULL, req, path, C_NULL)
        if ret < 0
            ccall(:uv_fs_req_cleanup, Cvoid, (Ptr{Cvoid},), req)
            uv_error("realpath", ret)
        end
        path = unsafe_string(ccall(:jl_uv_fs_t_ptr, Cstring, (Ptr{Cvoid},), req))
        ccall(:uv_fs_req_cleanup, Cvoid, (Ptr{Cvoid},), req)
        return path
    finally
        Libc.free(req)
    end
end

if Sys.iswindows()
# on windows, ~ means "temporary file"
expanduser(path::AbstractString) = path
contractuser(path::AbstractString) = path
else
function expanduser(path::AbstractString)
    y = iterate(path)
    y === nothing && return path
    c, i = y
    c != '~' && return path
    y = iterate(path, i)
    y === nothing && return homedir()
    y[1] == '/' && return homedir() * path[i:end]
    throw(ArgumentError("~user tilde expansion not yet implemented"))
end
function contractuser(path::AbstractString)
    home = homedir()
    if path == home
        return "~"
    elseif startswith(path, home)
        return joinpath("~", relpath(path, home))
    else
        return path
    end
end
end


"""
    expanduser(path::AbstractString) -> AbstractString

On Unix systems, replace a tilde character at the start of a path with the current user's home directory.
"""
expanduser(path::AbstractString)

"""
    contractuser(path::AbstractString) -> AbstractString

On Unix systems, if the path starts with `homedir()`, replace it with a tilde character.
"""
contractuser(path::AbstractString)


"""
    relpath(path::AbstractString, startpath::AbstractString = ".") -> AbstractString

Return a relative filepath to `path` either from the current directory or from an optional
start directory. This is a path computation: the filesystem is not accessed to confirm the
existence or nature of `path` or `startpath`.
"""
function relpath(path::String, startpath::String = ".")
    isempty(path) && throw(ArgumentError("`path` must be specified"))
    isempty(startpath) && throw(ArgumentError("`startpath` must be specified"))
    curdir = "."
    pardir = ".."
    path == startpath && return curdir
    path_arr  = split(abspath(path),      path_separator_re)
    start_arr = split(abspath(startpath), path_separator_re)
    i = 0
    while i < min(length(path_arr), length(start_arr))
        i += 1
        if path_arr[i] != start_arr[i]
            i -= 1
            break
        end
    end
    pathpart = join(path_arr[i+1:something(findlast(x -> !isempty(x), path_arr), 0)], path_separator)
    prefix_num = something(findlast(x -> !isempty(x), start_arr), 0) - i - 1
    if prefix_num >= 0
        prefix = pardir * path_separator
        relpath_ = isempty(pathpart)     ?
            (prefix^prefix_num) * pardir :
            (prefix^prefix_num) * pardir * path_separator * pathpart
    else
        relpath_ = pathpart
    end
    return isempty(relpath_) ? curdir :  relpath_
end
relpath(path::AbstractString, startpath::AbstractString) =
    relpath(String(path), String(startpath))

for f in (:isdirpath, :splitdir, :splitdrive, :splitext, :normpath, :abspath)
    @eval $f(path::AbstractString) = $f(String(path))
end
