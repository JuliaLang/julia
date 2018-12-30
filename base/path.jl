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
        m = match(r"^([^\\]+:|\\\\[^\\]+\\[^\\]+|\\\\\?\\UNC\\[^\\]+\\[^\\]+|\\\\\?\\[^\\]+:|)(.*)$", path)
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
    homedir() -> AbstractString

Return the current user's home directory.

!!! note
    `homedir` determines the home directory via `libuv`'s `uv_os_homedir`. For details
    (for example on how to specify the home directory via environment variables), see the
    [`uv_os_homedir` documentation](http://docs.libuv.org/en/v1.x/misc.html#c.uv_os_homedir).
"""
function homedir end

if Base.DISABLE_LIBUV
function homedir()
    haskey(ENV, "HOME") ? ENV["HOME"] : error("unable to retrieve home directory")
end    
else
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
end

if Sys.iswindows()
    isabspath(path::String) = occursin(path_absolute_re, path)
else
    isabspath(path::String) = startswith(path, '/')
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

function pathsep(paths::AbstractString...)
    for path in paths
        m = match(path_separator_re, String(path))
        m !== nothing && return m.match[1:1]
    end
    return path_separator
end

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

joinpath(a::AbstractString) = a

"""
    joinpath(parts...) -> AbstractString

Join path components into a full path. If some argument is an absolute path or
(on Windows) has a drive specification that doesn't match the drive computed for
the join of the preceding paths, then prior components are dropped.

# Examples
```jldoctest
julia> joinpath("/home/myuser", "example.jl")
"/home/myuser/example.jl"
```
"""
joinpath(a::AbstractString, b::AbstractString, c::AbstractString...) = joinpath(joinpath(a,b), c...)

function joinpath(a::String, b::String)
    isabspath(b) && return b
    A, a = splitdrive(a)
    B, b = splitdrive(b)
    !isempty(B) && A != B && return string(B,b)
    C = isempty(B) ? A : B
    isempty(a)                              ? string(C,b) :
    occursin(path_separator_re, a[end:end]) ? string(C,a,b) :
                                              string(C,a,pathsep(a,b),b)
end
joinpath(a::AbstractString, b::AbstractString) = joinpath(String(a), String(b))

"""
    normpath(path::AbstractString) -> AbstractString

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
normpath(a::AbstractString, b::AbstractString...) = normpath(joinpath(a,b...))

"""
    abspath(path::AbstractString) -> AbstractString

Convert a path to an absolute path by adding the current directory if necessary.
Also normalizes the path as in [`normpath`](@ref).
"""
abspath(a::String) = normpath(isabspath(a) ? a : joinpath(pwd(),a))

"""
    abspath(path::AbstractString, paths::AbstractString...) -> AbstractString

Convert a set of paths to an absolute path by joining them together and adding the
current directory if necessary. Equivalent to `abspath(joinpath(path, paths...))`.
"""
abspath(a::AbstractString, b::AbstractString...) = abspath(joinpath(a,b...))

if Sys.iswindows()

function realpath(path::AbstractString)
    h = ccall(:CreateFileW, stdcall, Int, (Cwstring, UInt32, UInt32, Ptr{Cvoid}, UInt32, UInt32, Int),
                path, 0, 0x03, C_NULL, 3, 0x02000000, 0)
    windowserror(:realpath, h == -1)
    try
        buf = Vector{UInt16}(undef, 256)
        oldlen = len = length(buf)
        while len >= oldlen
            len = ccall(:GetFinalPathNameByHandleW, stdcall, UInt32, (Int, Ptr{UInt16}, UInt32, UInt32),
                            h, buf, (oldlen=len)-1, 0x0)
            windowserror(:realpath, iszero(len))
            resize!(buf, len) # strips NUL terminator on last call
        end
        if 4 < len < 264 && 0x005c == buf[1] == buf[2] == buf[4] && 0x003f == buf[3]
            Base._deletebeg!(buf, 4) # omit \\?\ prefix for paths < MAXPATH in length
        end
        return transcode(String, buf)
    finally
        windowserror(:realpath, iszero(ccall(:CloseHandle, stdcall, Cint, (Int,), h)))
    end
end

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

else # !windows
function realpath(path::AbstractString)
    p = ccall(:realpath, Ptr{UInt8}, (Cstring, Ptr{UInt8}), path, C_NULL)
    systemerror(:realpath, p == C_NULL)
    str = unsafe_string(p)
    Libc.free(p)
    return str
end
end # os-test


"""
    realpath(path::AbstractString) -> AbstractString

Canonicalize a path by expanding symbolic links and removing "." and ".." entries.
On case-insensitive case-preserving filesystems (typically Mac and Windows), the
filesystem's stored case for the path is returned.

(This function throws an exception if `path` does not exist in the filesystem.)
"""
realpath(path::AbstractString)


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

for f in (:isabspath, :isdirpath, :splitdir, :splitdrive, :splitext, :normpath, :abspath)
    @eval $f(path::AbstractString) = $f(String(path))
end
