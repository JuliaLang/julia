# This file is a part of Julia. License is MIT: http://julialang.org/license

export
    abspath,
    basename,
    dirname,
    expanduser,
    homedir,
    isabspath,
    isdirpath,
    joinpath,
    normpath,
    realpath,
    relpath,
    splitdir,
    splitdrive,
    splitext

if is_unix()
    const path_separator    = "/"
    const path_separator_re = r"/+"
    const path_absolute_re  = r"^/"
    const path_directory_re = r"(?:^|/)\.{0,2}$"
    const path_dir_splitter = r"^(.*?)(/+)([^/]*)$"
    const path_ext_splitter = r"^((?:.*/)?(?:\.|[^/\.])[^/]*?)(\.[^/\.]*|)$"

    splitdrive(path::String) = ("",path)
elseif is_windows()
    const path_separator    = "\\"
    const path_separator_re = r"[/\\]+"
    const path_absolute_re  = r"^(?:\w+:)?[/\\]"
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
"""
function homedir()
    path_max = 1024
    buf = Vector{UInt8}(path_max)
    sz = Ref{Csize_t}(path_max + 1)
    while true
        rc = ccall(:uv_os_homedir, Cint, (Ptr{UInt8}, Ptr{Csize_t}), buf, sz)
        if rc == 0
            resize!(buf, sz[])
            return String(buf)
        elseif rc == UV_ENOBUFS
            resize!(buf, sz[] - 1)
        else
            error("unable to retrieve home directory")
        end
    end
end


"""
    isabspath(path::AbstractString) -> Bool

Determines whether a path is absolute (begins at the root directory).

```jldoctest
julia> isabspath("/home")
true

julia> isabspath("home")
false
```
"""
isabspath(path::String) = ismatch(path_absolute_re, path)

"""
    isdirpath(path::AbstractString) -> Bool

Determines whether a path refers to a directory (for example, ends with a path separator).

```jldoctest
julia> isdirpath("/home")
false

julia> isdirpath("/home/")
true
```
"""
isdirpath(path::String) = ismatch(path_directory_re, splitdrive(path)[2])

"""
    splitdir(path::AbstractString) -> (AbstractString, AbstractString)

Split a path into a tuple of the directory name and file name.

```jldoctest
julia> splitdir("/home/myuser")
("/home", "myuser")
```
"""
function splitdir(path::String)
    a, b = splitdrive(path)
    m = match(path_dir_splitter,b)
    m === nothing && return (a,b)
    a = string(a, isempty(m.captures[1]) ? m.captures[2][1] : m.captures[1])
    a, String(m.captures[3])
end

"""
    dirname(path::AbstractString) -> AbstractString

Get the directory part of a path.

```jldoctest
julia> dirname("/home/myuser")
"/home"
```
"""
 dirname(path::AbstractString) = splitdir(path)[1]

"""
    basename(path::AbstractString) -> AbstractString

Get the file name part of a path.

 ```jldoctest
julia> basename("/home/myuser/example.jl")
"example.jl"
```
"""
basename(path::AbstractString) = splitdir(path)[2]

"""
    splitext(path::AbstractString) -> (AbstractString, AbstractString)

If the last component of a path contains a dot, split the path into everything before the
dot and everything including and after the dot. Otherwise, return a tuple of the argument
unmodified and the empty string.

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

joinpath(a::AbstractString) = a

"""
    joinpath(parts...) -> AbstractString

Join path components into a full path. If some argument is an absolute path, then prior
components are dropped.

```jldoctest
julia> joinpath("/home/myuser","example.jl")
"/home/myuser/example.jl"
```
"""
joinpath(a::AbstractString, b::AbstractString, c::AbstractString...) = joinpath(joinpath(a,b), c...)

function joinpath(a::String, b::String)
    isabspath(b) && return b
    A, a = splitdrive(a)
    B, b = splitdrive(b)
    !isempty(B) && A != B && throw(ArgumentError("drive mismatch: $A$a $B$b"))
    C = isempty(B) ? A : B
    isempty(a)                             ? string(C,b) :
    ismatch(path_separator_re, a[end:end]) ? string(C,a,b) :
                                             string(C,a,pathsep(a,b),b)
end
joinpath(a::AbstractString, b::AbstractString) = joinpath(String(a), String(b))

"""
    normpath(path::AbstractString) -> AbstractString

Normalize a path, removing "." and ".." entries.

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
            shift!(parts)
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
"""
abspath(a::String) = normpath(isabspath(a) ? a : joinpath(pwd(),a))
abspath(a::AbstractString, b::AbstractString...) = abspath(joinpath(a,b...))

if is_windows()
function realpath(path::AbstractString)
    p = cwstring(path)
    buf = zeros(UInt16, length(p))
    while true
        n = ccall((:GetFullPathNameW, "kernel32"), stdcall,
            UInt32, (Ptr{UInt16}, UInt32, Ptr{UInt16}, Ptr{Void}),
            p, length(buf), buf, C_NULL)
        systemerror(:realpath, n == 0)
        x = n < length(buf) # is the buffer big enough?
        resize!(buf, n) # shrink if x, grow if !x
        x && return transcode(String, buf)
    end
end

function longpath(path::AbstractString)
    p = cwstring(path)
    buf = zeros(UInt16, length(p))
    while true
        n = ccall((:GetLongPathNameW, "kernel32"), stdcall,
            UInt32, (Ptr{UInt16}, Ptr{UInt16}, UInt32),
            p, buf, length(buf))
        systemerror(:longpath, n == 0)
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
"""
realpath(path::AbstractString)


if is_windows()
expanduser(path::AbstractString) = path # on windows, ~ means "temporary file"
else
function expanduser(path::AbstractString)
    i = start(path)
    c, i = next(path,i)
    if c != '~' return path end
    if done(path,i) return homedir() end
    c, j = next(path,i)
    if c == '/' return homedir()*path[i:end] end
    throw(ArgumentError("~user tilde expansion not yet implemented"))
end
end


"""
    expanduser(path::AbstractString) -> AbstractString

On Unix systems, replace a tilde character at the start of a path with the current user's home directory.
"""
expanduser(path::AbstractString)


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
    pathpart = join(path_arr[i+1:findlast(x -> !isempty(x), path_arr)], path_separator)
    prefix_num = findlast(x -> !isempty(x), start_arr) - i - 1
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
