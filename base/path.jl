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
    const path_separator_re = r"/+"sa
    const path_directory_re = r"(?:^|/)\.{0,2}$"sa
    const path_dir_splitter = r"^(.*?)(/+)([^/]*)$"sa
    const path_ext_splitter = r"^((?:.*/)?(?:\.|[^/\.])[^/]*?)(\.[^/\.]*|)$"sa

    splitdrive(path::String) = ("",path)
elseif Sys.iswindows()
    const path_separator    = "\\"
    const path_separator_re = r"[/\\]+"sa
    const path_absolute_re  = r"^(?:[A-Za-z]+:)?[/\\]"sa
    const path_directory_re = r"(?:^|[/\\])\.{0,2}$"sa
    const path_dir_splitter = r"^(.*?)([/\\]+)([^/\\]*)$"sa
    const path_ext_splitter = r"^((?:.*[/\\])?(?:\.|[^/\\\.])[^/\\]*?)(\.[^/\\\.]*|)$"sa

    const splitdrive_re = let
        # Slash in either direction.
        S = raw"[\\/]"
        # Not a slash in either direction.
        N = raw"[^\\/]"
        # Drive letter, e.g. `C:`
        drive = "$(N)+:"
        # UNC path, e.g. `\\server\share`
        unc = "$(S)$(S)$(N)+$(S)$(N)+"
        # Long drive letter, e.g. `\\?\C:`
        long_drive = "$(S)$(S)\\?$(S)$(drive)"
        # Long UNC path, e.g. `\\?\UNC\server\share`
        long_unc = "$(S)$(S)\\?$(S)UNC$(S)$(N)+$(S)$(N)+"
        # Need to match the long patterns first so they get priority.
        Regex("^($long_unc|$long_drive|$unc|$drive|)(.*)\$", "sa")
    end

    function splitdrive(path::String)
        m = match(splitdrive_re, path)::AbstractMatch
        String(something(m.captures[1])), String(something(m.captures[2]))
    end
else
    error("path primitives for this OS need to be defined")
end


"""
    splitdrive(path::AbstractString) -> (drive::AbstractString, path::AbstractString)

On Windows, split a path into the drive letter part and the path part. On Unix systems, the
first component is always the empty string.
"""
splitdrive(path::AbstractString)

"""
    homedir()::String

Return the current user's home directory.

!!! note
    `homedir` determines the home directory via `libuv`'s `uv_os_homedir`. For details
    (for example on how to specify the home directory via environment variables), see the
    [`uv_os_homedir` documentation](http://docs.libuv.org/en/v1.x/misc.html#c.uv_os_homedir).

See also [`Sys.username`](@ref).
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
            uv_error("homedir()", rc)
        end
    end
end


if Sys.iswindows()
    isabspath(path::AbstractString) = occursin(path_absolute_re, path)
else
    isabspath(path::AbstractString) = startswith(path, '/')
end

"""
    isabspath(path::AbstractString)::Bool

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
    isdirpath(path::AbstractString)::Bool

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
    splitdir(path::AbstractString) -> (dir::AbstractString, file::AbstractString)

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
    cs = m.captures
    getcapture(cs, i) = cs[i]::AbstractString
    c1, c2, c3 = getcapture(cs, 1), getcapture(cs, 2), getcapture(cs, 3)
    a = string(a, isempty(c1) ? c2[1] : c1)
    a, String(c3)
end

"""
    dirname(path::AbstractString)::String

Get the directory part of a path. Trailing characters ('/' or '\\') in the path are
counted as part of the path.

# Examples
```jldoctest
julia> dirname("/home/myuser")
"/home"

julia> dirname("/home/myuser/")
"/home/myuser"
```

See also [`basename`](@ref).
"""
dirname(path::AbstractString) = splitdir(path)[1]

"""
    basename(path::AbstractString)::String

Get the file name part of a path.

!!! note
    This function differs slightly from the Unix `basename` program, where trailing slashes are ignored,
    i.e. `\$ basename /foo/bar/` returns `bar`, whereas `basename` in Julia returns an empty string `""`.

# Examples
```jldoctest
julia> basename("/home/myuser/example.jl")
"example.jl"

julia> basename("/home/myuser/")
""
```

See also [`dirname`](@ref).
"""
basename(path::AbstractString) = splitdir(path)[2]

"""
    splitext(path::AbstractString) -> (path_without_extension::String, extension::String)

If the last component of a path contains one or more dots, split the path into everything before the
last dot and everything including and after the dot. Otherwise, return a tuple of the argument
unmodified and the empty string. "splitext" is short for "split extension".

# Examples
```jldoctest
julia> splitext("/home/myuser/example.jl")
("/home/myuser/example", ".jl")

julia> splitext("/home/myuser/example.tar.gz")
("/home/myuser/example.tar", ".gz")

julia> splitext("/home/my.user/example")
("/home/my.user/example", "")
```
"""
function splitext(path::String)
    a, b = splitdrive(path)
    m = match(path_ext_splitter, b)
    m === nothing && return (path,"")
    (a*something(m.captures[1])), String(something(m.captures[2]))
end

# NOTE: deprecated in 1.4
pathsep() = path_separator

"""
    splitpath(path::AbstractString)::Vector{String}

Split a file path into all its path components. This is the opposite of
`joinpath`. Returns an array of substrings, one for each directory or file in
the path, including the root directory if present.

!!! compat "Julia 1.1"
    This function requires at least Julia 1.1.

# Examples
```jldoctest
julia> splitpath("/home/myuser/example.jl")
4-element Vector{String}:
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

if Sys.iswindows()

function joinpath(paths::Union{Tuple, AbstractVector})::String
    assertstring(x) = x isa AbstractString || throw(ArgumentError("path component is not a string: $(repr(x))"))

    isempty(paths) && throw(ArgumentError("collection of path components must be non-empty"))
    assertstring(paths[1])
    result_drive, result_path = splitdrive(paths[1])

    p_path = ""
    for i in firstindex(paths)+1:lastindex(paths)
        assertstring(paths[i])
        p_drive, p_path = splitdrive(paths[i])

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

function joinpath(paths::Union{Tuple, AbstractVector})::String
    assertstring(x) = x isa AbstractString || throw(ArgumentError("path component is not a string: $(repr(x))"))

    isempty(paths) && throw(ArgumentError("collection of path components must be non-empty"))
    assertstring(paths[1])
    path = paths[1]
    for i in firstindex(paths)+1:lastindex(paths)
        p = paths[i]
        assertstring(p)
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

joinpath(paths::AbstractString...)::String = joinpath(paths)

"""
    joinpath(parts::AbstractString...)::String
    joinpath(parts::Vector{AbstractString})::String
    joinpath(parts::Tuple{AbstractString})::String

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

```jldoctest
julia> joinpath(["/home/myuser", "example.jl"])
"/home/myuser/example.jl"
```
"""
joinpath

"""
    normpath(path::AbstractString)::String

Normalize a path, removing "." and ".." entries and changing "/" to the canonical path separator
for the system.

# Examples
```jldoctest
julia> normpath("/home/myuser/../example.jl")
"/home/example.jl"

julia> normpath("Documents/Julia") == joinpath("Documents", "Julia")
true
```
"""
function normpath(path::String)
    isabs = isabspath(path)
    isdir = isdirpath(path)
    drive, path = splitdrive(path)
    parts = split(path, path_separator_re; keepempty=false)
    filter!(!=("."), parts)
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
    normpath(path::AbstractString, paths::AbstractString...)::String

Convert a set of paths to a normalized path by joining them together and removing
"." and ".." entries. Equivalent to `normpath(joinpath(path, paths...))`.
"""
normpath(a::AbstractString, b::AbstractString...) = normpath(joinpath(a,b...))

"""
    abspath(path::AbstractString)::String

Convert a path to an absolute path by adding the current directory if necessary.
Also normalizes the path as in [`normpath`](@ref).

# Examples

If you are in a directory called `JuliaExample` and the data you are using is two levels up relative to the `JuliaExample` directory, you could write:

    abspath("../../data")

Which gives a path like `"/home/JuliaUser/data/"`.

See also [`joinpath`](@ref), [`pwd`](@ref), [`expanduser`](@ref).
"""
function abspath(a::String)::String
    if !isabspath(a)
        cwd = pwd()
        a_drive, a_nodrive = splitdrive(a)
        if a_drive != "" && lowercase(splitdrive(cwd)[1]) != lowercase(a_drive)
            cwd = a_drive * path_separator
            a = joinpath(cwd, a_nodrive)
        else
            a = joinpath(cwd, a)
        end
    end
    return normpath(a)
end

"""
    abspath(path::AbstractString, paths::AbstractString...)::String

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
    realpath(path::AbstractString)::String

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
            uv_fs_req_cleanup(req)
            uv_error("realpath($(repr(path)))", ret)
        end
        path = unsafe_string(ccall(:jl_uv_fs_t_ptr, Cstring, (Ptr{Cvoid},), req))
        uv_fs_req_cleanup(req)
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
    c, i = y::Tuple{eltype(path),Int}
    c != '~' && return path
    y = iterate(path, i)
    y === nothing && return homedir()
    y[1]::eltype(path) == '/' && return homedir() * path[i:end]
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
    expanduser(path::AbstractString)::AbstractString

On Unix systems, replace a tilde character at the start of a path with the current user's home directory.

See also: [`contractuser`](@ref).
"""
expanduser(path::AbstractString)

"""
    contractuser(path::AbstractString)::AbstractString

On Unix systems, if the path starts with `homedir()`, replace it with a tilde character.

See also: [`expanduser`](@ref).
"""
contractuser(path::AbstractString)


"""
    relpath(path::AbstractString, startpath::AbstractString = ".")::String

Return a relative filepath to `path` either from the current directory or from an optional
start directory. This is a path computation: the filesystem is not accessed to confirm the
existence or nature of `path` or `startpath`.

On Windows, case sensitivity is applied to every part of the path except drive letters. If
`path` and `startpath` refer to different drives, the absolute path of `path` is returned.
"""
function relpath(path::String, startpath::String = ".")
    isempty(path) && throw(ArgumentError("`path` must be non-empty"))
    isempty(startpath) && throw(ArgumentError("`startpath` must be non-empty"))
    curdir = "."
    pardir = ".."
    path == startpath && return curdir
    if Sys.iswindows()
        path_drive, path_without_drive = splitdrive(path)
        startpath_drive, startpath_without_drive = splitdrive(startpath)
        isempty(startpath_drive) && (startpath_drive = path_drive) # by default assume same as path drive
        uppercase(path_drive) == uppercase(startpath_drive) || return abspath(path) # if drives differ return first path
        path_arr  = split(abspath(path_drive * path_without_drive),      path_separator_re)
        start_arr = split(abspath(path_drive * startpath_without_drive), path_separator_re)
    else
        path_arr  = split(abspath(path),      path_separator_re)
        start_arr = split(abspath(startpath), path_separator_re)
    end
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

# RFC3986 Section 2.1
percent_escape(s) = '%' * join(map(b -> uppercase(string(b, base=16)), codeunits(s)), '%')
# RFC3986 Section 2.3
encode_uri_component(s) = replace(s, r"[^A-Za-z0-9\-_.~/]+" => percent_escape)

"""
    uripath(path::AbstractString)

Encode `path` as a URI as per [RFC8089: The "file" URI
Scheme](https://www.rfc-editor.org/rfc/rfc8089), [RFC3986: Uniform Resource
Identifier (URI): Generic Syntax](https://www.rfc-editor.org/rfc/rfc3986), and
the [Freedesktop File URI spec](https://www.freedesktop.org/wiki/Specifications/file-uri-spec/).

## Examples

```julia-repl
julia> uripath("/home/user/example file.jl") # On a unix machine
"file://<hostname>/home/user/example%20file.jl"

juila> uripath("C:\\Users\\user\\example file.jl") # On a windows machine
"file:///C:/Users/user/example%20file.jl"
```
"""
function uripath end

@static if Sys.iswindows()
    function uripath(path::String)
        path = abspath(path)
        if startswith(path, "\\\\") # UNC path, RFC8089 Appendix E.3
            unixpath = join(eachsplit(path, path_separator_re, keepempty=false), '/')
            string("file://", encode_uri_component(unixpath)) # RFC8089 Section 2
        else
            drive, localpath = splitdrive(path) # Assuming that non-UNC absolute paths on Windows always have a drive component
            unixpath = join(eachsplit(localpath, path_separator_re, keepempty=false), '/')
            encdrive = replace(encode_uri_component(drive), "%3A" => ':', "%7C" => '|') # RFC8089 Appendices D.2, E.2.1, and E.2.2
            string("file:///", encdrive, '/', encode_uri_component(unixpath)) # RFC8089 Section 2
        end
    end
else
    function uripath(path::String)
        localpath = join(eachsplit(abspath(path), path_separator_re, keepempty=false), '/')
        host = if ispath("/proc/sys/fs/binfmt_misc/WSLInterop") # WSL sigil
            distro = get(ENV, "WSL_DISTRO_NAME", "") # See <https://patrickwu.space/wslconf/>
            "wsl\$/$distro" # See <https://github.com/microsoft/terminal/pull/14993> and <https://learn.microsoft.com/en-us/windows/wsl/filesystems>
        else
            gethostname() # Freedesktop File URI Spec, Hostnames section
        end
        string("file://", encode_uri_component(host), '/', encode_uri_component(localpath)) # RFC8089 Section 2
    end
end

uripath(path::AbstractString) = uripath(String(path))
