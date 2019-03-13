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
function homedir()
    path_max = 1024
    buf = Vector{UInt8}(undef, path_max)
    sz = RefValue{Csize_t}(path_max + 1)
    while true
        rc = ccall(:uv_os_homedir, Cint, (Ptr{UInt8}, Ptr{Csize_t}), buf, sz)
        if rc == 0
            resize!(buf, sz[])
            return String(buf)
        elseif rc == Base.UV_ENOBUFS
            resize!(buf, sz[] - 1)
        else
            error("unable to retrieve home directory")
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

Get the directory part of a path.

# Examples
```jldoctest
julia> dirname("/home/myuser")
"/home"
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
        buf = Array{UInt16}(undef, 256)
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


module FOLDERID

struct GUID
    l1::Culong
    w1::Cushort
    w2::Cushort
    b::NTuple{8,Cuchar}
end

const Fonts                  = GUID(0xFD228CB7, 0xAE11, 0x4AE3, (0x86, 0x4C, 0x16, 0xF3, 0x91, 0x0A, 0xB8, 0xFE))
const Desktop                = GUID(0xB4BFCC3A, 0xDB2C, 0x424C, (0xB0, 0x29, 0x7F, 0xE9, 0x9A, 0x87, 0xC6, 0x41))
const System                 = GUID(0x1AC14E77, 0x02E7, 0x4E5D, (0xB7, 0x44, 0x2E, 0xB1, 0xAE, 0x51, 0x98, 0xB7))
const SystemX86              = GUID(0xD65231B0, 0xB2F1, 0x4857, (0xA4, 0xCE, 0xA8, 0xE7, 0xC6, 0xEA, 0x7D, 0x27))
const Windows                = GUID(0xF38BF404, 0x1D43, 0x42F2, (0x93, 0x05, 0x67, 0xDE, 0x0B, 0x28, 0xFC, 0x23))
const Profile                = GUID(0x5E6C858F, 0x0E22, 0x4760, (0x9A, 0xFE, 0xEA, 0x33, 0x17, 0xB6, 0x71, 0x73))
const ProgramFilesX86        = GUID(0x7C5A40EF, 0xA0FB, 0x4BFC, (0x87, 0x4A, 0xC0, 0xF2, 0xE0, 0xB9, 0xFA, 0x8E))
const ProgramFilesCommonX86  = GUID(0xDE974D24, 0xD9C6, 0x4D3E, (0xBF, 0x91, 0xF4, 0x45, 0x51, 0x20, 0xB9, 0x17))
const ProgramFilesX64        = GUID(0x6d809377, 0x6af0, 0x444b, (0x89, 0x57, 0xa3, 0x77, 0x3f, 0x02, 0x20, 0x0e))
const ProgramFilesCommonX64  = GUID(0x6365d5a7, 0xf0d , 0x45e5, (0x87, 0xf6, 0xd , 0xa5, 0x6b, 0x6a, 0x4f, 0x7d))
const ProgramFiles           = GUID(0x905e63b6, 0xc1bf, 0x494e, (0xb2, 0x9c, 0x65, 0xb7, 0x32, 0xd3, 0xd2, 0x1a))
const ProgramFilesCommon     = GUID(0xF7F1ED05, 0x9F6D, 0x47A2, (0xAA, 0xAE, 0x29, 0xD3, 0x17, 0xC6, 0xF0, 0x66))
const UserProgramFiles       = GUID(0x5cd7aee2, 0x2219, 0x4a67, (0xb8, 0x5d, 0x6c, 0x9c, 0xe1, 0x56, 0x60, 0xcb))
const UserProgramFilesCommon = GUID(0xbcbd3057, 0xca5c, 0x4622, (0xb4, 0x2d, 0xbc, 0x56, 0xdb, 0x0a, 0xe5, 0x16))
const RoamingAppData         = GUID(0x3EB685DB, 0x65F9, 0x4CF6, (0xA0, 0x3A, 0xE3, 0xEF, 0x65, 0x72, 0x9F, 0x3D))
const LocalAppData           = GUID(0xF1B32785, 0x6FBA, 0x4FCF, (0x9D, 0x55, 0x7B, 0x8E, 0x7F, 0x15, 0x70, 0x91))
const LocalAppDataLow        = GUID(0xA520A1A4, 0x1780, 0x4FF6, (0xBD, 0x18, 0x16, 0x73, 0x43, 0xC5, 0xAF, 0x16))

end

if Sys.iswindows()

import .FOLDERID
import .FOLDERID: GUID

const KF_FLAG_DEFAULT = 0x00000000


"""
    get_known_folder_path(folderid::GUID) -> AbstractString

Windows only function to retrieve the full path of a known folder identified by the folder's FOLDERID.
The FOLDERID module contains the folderid constants available for query. 

# Examples

```
julia> get_known_folder_path(FOLDERID.System)
"C:\\WINDOWS\\system32"

julia> get_known_folder_path(FOLDERID.Fonts)
"C:\\WINDOWS\\Fonts"
```

"""
function get_known_folder_path(folderid::GUID)
    pathptr = Ref{Ptr{Cwchar_t}}()
    result = ccall((:SHGetKnownFolderPath,:shell32), stdcall, Cint,
                    (GUID, UInt32, Ptr{Cvoid}, Ref{Ptr{Cwchar_t}}), folderid, KF_FLAG_DEFAULT, C_NULL, pathptr)
    result != 0 && error()
    pathbuf = unsafe_wrap(Vector{Cwchar_t}, pathptr[], ccall(:wcslen, UInt, (Ptr{Cwchar_t},), pathptr[]))
    path = transcode(String, pathbuf)
    ccall((:CoTaskMemFree, :ole32), stdcall, Cvoid, (Ptr{Cwchar_t},), pathptr[])
    return path
end
end
