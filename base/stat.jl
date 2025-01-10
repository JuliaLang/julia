# This file is a part of Julia. License is MIT: https://julialang.org/license

# filesystem operations

export
    ctime,
    filemode,
    filesize,
    gperm,
    isblockdev,
    ischardev,
    isdir,
    isfifo,
    isfile,
    islink,
    ismount,
    ispath,
    issetgid,
    issetuid,
    issocket,
    issticky,
    lstat,
    mtime,
    operm,
    stat,
    uperm

"""
    StatStruct

A struct which stores information about a file. Usually
constructed by calling [`stat`](@ref) on a path.

This struct is used internally as the foundation of a number of utility
functions. Some return specific parts of the information stored in it
directly, such as [`filesize`](@ref), [`mtime`](@ref) and [`ctime`](@ref). Others add
some logic on top using bit-manipulation, such as [`isfifo`](@ref), [`ischardev`](@ref), and [`issetuid`](@ref).

The following fields of this struct are considered public API:

| Name    | Type                            | Description                                                        |
|:--------|:--------------------------------|:-------------------------------------------------------------------|
| desc    | `Union{String, Base.OS_HANDLE}` | The path or OS file descriptor                                     |
| size    | `Int64`                         | The size (in bytes) of the file                                    |
| device  | `UInt`                          | ID of the device that contains the file                            |
| inode   | `UInt`                          | The inode number of the file                                       |
| mode    | `UInt`                          | The protection mode of the file                                    |
| nlink   | `Int`                           | The number of hard links to the file                               |
| uid     | `UInt`                          | The user id of the owner of the file                               |
| gid     | `UInt`                          | The group id of the file owner                                     |
| rdev    | `UInt`                          | If this file refers to a device, the ID of the device it refers to |
| blksize | `Int64`                         | The file-system preferred block size for the file                  |
| blocks  | `Int64`                         | The number of 512-byte blocks allocated                            |
| mtime   | `Float64`                       | Unix timestamp of when the file was last modified                  |
| ctime   | `Float64`                       | Unix timestamp of when the file's metadata was changed             |

See also: [`stat`](@ref)
"""
struct StatStruct
    desc    :: Union{String, OS_HANDLE} # for show method, not included in equality or hash
    device  :: UInt
    inode   :: UInt
    mode    :: UInt
    nlink   :: Int
    uid     :: UInt
    gid     :: UInt
    rdev    :: UInt
    size    :: Int64
    blksize :: Int64
    blocks  :: Int64
    mtime   :: Float64
    ctime   :: Float64
    ioerrno :: Int32
end

@eval function Base.:(==)(x::StatStruct, y::StatStruct) # do not include `desc` in equality or hash
    $(let ex = true
        for fld in fieldnames(StatStruct)[2:end]
            ex = :(getfield(x, $(QuoteNode(fld))) === getfield(y, $(QuoteNode(fld))) && $ex)
        end
        Expr(:return, ex)
    end)
end
@eval function Base.hash(obj::StatStruct, h::UInt)
    $(quote
        $(Any[:(h = hash(getfield(obj, $(QuoteNode(fld))), h)) for fld in fieldnames(StatStruct)[2:end]]...)
        return h
    end)
end

StatStruct() = StatStruct("", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, Base.UV_ENOENT)
StatStruct(buf::Union{Memory{UInt8},Vector{UInt8},Ptr{UInt8}}, ioerrno::Int32) = StatStruct("", buf, ioerrno)
StatStruct(desc::Union{AbstractString, OS_HANDLE}, buf::Union{Memory{UInt8},Vector{UInt8},Ptr{UInt8}}, ioerrno::Int32) = StatStruct(
    desc isa OS_HANDLE ? desc : String(desc),
    ioerrno != 0 ? zero(UInt32) : ccall(:jl_stat_dev,     UInt32,  (Ptr{UInt8},), buf),
    ioerrno != 0 ? zero(UInt32) : ccall(:jl_stat_ino,     UInt32,  (Ptr{UInt8},), buf),
    ioerrno != 0 ? zero(UInt32) : ccall(:jl_stat_mode,    UInt32,  (Ptr{UInt8},), buf),
    ioerrno != 0 ? zero(UInt32) : ccall(:jl_stat_nlink,   UInt32,  (Ptr{UInt8},), buf),
    ioerrno != 0 ? zero(UInt32) : ccall(:jl_stat_uid,     UInt32,  (Ptr{UInt8},), buf),
    ioerrno != 0 ? zero(UInt32) : ccall(:jl_stat_gid,     UInt32,  (Ptr{UInt8},), buf),
    ioerrno != 0 ? zero(UInt32) : ccall(:jl_stat_rdev,    UInt32,  (Ptr{UInt8},), buf),
    ioerrno != 0 ? zero(UInt64) : ccall(:jl_stat_size,    UInt64,  (Ptr{UInt8},), buf),
    ioerrno != 0 ? zero(UInt64) : ccall(:jl_stat_blksize, UInt64,  (Ptr{UInt8},), buf),
    ioerrno != 0 ? zero(UInt64) : ccall(:jl_stat_blocks,  UInt64,  (Ptr{UInt8},), buf),
    ioerrno != 0 ? zero(Float64) : ccall(:jl_stat_mtime,   Float64, (Ptr{UInt8},), buf),
    ioerrno != 0 ? zero(Float64) : ccall(:jl_stat_ctime,   Float64, (Ptr{UInt8},), buf),
    ioerrno
)

function iso_datetime_with_relative(t, tnow)
    str = Libc.strftime("%FT%T%z", t)
    secdiff = t - tnow
    for (d, name) in ((24*60*60, "day"), (60*60, "hour"), (60, "minute"), (1, "second"))
        tdiff = round(Int, div(abs(secdiff), d))
        if tdiff != 0 # find first unit difference
            plural = tdiff == 1 ? "" : "s"
            when = secdiff < 0 ? "ago" : "in the future"
            return "$str ($tdiff $name$plural $when)"
        end
    end
    return "$str (just now)"
end


function getusername(uid::Unsigned)
    pwd = Libc.getpwuid(uid, false)
    pwd === nothing && return
    isempty(pwd.username) && return
    return pwd.username
end

function getgroupname(gid::Unsigned)
    gp = Libc.getgrgid(gid, false)
    gp === nothing && return
    isempty(gp.groupname) && return
    return gp.groupname
end

function show_statstruct(io::IO, st::StatStruct, oneline::Bool)
    print(io, oneline ? "StatStruct(" : "StatStruct for ")
    show(io, st.desc)
    code = st.ioerrno
    if code != 0
        print(io, oneline ? " " : "\n ")
        print(io, Base.uverrorname(code), ": ", Base.struverror(code))
    else
        oneline || print(io, "\n  ")
        print(io, " size: ", st.size, " bytes")
        oneline || print(io, "\n")
        print(io, " device: ", st.device)
        oneline || print(io, "\n ")
        print(io, " inode: ", st.inode)
        oneline || print(io, "\n  ")
        print(io, " mode: 0o", string(filemode(st), base = 8, pad = 6), " (", filemode_string(st), ")")
        oneline || print(io, "\n ")
        print(io, " nlink: ", st.nlink)
        oneline || print(io, "\n   ")
        print(io, " uid: $(st.uid)")
        username = getusername(st.uid)
        username === nothing || print(io, " (", username, ")")
        oneline || print(io, "\n   ")
        print(io, " gid: ", st.gid)
        groupname = getgroupname(st.gid)
        groupname === nothing || print(io, " (", groupname, ")")
        oneline || print(io, "\n  ")
        print(io, " rdev: ", st.rdev)
        oneline || print(io, "\n ")
        print(io, " blksz: ", st.blksize)
        oneline || print(io, "\n")
        print(io, " blocks: ", st.blocks)
        tnow = round(UInt, time())
        oneline || print(io, "\n ")
        print(io, " mtime: ", iso_datetime_with_relative(st.mtime, tnow))
        oneline || print(io, "\n ")
        print(io, " ctime: ", iso_datetime_with_relative(st.ctime, tnow))
    end
    oneline && print(io, ")")
    return nothing
end

show(io::IO, st::StatStruct) = show_statstruct(io, st, true)
show(io::IO, ::MIME"text/plain", st::StatStruct) = show_statstruct(io, st, false)

# stat & lstat functions

checkstat(s::StatStruct) = Int(s.ioerrno) in (0, Base.UV_ENOENT, Base.UV_ENOTDIR, Base.UV_EINVAL) ? s : uv_error(string("stat(", repr(s.desc), ")"), s.ioerrno)

macro stat_call(sym, arg1type, arg)
    return quote
        stat_buf = fill!(Memory{UInt8}(undef, Int(ccall(:jl_sizeof_stat, Int32, ()))), 0x00)
        r = ccall($(Expr(:quote, sym)), Int32, ($(esc(arg1type)), Ptr{UInt8}), $(esc(arg)), stat_buf)
        return checkstat(StatStruct($(esc(arg)), stat_buf, r))
    end
end

stat(fd::OS_HANDLE)         = @stat_call jl_fstat OS_HANDLE fd
function stat(path::AbstractString)
    # @info "stat($(repr(path)))" exception=(ErrorException("Fake error for backtrace printing"),stacktrace())
    @stat_call jl_stat  Cstring path
end
function lstat(path::AbstractString)
    # @info "lstat($(repr(path)))" exception=(ErrorException("Fake error for backtrace printing"),stacktrace())
    @stat_call jl_lstat Cstring path
end
if RawFD !== OS_HANDLE
    global stat(fd::RawFD)  = stat(Libc._get_osfhandle(fd))
end

"""
    stat(path)
    stat(path_elements...)

Return a structure whose fields contain information about the file.
If multiple arguments are given, they are joined by [`joinpath`](@ref).

The fields of the structure are:

| Name    | Type                            | Description                                                        |
|:--------|:--------------------------------|:-------------------------------------------------------------------|
| desc    | `Union{String, Base.OS_HANDLE}` | The path or OS file descriptor                                     |
| size    | `Int64`                         | The size (in bytes) of the file                                    |
| device  | `UInt`                          | ID of the device that contains the file                            |
| inode   | `UInt`                          | The inode number of the file                                       |
| mode    | `UInt`                          | The protection mode of the file                                    |
| nlink   | `Int`                           | The number of hard links to the file                               |
| uid     | `UInt`                          | The user id of the owner of the file                               |
| gid     | `UInt`                          | The group id of the file owner                                     |
| rdev    | `UInt`                          | If this file refers to a device, the ID of the device it refers to |
| blksize | `Int64`                         | The file-system preferred block size for the file                  |
| blocks  | `Int64`                         | The number of 512-byte blocks allocated                            |
| mtime   | `Float64`                       | Unix timestamp of when the file was last modified                  |
| ctime   | `Float64`                       | Unix timestamp of when the file's metadata was changed             |
"""
stat(path) = (path2 = joinpath(path); path2 isa typeof(path) ? error("stat not implemented for $(typeof(path))") : stat(path2))
stat(path...) = stat(joinpath(path...))

"""
    lstat(path)
    lstat(path_elements...)

Like [`stat`](@ref), but for symbolic links gets the info
for the link itself rather than the file it refers to.

This function must be called on a file path rather
than a file object or a file descriptor.
"""
lstat(path) = (path2 = joinpath(path); path2 isa typeof(path) ? error("lstat not implemented for $(typeof(path))") : lstat(path2))
lstat(path...) = lstat(joinpath(path...))

# some convenience functions

const filemode_table = (
    [
        (S_IFLNK, "l"),
        (S_IFSOCK, "s"),  # Must appear before IFREG and IFDIR as IFSOCK == IFREG | IFDIR
        (S_IFREG, "-"),
        (S_IFBLK, "b"),
        (S_IFDIR, "d"),
        (S_IFCHR, "c"),
        (S_IFIFO, "p")
    ],
    [
        (S_IRUSR, "r"),
    ],
    [
        (S_IWUSR, "w"),
    ],
    [
        (S_IXUSR|S_ISUID, "s"),
        (S_ISUID, "S"),
        (S_IXUSR, "x")
    ],
    [
        (S_IRGRP, "r"),
    ],
    [
        (S_IWGRP, "w"),
    ],
    [
        (S_IXGRP|S_ISGID, "s"),
        (S_ISGID, "S"),
        (S_IXGRP, "x")
    ],
    [
        (S_IROTH, "r"),
    ],
    [
        (S_IWOTH, "w"),
    ],
    [
        (S_IXOTH|S_ISVTX, "t"),
        (S_ISVTX, "T"),
        (S_IXOTH, "x")
    ]
)

"""
    filemode(path)
    filemode(path_elements...)
    filemode(stat_struct)

Return the mode of the file located at `path`,
or the mode indicated by the file descriptor `stat_struct`.

Equivalent to `stat(path).mode` or `stat_struct.mode`.
"""
filemode(st::StatStruct) = st.mode
filemode_string(st::StatStruct) = filemode_string(st.mode)
function filemode_string(mode)
    str = IOBuffer()
    for table in filemode_table
        complete = true
        for (bit, char) in table
            if mode & bit == bit
                write(str, char)
                complete = false
                break
            end
        end
        complete && write(str, "-")
    end
    return String(take!(str))
end

"""
    filesize(path)
    filesize(path_elements...)
    filesize(stat_struct)

Return the size of the file located at `path`,
or the size indicated by file descriptor `stat_struct`.

Equivalent to `stat(path).size` or `stat_struct.size`.
"""
filesize(st::StatStruct) = st.size

"""
    mtime(path)
    mtime(path_elements...)
    mtime(stat_struct)

Return the unix timestamp of when the file at `path` was last modified,
or the last modified timestamp indicated by the file descriptor `stat_struct`.

Equivalent to `stat(path).mtime` or `stat_struct.mtime`.
"""
mtime(st::StatStruct) = st.mtime

"""
    ctime(path)
    ctime(path_elements...)
    ctime(stat_struct)

Return the unix timestamp of when the metadata of the file at `path` was last modified,
or the last modified metadata timestamp indicated by the file descriptor `stat_struct`.

Equivalent to `stat(path).ctime` or `stat_struct.ctime`.
"""
ctime(st::StatStruct) = st.ctime

# mode type predicates

"""
    ispath(path)::Bool
    ispath(path_elements...)::Bool

Return `true` if a valid filesystem entity exists at `path`,
otherwise returns `false`.

This is the generalization of [`isfile`](@ref), [`isdir`](@ref) etc.
"""
ispath(st::StatStruct) = st.ioerrno == 0
function ispath(path::String)
    # We use `access()` and `F_OK` to determine if a given path exists. `F_OK` comes from `unistd.h`.
    F_OK = 0x00
    r = ccall(:jl_fs_access, Cint, (Cstring, Cint), path, F_OK)
    if !(r in (0, Base.UV_ENOENT, Base.UV_ENOTDIR, Base.UV_EINVAL))
        uv_error(string("ispath(", repr(path), ")"), r)
    end
    return r == 0
end
ispath(path::AbstractString) = ispath(String(path))

"""
    isfifo(path)::Bool
    isfifo(path_elements...)::Bool
    isfifo(stat_struct)::Bool

Return `true` if the file at `path` or file descriptor `stat_struct` is FIFO, `false` otherwise.
"""
isfifo(st::StatStruct) = filemode(st) & 0xf000 == 0x1000

"""
    ischardev(path)::Bool
    ischardev(path_elements...)::Bool
    ischardev(stat_struct)::Bool

Return `true` if the path `path` or file descriptor `stat_struct` refer to a character device, `false` otherwise.
"""
ischardev(st::StatStruct) = filemode(st) & 0xf000 == 0x2000

"""
    isdir(path)::Bool
    isdir(path_elements...)::Bool

Return `true` if `path` points to a directory, `false` otherwise.

# Examples
```jldoctest
julia> isdir(homedir())
true

julia> isdir("not/a/directory")
false
```

See also [`isfile`](@ref) and [`ispath`](@ref).
"""
isdir(st::StatStruct) = filemode(st) & 0xf000 == 0x4000

"""
    isblockdev(path)::Bool
    isblockdev(path_elements...)::Bool
    isblockdev(stat_struct)::Bool

Return `true` if the path `path` or file descriptor `stat_struct` refer to a block device, `false` otherwise.
"""
isblockdev(st::StatStruct) = filemode(st) & 0xf000 == 0x6000

"""
    isfile(path)::Bool
    isfile(path_elements...)::Bool

Return `true` if `path` points to a regular file, `false` otherwise.

# Examples
```jldoctest
julia> isfile(homedir())
false

julia> filename = "test_file.txt";

julia> write(filename, "Hello world!");

julia> isfile(filename)
true

julia> rm(filename);

julia> isfile(filename)
false
```

See also [`isdir`](@ref) and [`ispath`](@ref).
"""
isfile(st::StatStruct) = filemode(st) & 0xf000 == 0x8000

"""
    islink(path)::Bool
    islink(path_elements...)::Bool

Return `true` if `path` points to a symbolic link, `false` otherwise.
"""
islink(st::StatStruct) = filemode(st) & 0xf000 == 0xa000

"""
    issocket(path)::Bool
    issocket(path_elements...)::Bool

Return `true` if `path` points to a socket, `false` otherwise.
"""
issocket(st::StatStruct) = filemode(st) & 0xf000 == 0xc000

# mode permission predicates

"""
    issetuid(path)::Bool
    issetuid(path_elements...)::Bool
    issetuid(stat_struct)::Bool

Return `true` if the file at `path` or file descriptor `stat_struct` have the setuid flag set, `false` otherwise.
"""
issetuid(st::StatStruct) = (filemode(st) & 0o4000) > 0

"""
    issetgid(path)::Bool
    issetgid(path_elements...)::Bool
    issetgid(stat_struct)::Bool

Return `true` if the file at `path` or file descriptor `stat_struct` have the setgid flag set, `false` otherwise.
"""
issetgid(st::StatStruct) = (filemode(st) & 0o2000) > 0

"""
    issticky(path)::Bool
    issticky(path_elements...)::Bool
    issticky(stat_struct)::Bool

Return `true` if the file at `path` or file descriptor `stat_struct` have the sticky bit set, `false` otherwise.
"""
issticky(st::StatStruct) = (filemode(st) & 0o1000) > 0

"""
    uperm(path)
    uperm(path_elements...)
    uperm(stat_struct)

Return a bitfield of the owner permissions for the file at `path` or file descriptor `stat_struct`.

| Value | Description        |
|:------|:-------------------|
| 01    | Execute Permission |
| 02    | Write Permission   |
| 04    | Read Permission    |

The fact that a bitfield is returned means that if the permission
is read+write, the bitfield is "110", which maps to the decimal
value of 0+2+4=6. This is reflected in the printing of the
returned `UInt8` value.

See also [`gperm`](@ref) and [`operm`](@ref).

```jldoctest
julia> touch("dummy_file");  # Create test-file without contents

julia> uperm("dummy_file")
0x06

julia> bitstring(ans)
"00000110"

julia> has_read_permission(path) = uperm(path) & 0b00000100 != 0;  # Use bit mask to check specific bit

julia> has_read_permission("dummy_file")
true

julia> rm("dummy_file")     # Clean up test-file
```
"""
uperm(st::StatStruct) = UInt8((filemode(st) >> 6) & 0x7)

"""
    gperm(path)
    gperm(path_elements...)
    gperm(stat_struct)

Like [`uperm`](@ref) but gets the permissions of the group owning the file.

See also [`operm`](@ref).
"""
gperm(st::StatStruct) = UInt8((filemode(st) >> 3) & 0x7)

"""
    operm(path)
    operm(path_elements...)
    operm(stat_struct)

Like [`uperm`](@ref) but gets the permissions for people who neither own the
file nor are a member of the group owning the file.

See also [`gperm`](@ref).
"""
operm(st::StatStruct) = UInt8((filemode(st)     ) & 0x7)

# mode predicate methods for file names

for f in Symbol[
    :ispath,
    :isfifo,
    :ischardev,
    :isdir,
    :isblockdev,
    :isfile,
    :issocket,
    :issetuid,
    :issetgid,
    :issticky,
    :uperm,
    :gperm,
    :operm,
    :filemode,
    :filesize,
    :mtime,
    :ctime,
]
    @eval ($f)(path...)  = ($f)(stat(path...))
end

islink(path...) = islink(lstat(path...))

# samefile can be used for files and directories: #11145#issuecomment-99511194
function samefile(a::StatStruct, b::StatStruct)
    ispath(a) && ispath(b) && a.device == b.device && a.inode == b.inode
end

"""
    samefile(path_a, path_b)

Check if the paths `path_a` and `path_b` refer to the same existing file or directory.
"""
samefile(a::AbstractString, b::AbstractString) = samefile(stat(a), stat(b))

"""
    ismount(path)::Bool
    ismount(path_elements...)::Bool

Return `true` if `path` is a mount point, `false` otherwise.
"""
function ismount(path...)
    path = joinpath(path...)
    isdir(path) || return false
    s1 = lstat(path)
    # Symbolic links cannot be mount points
    islink(s1) && return false
    parent_path = joinpath(path, "..")
    s2 = lstat(parent_path)
    # If a directory and its parent are on different devices,  then the
    # directory must be a mount point
    (s1.device != s2.device) && return true
    (s1.inode == s2.inode) && return true
    false
end
