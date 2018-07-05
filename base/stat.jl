# This file is a part of Julia. License is MIT: https://julialang.org/license

# filesystem operations

export
    fileflags,
    filetype,
    ismount,
    permissions

struct StatStruct
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
end

StatStruct() = StatStruct(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

StatStruct(buf::Union{Vector{UInt8},Ptr{UInt8}}) = StatStruct(
    ccall(:jl_stat_dev,     UInt32,  (Ptr{UInt8},), buf),
    ccall(:jl_stat_ino,     UInt32,  (Ptr{UInt8},), buf),
    ccall(:jl_stat_mode,    UInt32,  (Ptr{UInt8},), buf),
    ccall(:jl_stat_nlink,   UInt32,  (Ptr{UInt8},), buf),
    ccall(:jl_stat_uid,     UInt32,  (Ptr{UInt8},), buf),
    ccall(:jl_stat_gid,     UInt32,  (Ptr{UInt8},), buf),
    ccall(:jl_stat_rdev,    UInt32,  (Ptr{UInt8},), buf),
    ccall(:jl_stat_size,    UInt64,  (Ptr{UInt8},), buf),
    ccall(:jl_stat_blksize, UInt64,  (Ptr{UInt8},), buf),
    ccall(:jl_stat_blocks,  UInt64,  (Ptr{UInt8},), buf),
    ccall(:jl_stat_mtime,   Float64, (Ptr{UInt8},), buf),
    ccall(:jl_stat_ctime,   Float64, (Ptr{UInt8},), buf),
)

show(io::IO, st::StatStruct) = print(io, "StatStruct(mode=0o$(string(st.mode, base = 8, pad = 6)), size=$(st.size))")

# stat & lstat functions

macro stat_call(sym, arg1type, arg)
    return quote
        stat_buf = zeros(UInt8, ccall(:jl_sizeof_stat, Int32, ()))
        r = ccall($(Expr(:quote, sym)), Int32, ($(esc(arg1type)), Ptr{UInt8}), $(esc(arg)), stat_buf)
        r == 0 || r == Base.UV_ENOENT || r == Base.UV_ENOTDIR || throw(UVError("stat", r))
        st = StatStruct(stat_buf)
        if (filetype($1) != :invalid) & (r == 0)
            error("stat returned zero type for a valid path")
        end
        return st
    end
end

stat(fd::OS_HANDLE)         = @stat_call jl_fstat OS_HANDLE fd
stat(path::AbstractString)  = @stat_call jl_stat  Cstring path
lstat(path::AbstractString) = @stat_call jl_lstat Cstring path
if RawFD !== OS_HANDLE
    global stat(fd::RawFD)  = stat(Libc._get_osfhandle(fd))
end
stat(fd::Integer)           = stat(RawFD(fd))

"""
    stat(file; link = false)

Returns a structure whose fields contain information about the file.
If `link` is `true, gets the info for the link itself rather than the file it
refers to. Also, if `link` is true, must be called on a file path rather than a
file object or a file descriptor.

The fields of the structure are:

| Name    | Description                                                        |
|:--------|:-------------------------------------------------------------------|
| size    | The size (in bytes) of the file                                    |
| device  | ID of the device that contains the file                            |
| inode   | The inode number of the file                                       |
| mode    | The protection mode of the file                                    |
| nlink   | The number of hard links to the file                               |
| uid     | The user id of the owner of the file                               |
| gid     | The group id of the file owner                                     |
| rdev    | If this file refers to a device, the ID of the device it refers to |
| blksize | The file-system preferred block size for the file                  |
| blocks  | The number of such blocks allocated                                |
| mtime   | Unix timestamp of when the file was last modified                  |
| ctime   | Unix timestamp of when the file was created                        |

"""
function stat(path..., link = false)
    full_path = joinpath(path...)
    if link
        stat(full_path)
    else
        lstat(full_path)
    end
end

"""
    filetype(path; link = false) -> Symbol

Returns the file type of `path`, with the following possibilities:

- `:invalid`: not a valid filesystem path.-
- `:FIFO`: a FIFO
- `:character`: a character device
- `:dir`: a directory
- `:block`: a block device
- `:file`: a regular file
- `:link`: a symbolic link
- `:socket`: a socket

If `link` is true, get the type of a link itself, with the same restrictions as
[`stat`](@ref).
"""
function filetype(st::StatStruct; link = false)
    mode = st.mode & 0xf000
    if mode == 0x0000
        :invalid
    elseif mode == 0x1000
        :FIFO
    elseif mode == 0x2000
        :character
    elseif mode == 0x4000
        :dir
    elseif mode == 0x6000
        :block
    elseif mode == 0x8000
        :file
    elseif mode == 0xa000
        :link
    elseif mode == 0xc000
        :socket
    else
        error("Unknown path type")
    end
end

"""
    fileflags(path)

Returns a named tuple of the flags of a `path`, if the `path`:

- `set_user`: has the setuid flag set
- `set_group`: has the setgid flag set
- `sticky`: has the sticky bit set
"""
function fileflags(st::StatStruct)
    mode = st.mode
    (
        set_user = (mode & 0o4000) > 0,
        set_group = (mode & 0o2000) > 0,
        sticky = (mode & 0o1000) > 0
    )
end

function translate_permissions(read_write_execute)
    read, write_execute = divrem(read_write_execute, 4)
    write, execute = divrem(write_execute, 2)
    (
        read = Bool(read),
        write = Bool(write),
        execute = Bool(execute)
    )
end


permission_bitfield(mode, shift) = UInt8((mode >> shift) & 0x7)

# unexported but necessary for compatibility?
"""
    permission_bitfields(path)

Returns a named tuple of permission bitfields for `user`, `group`, and `other`.

| Value | Description        |
|:------|:-------------------|
| 01    | Execute Permission |
| 02    | Write Permission   |
| 04    | Read Permission    |
"""
function permission_bitfields(st::StatStruct)
    mode = st.mode
    (
        user = permission_bitfield(mode, 6),
        group = permission_bitfield(mode, 3),
        other = permission_bitfield(mode, 0)
    )
end

permission_for_shift(mode, shift) =
    translate_permission(permission_bitfield(mode, shift))

"""
    permissions(path)

Returns a nested named tuple of bools. The outer tuple will classify permissions
as `user`, `group`, or `other`. The inner tuple will classify permisissions as
`read`, `write`, or `execute`.
"""
function permissions(st::StatStruct)
    mode = st.mode
    (
        user = permission_for_shift(mode, 6),
        group = permission_for_shift(mode, 3),
        other = permission_for_shift(mode, 0)
    )
end

permissions(path...) = permissions(stat(path...))

for f in Symbol[
    :fileflags,
    :filetype,
    :permissions
]
    @eval ($f)(path...; link = false)  = ($f)(stat(path...; link = link))
end

# samefile can be used for files and directories: #11145#issuecomment-99511194
samefile(a::StatStruct, b::StatStruct) = a.device==b.device && a.inode==b.inode
function samefile(a::AbstractString, b::AbstractString)
    infoa = stat(a)
    infob = stat(b)
    if filetype(infoa) != :invalid && filetype(infob) != :invalid
        samefile(infoa, infob)
    else
        return false
    end
end

"""
    ismount(path) -> Bool

Returns `true` if `path` is a mount point, `false` otherwise.
"""
function ismount(path...)
    path = joinpath(path...)
    filetype(path) == :dir || return false
    s1 = lstat(path; link = true)
    # Symbolic links cannot be mount points
    filetype(s1) == :link && return false
    parent_path = joinpath(path, "..")
    s2 = lstat(parent_path; link = true)
    # If a directory and its parent are on different devices,  then the
    # directory must be a mount point
    (s1.device != s2.device) && return true
    (s1.inode == s2.inode) && return true
    false
end
