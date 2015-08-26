# This file is a part of Julia. License is MIT: http://julialang.org/license

immutable StatStruct
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

show(io::IO, st::StatStruct) = print(io, "StatStruct(mode=$(oct(filemode(st),6)), size=$(filesize(st)))")

# stat & lstat functions

const stat_buf = Array(UInt8, ccall(:jl_sizeof_stat, Int32, ()))
macro stat_call(sym, arg1type, arg)
    quote
        fill!(stat_buf,0)
        r = ccall($(Expr(:quote,sym)), Int32, ($arg1type, Ptr{UInt8}), $(esc(arg)), stat_buf)
        r==0 || r==Base.UV_ENOENT || r==Base.UV_ENOTDIR || throw(UVError("stat",r))
        st = StatStruct(stat_buf)
        if ispath(st) != (r==0)
            error("stat returned zero type for a valid path")
        end
        st
    end
end

stat(fd::RawFD)     = @stat_call jl_fstat Int32 fd.fd
stat(fd::Integer)   = @stat_call jl_fstat Int32 fd
stat(path::AbstractString)  = @stat_call jl_stat  Cstring path
lstat(path::AbstractString) = @stat_call jl_lstat Cstring path

stat(path...) = stat(joinpath(path...))
lstat(path...) = lstat(joinpath(path...))

# some convenience functions

filemode(st::StatStruct) = st.mode
filesize(st::StatStruct) = st.size
   mtime(st::StatStruct) = st.mtime
   ctime(st::StatStruct) = st.ctime

# mode type predicates

    ispath(st::StatStruct) = filemode(st) & 0xf000 != 0x0000
    isfifo(st::StatStruct) = filemode(st) & 0xf000 == 0x1000
 ischardev(st::StatStruct) = filemode(st) & 0xf000 == 0x2000
     isdir(st::StatStruct) = filemode(st) & 0xf000 == 0x4000
isblockdev(st::StatStruct) = filemode(st) & 0xf000 == 0x6000
    isfile(st::StatStruct) = filemode(st) & 0xf000 == 0x8000
    islink(st::StatStruct) = filemode(st) & 0xf000 == 0xa000
  issocket(st::StatStruct) = filemode(st) & 0xf000 == 0xc000

# mode permission predicates

issetuid(st::StatStruct) = (filemode(st) & 0o4000) > 0
issetgid(st::StatStruct) = (filemode(st) & 0o2000) > 0
issticky(st::StatStruct) = (filemode(st) & 0o1000) > 0

uperm(st::StatStruct) = UInt8((filemode(st) >> 6) & 0x7)
gperm(st::StatStruct) = UInt8((filemode(st) >> 3) & 0x7)
operm(st::StatStruct) = UInt8((filemode(st)     ) & 0x7)

# mode predicate methods for file names

for f in Symbol[
    :ispath
    :isfifo
    :ischardev
    :isdir
    :isblockdev
    :isfile
    :islink
    :issocket
    :issetuid
    :issetgid
    :issticky
    :uperm
    :gperm
    :operm
    :filemode
    :filesize
    :mtime
    :ctime
]
    @eval ($f)(path...)  = ($f)(stat(path...))
end

islink(path...) = islink(lstat(path...))

# samefile can be used for files and directories: #11145#issuecomment-99511194
samefile(a::StatStruct, b::StatStruct) = a.device==b.device && a.inode==b.inode
function samefile(a::AbstractString, b::AbstractString)
    if ispath(a) && ispath(b)
        samefile(stat(a),stat(b))
    else
        return false
    end
end

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
