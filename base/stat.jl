immutable StatStruct
    device  :: Uint
    inode   :: Uint
    mode    :: Uint
    nlink   :: Int
    uid     :: Uint
    gid     :: Uint
    rdev    :: Uint
    size    :: Int64
    blksize :: Int64
    blocks  :: Int64
    mtime   :: Float64
    ctime   :: Float64
end

StatStruct(buf::Union(Vector{Uint8},Ptr{Uint8})) = StatStruct(
     uint(ccall(:jl_stat_dev,     Uint32,  (Ptr{Uint8},), buf)),
     uint(ccall(:jl_stat_ino,     Uint32,  (Ptr{Uint8},), buf)),
     uint(ccall(:jl_stat_mode,    Uint32,  (Ptr{Uint8},), buf)),
      int(ccall(:jl_stat_nlink,   Uint32,  (Ptr{Uint8},), buf)),
     uint(ccall(:jl_stat_uid,     Uint32,  (Ptr{Uint8},), buf)),
     uint(ccall(:jl_stat_gid,     Uint32,  (Ptr{Uint8},), buf)),
     uint(ccall(:jl_stat_rdev,    Uint32,  (Ptr{Uint8},), buf)),
    int64(ccall(:jl_stat_size,    Uint64,  (Ptr{Uint8},), buf)),
    int64(ccall(:jl_stat_blksize, Uint64,  (Ptr{Uint8},), buf)),
    int64(ccall(:jl_stat_blocks,  Uint64,  (Ptr{Uint8},), buf)),
          ccall(:jl_stat_mtime,   Float64, (Ptr{Uint8},), buf),
          ccall(:jl_stat_ctime,   Float64, (Ptr{Uint8},), buf),
)

show(io::IO, st::StatStruct) = print("StatStruct(mode=$(oct(st.mode,6)), size=$(st.size))")

# stat & lstat functions

const stat_buf = Array(Uint8, ccall(:jl_sizeof_stat, Int32, ()))
macro stat_call(sym,arg1type,arg)
    quote
        fill!(stat_buf,0)
        r = ccall($(Expr(:quote,sym)), Int32, ($arg1type,Ptr{Uint8}), $(esc(arg)), stat_buf)
        r==0 || r==UV_ENOENT || r==UV_ENOTDIR || throw(UVError("stat",r))
        st = StatStruct(stat_buf)
        if ispath(st) != (r==0)
            error("stat returned zero type for a valid path")
        end
        st
    end
end

stat(fd::RawFD)     = @stat_call jl_fstat Int32 fd.fd
stat(fd::Integer)   = @stat_call jl_fstat Int32 fd
stat(path::ByteString)  = @stat_call jl_stat  Ptr{Uint8} path
stat(path::String) = stat(bytestring(path))
lstat(path::ByteString) = @stat_call jl_lstat Ptr{Uint8} path
lstat(path::String) = lstat(bytestring(path))

stat(path...) = stat(joinpath(path...))
lstat(path...) = lstat(joinpath(path...))

# mode type predicates

    ispath(st::StatStruct) = st.mode & 0xf000 != 0x0000
    isfifo(st::StatStruct) = st.mode & 0xf000 == 0x1000
 ischardev(st::StatStruct) = st.mode & 0xf000 == 0x2000
     isdir(st::StatStruct) = st.mode & 0xf000 == 0x4000
isblockdev(st::StatStruct) = st.mode & 0xf000 == 0x6000
    isfile(st::StatStruct) = st.mode & 0xf000 == 0x8000
    islink(st::StatStruct) = st.mode & 0xf000 == 0xa000
  issocket(st::StatStruct) = st.mode & 0xf000 == 0xc000

# mode permission predicates

issetuid(st::StatStruct) = (st.mode & 0o4000) > 0
issetgid(st::StatStruct) = (st.mode & 0o2000) > 0
issticky(st::StatStruct) = (st.mode & 0o1000) > 0

  isreadable(st::StatStruct) = (st.mode & 0o444) > 0
  iswritable(st::StatStruct) = (st.mode & 0o222) > 0
isexecutable(st::StatStruct) = (st.mode & 0o111) > 0

uperm(st::StatStruct) = uint8(st.mode >> 6) & 0x7
gperm(st::StatStruct) = uint8(st.mode >> 3) & 0x7
operm(st::StatStruct) = uint8(st.mode     ) & 0x7

# mode predicate methods for file names

for f in {
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
    :isreadable
    :iswritable
    :isexecutable
    :uperm
    :gperm
    :operm
}
    @eval ($f)(path...)  = ($f)(stat(path...))
end

islink(path...) = islink(lstat(path...))


# some convenience functions

filemode(path...) = stat(path...).mode
filesize(path...) = stat(path...).size
   mtime(path...) = stat(path...).mtime
   ctime(path...) = stat(path...).ctime

samefile(a::StatStruct, b::StatStruct) = a.device==b.device && a.inode==b.inode
samefile(a::String, b::String) = samefile(stat(a),stat(b))
