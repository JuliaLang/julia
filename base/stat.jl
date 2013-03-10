immutable Stat
    device  :: Uint
    inode   :: Uint
    mode    :: Uint
    nlink   :: Int
    uid     :: Uint
    gid     :: Uint
    rdev    :: Uint
    size    :: Int
    blksize :: Int
    blocks  :: Int
    mtime   :: Float64
    ctime   :: Float64
end

Stat(buf::Vector{Uint8}) = Stat(
    uint(ccall(:jl_stat_dev,     Uint32,  (Ptr{Uint8},), buf)),
    uint(ccall(:jl_stat_ino,     Uint32,  (Ptr{Uint8},), buf)),
    uint(ccall(:jl_stat_mode,    Uint32,  (Ptr{Uint8},), buf)),
     int(ccall(:jl_stat_nlink,   Uint32,  (Ptr{Uint8},), buf)),
    uint(ccall(:jl_stat_uid,     Uint32,  (Ptr{Uint8},), buf)),
    uint(ccall(:jl_stat_gid,     Uint32,  (Ptr{Uint8},), buf)),
    uint(ccall(:jl_stat_rdev,    Uint32,  (Ptr{Uint8},), buf)),
         ccall(:jl_stat_size,    Int,     (Ptr{Uint8},), buf),
     int(ccall(:jl_stat_blksize, Uint32,  (Ptr{Uint8},), buf)),
     int(ccall(:jl_stat_blocks,  Uint32,  (Ptr{Uint8},), buf)),
         ccall(:jl_stat_mtime,   Float64, (Ptr{Uint8},), buf),
         ccall(:jl_stat_ctime,   Float64, (Ptr{Uint8},), buf),
)

show(io::IO, st::Stat) = print("Stat(mode=$(oct(st.mode,6)), size=$(st.size))")

# stat & lstat functions

const stat_buf = Array(Uint8, ccall(:jl_sizeof_stat, Int32, ()))
macro stat_call(sym,arg)
    quote
        fill!(stat_buf,0)
        r = ccall($(Expr(:quote,sym)), Int32, (Ptr{Uint8},Ptr{Uint8}), $arg, stat_buf)
        uv_errno = _uv_lasterror(eventloop())
        ENOENT = 34
        system_error(:stat, r!=0 && uv_errno!=ENOENT)
        st = Stat(stat_buf)
        if ispath(st) != (r==0)
            error("WTF: stat returned zero type for a valid path!?")
        end
        st
    end
end

stat(path::String)  = @stat_call jl_stat  path
stat(fd::Integer)   = @stat_call jl_fstat fd
lstat(path::String) = @stat_call jl_lstat path

# mode type predicates

    ispath(mode::Unsigned) = mode & 0xf000 != 0x0000
    isfifo(mode::Unsigned) = mode & 0xf000 == 0x1000
 ischardev(mode::Unsigned) = mode & 0xf000 == 0x2000
     isdir(mode::Unsigned) = mode & 0xf000 == 0x4000
isblockdev(mode::Unsigned) = mode & 0xf000 == 0x6000
    isfile(mode::Unsigned) = mode & 0xf000 == 0x8000
    islink(mode::Unsigned) = mode & 0xf000 == 0xa000
  issocket(mode::Unsigned) = mode & 0xf000 == 0xc000

# mode permission predicates

issetuid(mode::Unsigned) = (mode & 0x800) > 0
issetgid(mode::Unsigned) = (mode & 0x400) > 0
issticky(mode::Unsigned) = (mode & 0x200) > 0

  isreadable(mode::Unsigned) = (mode & 0x124) > 0
 iswriteable(mode::Unsigned) = (mode & 0x092) > 0
isexecutable(mode::Unsigned) = (mode & 0x049) > 0

uperm(mode::Unsigned) = uint8(mode >> 6) & 0x7
gperm(mode::Unsigned) = uint8(mode >> 3) & 0x7
operm(mode::Unsigned) = uint8(mode     ) & 0x7

# mode predicate methods for file names & stat objects

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
    :iswriteable
    :isexecutable
    :uperm
    :gperm
    :operm
}
    @eval ($f)(st::Stat)     = ($f)(st.mode)
    @eval ($f)(path::String) = ($f)(stat(path))
end

islink(path::String) = islink(lstat(path))

# some convenience functions

filemode(path::String) = stat(path).mode
filesize(path::String) = stat(path).size
   mtime(path::String) = stat(path).mtime
   ctime(path::String) = stat(path).ctime

samefile(a::Stat, b::Stat) = a.device==b.device && a.inode==b.inode
samefile(a::String, b::String) = samefile(stat(a),stat(b))
