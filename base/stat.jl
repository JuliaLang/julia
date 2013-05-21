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

Stat(buf::Union(Vector{Uint8},Ptr{Uint8})) = Stat(
    uint(ccall(:jl_stat_dev,     Uint32,  (Ptr{Uint8},), buf)),
    uint(ccall(:jl_stat_ino,     Uint32,  (Ptr{Uint8},), buf)),
    uint(ccall(:jl_stat_mode,    Uint32,  (Ptr{Uint8},), buf)),
     int(ccall(:jl_stat_nlink,   Uint32,  (Ptr{Uint8},), buf)),
    uint(ccall(:jl_stat_uid,     Uint32,  (Ptr{Uint8},), buf)),
    uint(ccall(:jl_stat_gid,     Uint32,  (Ptr{Uint8},), buf)),
    uint(ccall(:jl_stat_rdev,    Uint32,  (Ptr{Uint8},), buf)),
     int(ccall(:jl_stat_size,    Coff_t,  (Ptr{Uint8},), buf)),
     int(ccall(:jl_stat_blksize, Uint32,  (Ptr{Uint8},), buf)),
     int(ccall(:jl_stat_blocks,  Uint32,  (Ptr{Uint8},), buf)),
         ccall(:jl_stat_mtime,   Float64, (Ptr{Uint8},), buf),
         ccall(:jl_stat_ctime,   Float64, (Ptr{Uint8},), buf),
)

show(io::IO, st::Stat) = print("Stat(mode=$(oct(st.mode,6)), size=$(st.size))")

# stat & lstat functions

const stat_buf = Array(Uint8, ccall(:jl_sizeof_stat, Int32, ()))
macro stat_call(sym,arg1type,arg)
    quote
        fill!(stat_buf,0)
        r = ccall($(Expr(:quote,sym)), Int32, ($arg1type,Ptr{Uint8}), $arg, stat_buf)
        uv_errno = _uv_lasterror(eventloop())
        ENOENT, ENOTDIR = 34, 27
        systemerror(:stat, r!=0 && uv_errno!=ENOENT && uv_errno!=ENOTDIR)
        st = Stat(stat_buf)
        if ispath(st) != (r==0)
            error("WTF: stat returned zero type for a valid path!?")
        end
        st
    end
end

stat(fd::Integer)   = @stat_call jl_fstat Int32 fd
stat(path::String)  = @stat_call jl_stat  Ptr{Uint8} path
lstat(path::String) = @stat_call jl_lstat Ptr{Uint8} path

stat(path...) = stat(joinpath(path...))
lstat(path...) = lstat(joinpath(path...))

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

issetuid(mode::Unsigned) = (mode & 0o4000) > 0
issetgid(mode::Unsigned) = (mode & 0o2000) > 0
issticky(mode::Unsigned) = (mode & 0o1000) > 0

  isreadable(mode::Unsigned) = (mode & 0o444) > 0
 iswriteable(mode::Unsigned) = (mode & 0o222) > 0
isexecutable(mode::Unsigned) = (mode & 0o111) > 0

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
    @eval ($f)(st::Stat) = ($f)(st.mode)
    @eval ($f)(path...)  = ($f)(stat(path...))
end

islink(path...) = islink(lstat(path...))

# some convenience functions

filemode(path...) = stat(path...).mode
filesize(path...) = stat(path...).size
   mtime(path...) = stat(path...).mtime
   ctime(path...) = stat(path...).ctime

samefile(a::Stat, b::Stat) = a.device==b.device && a.inode==b.inode
samefile(a::String, b::String) = samefile(stat(a),stat(b))
