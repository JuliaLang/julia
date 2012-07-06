type Stat
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
    ccall(:jl_stat_dev,     Uint,    (Ptr{Uint8},), buf),
    ccall(:jl_stat_ino,     Uint,    (Ptr{Uint8},), buf),
    ccall(:jl_stat_mode,    Uint,    (Ptr{Uint8},), buf),
    ccall(:jl_stat_nlink,   Int,     (Ptr{Uint8},), buf),
    ccall(:jl_stat_uid,     Uint,    (Ptr{Uint8},), buf),
    ccall(:jl_stat_uid,     Uint,    (Ptr{Uint8},), buf),
    ccall(:jl_stat_rdev,    Uint,    (Ptr{Uint8},), buf),
    ccall(:jl_stat_size,    Int,     (Ptr{Uint8},), buf),
    ccall(:jl_stat_blksize, Int,     (Ptr{Uint8},), buf),
    ccall(:jl_stat_blocks,  Int,     (Ptr{Uint8},), buf),
    ccall(:jl_stat_mtime,   Float64, (Ptr{Uint8},), buf),
    ccall(:jl_stat_ctime,   Float64, (Ptr{Uint8},), buf),
)

show(io::IO, st::Stat) = printf(f"Stat(mode=%06o,size=%i)\n", st.mode, st.size)

# stat & lstat functions

const _jl_stat_buf = Array(Uint8, ccall(:jl_sizeof_stat, Int, ()))
macro _jl_stat_call(sym)
    quote
    fill!(_jl_stat_buf,0)
    ccall($(expr(:quote,sym)), Int32, (Ptr{Uint8}, Ptr{Uint8}), path, _jl_stat_buf)
    Stat(_jl_stat_buf)
    end
end

stat(path::String)  = @_jl_stat_call jl_stat
stat(fd::Integer)   = @_jl_stat_call jl_fstat
lstat(path::String) = @_jl_stat_call jl_lstat

# mode type predicates

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

  isreadable(mode::Unsigned) = (mode & 0x0124) > 0
 iswriteable(mode::Unsigned) = (mode & 0x0092) > 0
isexecutable(mode::Unsigned) = (mode & 0x0049) > 0

uperm(mode::Unsigned) = uint8(mode >> 6) & 0x7
gperm(mode::Unsigned) = uint8(mode >> 3) & 0x7
operm(mode::Unsigned) = uint8(mode     ) & 0x7

# mode predicate methods for file names & stat objects

for f in {
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
    @eval ($f)(path::String) = ($f)(stat(path))
    @eval ($f)(st::Stat)     = ($f)(st.mode)
end

islink(path::String) = islink(lstat(path))

# some convenience functions

filemode(path::String) = stat(path).mode
filesize(path::String) = stat(path).size
   mtime(path::String) = stat(path).mtime
   ctime(path::String) = stat(path).ctime

exists(st::Stat) = st.inode != 0
