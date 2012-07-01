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

const _jl_stat_buf = Array(Uint8, ccall(:jl_sizeof_stat, Int, ()))

function stat(path::String)
    ccall(:jl_stat, Int32, (Ptr{Uint8}, Ptr{Uint8}), path, _jl_stat_buf)
    Stat(_jl_stat_buf)
end
function stat(fd::Integer)
    ccall(:jl_fstat, Int32, (Int, Ptr{Uint8}), fd, _jl_stat_buf)
    Stat(_jl_stat_buf)
end
function lstat(path::String)
    ccall(:jl_lstat, Int32, (Ptr{Uint8}, Ptr{Uint8}), path, _jl_stat_buf)
    Stat(_jl_stat_buf)
end

show(io::IO, st::Stat) = printf(f"Stat(mode=%06o,size=%i)\n", st.mode, st.size)

# accesssor functions

filemode(path::String) = stat(path).mode
filesize(path::String) = stat(path).size
   mtime(path::String) = stat(path).mtime
   ctime(path::String) = stat(path).ctime

# type predicates

    isfifo(st::Stat) = st.mode & 0xf000 == 0x1000
 ischardev(st::Stat) = st.mode & 0xf000 == 0x2000
     isdir(st::Stat) = st.mode & 0xf000 == 0x4000
isblockdev(st::Stat) = st.mode & 0xf000 == 0x6000
    isfile(st::Stat) = st.mode & 0xf000 == 0x8000
    islink(st::Stat) = st.mode & 0xf000 == 0xa000
  issocket(st::Stat) = st.mode & 0xf000 == 0xc000

# permission predicates

  isreadable(mode::Unsigned) = (mode & 0x0124) > 0
 iswriteable(mode::Unsigned) = (mode & 0x0092) > 0
isexecutable(mode::Unsigned) = (mode & 0x0049) > 0

  isreadable(st::Stat) =   isreadable(st.mode)
 iswriteable(st::Stat) =  iswriteable(st.mode)
isexecutable(st::Stat) = isexecutable(st.mode)

 user(mode::Unsigned) = uint8(mode >> 6) & 0x7
group(mode::Unsigned) = uint8(mode >> 3) & 0x7
world(mode::Unsigned) = uint8(mode     ) & 0x7

 user(st::Stat) =  user(st.mode)
group(st::Stat) = group(st.mode)
world(st::Stat) = world(st.mode)

issetuid(mode::Unsigned) = (mode & 0x800) > 0
issetgid(mode::Unsigned) = (mode & 0x400) > 0
issticky(mode::Unsigned) = (mode & 0x200) > 0

# methods for file names

for f in {
    :isfifo
    :ischardev
    :isdir
    :isblockdev
    :isfile
    :issocket
    :isreadable
    :iswriteable
    :isexecutable
    :user
    :group
    :world
}
    @eval ($f)(path::String) = ($f)(stat(path))
end

islink(path::String) = islink(lstat(path))
