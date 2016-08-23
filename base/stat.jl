# This file is a part of Julia. License is MIT: http://julialang.org/license

# filesystem operations

export
    ctime,
    filemode,
    filesize,
    gperm,
    isblockdev,
    ischardev,
    isdir,
    isdir_casesensitive,
    isfifo,
    isfile,
    isfile_casesensitive,
    islink,
    ismount,
    ispath,
    ispath_casesensitive,
    issetgid,
    issetuid,
    issocket,
    issticky,
    lstat,
    mtime,
    operm,
    stat,
    uperm

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

const stat_buf = Array{UInt8}(ccall(:jl_sizeof_stat, Int32, ()))
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
    infoa = stat(a)
    infob = stat(b)
    if ispath(infoa) && ispath(infob)
        samefile(infoa, infob)
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

# Cross-platform case-sensitive path canonicalization.
# _ispath_casesensitive(path) assumes that ispath(path) is true,
# and returns whether path matches the case stored in the filesystem.
if is_unix() && !is_apple()
    # assume case-sensitive filesystems, don't have to do anything
    _ispath_casesensitive(path) = true
elseif is_windows()
    # GetLongPathName Win32 function returns the case-preserved filename on NTFS.
    _ispath_casesensitive(path) = Filesystem.longpath(path) == path
elseif is_apple()
    # HFS+ filesystem is case-preserving. The getattrlist API returns
    # a case-preserved filename. In the rare event that HFS+ is operating
    # in case-sensitive mode, this will still work but will be redundant.

    # Constants from <sys/attr.h>
    const ATRATTR_BIT_MAP_COUNT = 5
    const ATTR_CMN_NAME = 1
    const BITMAPCOUNT = 1
    const COMMONATTR = 5
    const FSOPT_NOFOLLOW = 1  # Don't follow symbolic links

    const attr_list = zeros(UInt8, 24)
    attr_list[BITMAPCOUNT] = ATRATTR_BIT_MAP_COUNT
    attr_list[COMMONATTR] = ATTR_CMN_NAME

    # This essentially corresponds to the following C code:
    # attrlist attr_list;
    # memset(&attr_list, 0, sizeof(attr_list));
    # attr_list.bitmapcount = ATTR_BIT_MAP_COUNT;
    # attr_list.commonattr = ATTR_CMN_NAME;
    # struct Buffer {
    #    u_int32_t total_length;
    #    u_int32_t filename_offset;
    #    u_int32_t filename_length;
    #    char filename[max_filename_length];
    # };
    # Buffer buf;
    # getattrpath(path, &attr_list, &buf, sizeof(buf), FSOPT_NOFOLLOW);
    function _ispath_casesensitive(path)
        # we need to strip out trailing '/' for directories
        path = rstrip(path, '/')
        const header_size = 12
        path_basename = String(basename(path))
        buf = Array{UInt8}(length(path_basename) + header_size + 1)
        local casepreserved_basename
        while true
            ret = ccall(:getattrlist, Cint,
                        (Cstring, Ptr{Void}, Ptr{Void}, Csize_t, Culong),
                        path, attr_list, buf, sizeof(buf), FSOPT_NOFOLLOW)
            systemerror(:getattrlist, ret ≠ 0)
            filename_length = unsafe_load(
              convert(Ptr{UInt32}, pointer(buf) + 8))
            if (filename_length + header_size) > length(buf)
                resize!(buf, filename_length + header_size)
                continue
            end
            casepreserved_basename =
              view(buf, (header_size+1):(header_size+filename_length-1))
            break
        end
        # Hack to compensate for inability to create a string from a subarray with no allocations.
        path_basename.data == casepreserved_basename && return true

        # If there is no match, it's possible that the file does exist but HFS+
        # performed unicode normalization. See  https://developer.apple.com/library/mac/qa/qa1235/_index.html.
        isascii(path_basename) && return false
        normalize_string(path_basename, :NFD).data == casepreserved_basename
    end
else
    # Generic fallback that performs a slow directory listing.
    function _ispath_casesensitive(path)
        dir, filename = splitdir(path)
        any(readdir(dir) .== filename)
    end
end
for f in (:ispath, :isfile, :isdir)
    fc = Symbol(f,"_casesensitive")
    @eval @doc let f=$(string(f)); """
    $(f)_casesensitive(path)

This function implements a case-sensitive `$f` on filesystems (e.g. Mac and Windows)
that are case-insensitive but case-preserving.   It is identical to `$f` except
that it returns `false` if `basename(path)` does not also match the *case*
of the path stored in the filesystem.  (It is equivalent to `$f` on case-sensitive
filesystems.)"""; end -> $fc(path) = $f(path) && _ispath_casesensitive(path)
    @eval $fc(path...) = $fc(joinpath(path...))
end
