# general zlib constants, definitions

const _zlib = "libz"

# Constants

zlib_version = bytestring(ccall((:zlibVersion, _zlib), Ptr{Uint8}, ()))
ZLIB_VERSION = tuple([int(c) for c in split(zlib_version, '.')]...)

# Flush values
const Z_NO_FLUSH       = int32(0)
const Z_PARTIAL_FLUSH  = int32(1)
const Z_SYNC_FLUSH     = int32(2)
const Z_FULL_FLUSH     = int32(3)
const Z_FINISH         = int32(4)
const Z_BLOCK          = int32(5)
const Z_TREES          = int32(6)

# Return codes
const Z_OK             = int32(0)
const Z_STREAM_END     = int32(1)
const Z_NEED_DICT      = int32(2)
const Z_ERRNO          = int32(-1)
const Z_STREAM_ERROR   = int32(-2)
const Z_DATA_ERROR     = int32(-3)
const Z_MEM_ERROR      = int32(-4)
const Z_BUF_ERROR      = int32(-5)
const Z_VERSION_ERROR  = int32(-6)


# Zlib errors as Exceptions
zerror(e::Integer) = bytestring(ccall((:zError, _zlib), Ptr{Uint8}, (Int32,), e))
type ZError <: Exception
    err::Int32
    err_str::String

    ZError(e::Integer) = (e == Z_ERRNO ? new(e, strerror()) : new(e, zerror(e)))
end

# Compression Levels
const Z_NO_COMPRESSION      = int32(0)
const Z_BEST_SPEED          = int32(1)
const Z_BEST_COMPRESSION    = int32(9)
const Z_DEFAULT_COMPRESSION = int32(-1)

# Compression Strategy
const Z_FILTERED             = int32(1)
const Z_HUFFMAN_ONLY         = int32(2)
const Z_RLE                  = int32(3)
const Z_FIXED                = int32(4)
const Z_DEFAULT_STRATEGY     = int32(0)

# data_type field
const Z_BINARY    = int32(0)
const Z_TEXT      = int32(1)
const Z_ASCII     = Z_TEXT
const Z_UNKNOWN   = int32(2)

# deflate compression method
const Z_DEFLATED    = int32(8)

# misc
const Z_NULL   = C_NULL

# Constants for use with gzbuffer
const Z_DEFAULT_BUFSIZE = 8192
const Z_BIG_BUFSIZE = 131072

# Constants for use with gzseek
const SEEK_SET = int32(0)
const SEEK_CUR = int32(1)

# Create ZFileOffset alias
# This is usually the same as FileOffset, 
# unless we're on a 32-bit system and
# 64-bit functions are not available

# Get compile-time option flags
zlib_compile_flags = ccall((:zlibCompileFlags, _zlib), Uint, ())

let _zlib_h = dlopen("libz")
    global ZFileOffset

    z_off_t_sz   = 2 << ((zlib_compile_flags >> 6) & uint(3))
    if z_off_t_sz == sizeof(FileOffset) || (sizeof(FileOffset) == 8 && dlsym_e(_zlib_h, :gzopen64) != C_NULL)
        typealias ZFileOffset FileOffset
    elseif z_off_t_sz == 4      # 64-bit functions not available
        typealias ZFileOffset Int32
    else
        error("Can't figure out what to do with ZFileOffset.  sizeof(z_off_t) = ", z_off_t_sz)
    end
end
