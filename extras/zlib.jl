module Zlib
using Base

export
# Compression routines
   compress_bound,
   compress,
   compress_to_buffer,

# Compression constants (zlib_h.jl)
   Z_NO_COMPRESSION,
   Z_BEST_SPEED,
   Z_BEST_COMPRESSION,
   Z_DEFAULT_COMPRESSION,

# Uncompress routines
   uncompress,
   uncompress_to_buffer,

# ZError, related constants (zlib_h.jl)
   ZError,
   Z_ERRNO,
   Z_STREAM_ERROR,
   Z_DATA_ERROR,
   Z_MEM_ERROR,
   Z_BUF_ERROR,
   Z_VERSION_ERROR

load("zlib_h")

# zlib functions

# Returns the maximum size of the compressed output buffer for a given uncompressed input size
compress_bound(input_size::Uint) = ccall(dlsym(_zlib, :compressBound), Uint, (Uint, ), input_size)
compress_bound(input_size::Integer) = compress_bound(convert(Uint, input_size))

# Compress
function compress(source::Array{Uint8}, level::Int32)
    dest = zeros(Uint8, compress_bound(length(source)))
    nb = compress_to_buffer(source, dest, level)

    # Shrink the buffer to the actual compressed size
    return grow(dest, nb-length(dest))
end
compress(source::Array{Uint8}, level::Integer) = compress(source, int32(level))
compress(source::Array{Uint8}) = compress(source, Z_DEFAULT_COMPRESSION)
compress(source::String, args...) = compress(convert(Array{Uint8}, source), args...)


# Compress to buffer
# Returns the number of uncompressed bytes in the buffer
# Throws ZError on error
function compress_to_buffer(source::Array{Uint8}, dest::Array{Uint8}, level::Int32)
    # Length-1 array; compress2 will modify this with the actual compressed length
    dest_buf_size = Uint[length(dest)]

    # Compress the input
    ret = ccall(dlsym(_zlib, :compress2), Int32, (Ptr{Uint8}, Ptr{Uint}, Ptr{Uint}, Uint, Int32),
                dest, dest_buf_size, source, length(source), int32(level))

    if ret != Z_OK
        # Note that if ret > 0, it's not an (unrecoverable) error, but that won't happen here
        throw(ZError(ret))
    end

    # Shrink the buffer to the actual compressed size
    return dest_buf_size[1]
end
compress_to_buffer(source::Array{Uint8}, dest::Array{Uint8}, level::Integer) =
    compress_to_buffer(source, dest, int32(level))
compress_to_buffer(source::Array{Uint8}, dest::Array{Uint8}) = 
    compress_to_buffer(source, dest, Z_DEFAULT_COMPRESSION)
compress_to_buffer(source::String, args...) = 
    compress_to_buffer(convert(Array{Uint8}, source), args...)


# Uncompress
# Returns the uncompressed buffer, sized to the number of uncompressed bytes
function uncompress(source::Array{Uint8}, uncompressed_size::Int)
    dest = zeros(Uint8, uncompressed_size)
    sz = 0

    while true
        try
            sz = uncompress_to_buffer(source, dest)
            break
        catch x
            if !isa(x, ZError) ||  x.err != Z_BUF_ERROR
                throw(x)
            end
            # Z_BUF_ERROR: resize buf, try again
            # Note: resizing by powers of 2 seems to be more efficient at allocating memory
            uncompressed_size = nextpow2(uncompressed_size*2)
            grow(dest, uncompressed_size-length(dest))
        end
    end

    # Shrink the buffer to the actual uncompressed size
    return grow(dest, sz-length(dest))
end
uncompress(source::Array{Uint8}) = uncompress(source, nextpow2(length(source)<<1))


# Uncompress to a buffer
# Returns number of bytes uncompressed to buffer
# Throws ZError on error
function uncompress_to_buffer(source::Array{Uint8}, dest::Array{Uint8})
    # Length-1 array; uncompress will modify this with the actual compressed length
    uncompressed_size = length(dest)
    dest_buf_size = Uint[uncompressed_size]

    # Uncompress the input
    ret = ccall(dlsym(_zlib, :uncompress), Int32, (Ptr{Uint}, Ptr{Uint}, Ptr{Uint}, Uint),
                dest, dest_buf_size, source, length(source))

    if ret != Z_OK
        # Note that if ret > 0, it's not an (unrecoverable) error, but that won't happen here
        throw(ZError(ret))
    end

    return dest_buf_size[1]
end

end # module Zlib
