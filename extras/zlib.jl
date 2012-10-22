module Zlib
import Base.*

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

# gz files interface
   gzopen,

# ZError, related constants (zlib_h.jl)
   ZError,
   Z_ERRNO,
   Z_STREAM_ERROR,
   Z_DATA_ERROR,
   Z_MEM_ERROR,
   Z_BUF_ERROR,
   Z_VERSION_ERROR

load("zlib_h.jl")

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
    ret = ccall(dlsym(_zlib, :compress2), Int32, (Ptr{Uint}, Ptr{Uint}, Ptr{Uint}, Uint, Int32),
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

type GzStream <: IO
    gzstruct::Ptr{Void}
    name::String
    read_buffer::Vector{Uint8}
    read_buffer_size::Int
    read_buffer_pos::Int
    function GzStream(struct, name)
        new(struct, name, Array(Uint8, Z_DEFAULT_BUFSIZE), 0, 0)
    end
end

show(io, s::GzStream) = print(io, "GzStream(", s.name, ")")

function gzopen(path::String, mode::String)
    if mode != "rb" && mode != "wb"
        error("unsupported gzopen mode: ", mode)
    end
    if mode == "rb"
        fmode = "r"
    elseif mode == "wb"
        fmode = "w"
    end
    f = open(path, fmode)
    fdes = int32(fd(f))
    fdes_dup = dup(FileDes(fdes))
    close(f)
    struct = ccall(dlsym(_zlib, :gzdopen), Ptr{Void}, (Int32, Ptr{Uint8}), fdes_dup.fd, bytestring(mode))
    if struct == C_NULL
        error("gzopen: failed to open file: $path")
    end
    return GzStream(struct, strcat("<file ", path, ">"))
end

function gzopen(f::Function, args...)
    io = gzopen(args...)
    x = try f(io) catch err
        close(io)
        throw(err)
    end
    close(io)
    return x
end

function close(gzs::GzStream)
    if gzs.gzstruct == C_NULL
        return
    end
    ret = ccall(dlsym(_zlib, :gzclose), Int32, (Ptr{Void},), gzs.gzstruct)
    if ret != Z_OK
        error("error closing GzStream")
    end
    gzs.gzstruct = C_NULL
    gzs.name = strcat("[CLOSED] ", gzs.name)
    return
end

function write(gzs::GzStream, x::Uint8)
    ret = ccall(dlsym(_zlib, :gzputc), Int32, (Ptr{Void,}, Int32), gzs.gzstruct, x)
    if ret == -1
        error("gzwrite failed")
    end
    return
end

function _gzread_chunk(s::GzStream)
    ret = ccall(dlsym(_zlib, :gzread), Int32, (Ptr{Void}, Ptr{Void}, Int32), s.gzstruct, pointer(s.read_buffer), Z_DEFAULT_BUFSIZE)
    if ret == -1
        error("gzread failed")
    end
    s.read_buffer_size = ret
    s.read_buffer_pos = 1
    return
end

function read(s::GzStream, x::Type{Uint8})
    if s.read_buffer_pos == 0
        _gzread_chunk(s)
    end
    if s.read_buffer_size == 0
        error("gzread: end of file") # XXX?
    end
    ret = s.read_buffer[s.read_buffer_pos]
    s.read_buffer_pos += 1
    if s.read_buffer_pos > s.read_buffer_size
        s.read_buffer_pos = 0
    end
    return ret
end

function eof(s::GzStream)
    if s.read_buffer_pos == 0
        return ccall(dlsym(_zlib, :gzeof), Int32, (Ptr{Void},), s.gzstruct) == 1
    else
        return false
    end
end

readline(s::GzStream) = readuntil(s, '\n')

function readuntil(s::GzStream, delim::Char)
    dest = memio()
    while !eof(s)
        c = char(read(s, Uint8))
        print(dest, c)
        if c == delim
            break
        end
    end
    takebuf_string(dest)
end

function readall(s::GzStream)
    dest = memio()
    while !eof(s)
        c = char(read(s, Uint8))
        print(dest, c)
    end
    takebuf_string(dest)
end

end # module Zlib
