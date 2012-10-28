## gzip file io ##
module GZip
import Base.*

export
  GZipStream,
  show,

# io functions
  gzopen,
  gzdopen,
  fd,
  close,
  flush,
  truncate,
  seek,
  skip,
  position,
  eof,
  read,
  readuntil,
  readline,
  write,

# lower-level io functions
  gzgetc,
  gzungetc,
  gzgets,
  gzputc,
  gzwrite,
  gzread,
  gzbuffer,

# GZError, ZError, related constants (zlib_h.jl)
  GZError,
  ZError,
  Z_OK,
  Z_STREAM_END,
  Z_NEED_DICT,
  Z_ERRNO,
  Z_STREAM_ERROR,
  Z_DATA_ERROR,
  Z_MEM_ERROR,
  Z_BUF_ERROR,
  Z_VERSION_ERROR,

# Compression constants (zlib_h.jl)
  Z_NO_COMPRESSION,
  Z_BEST_SPEED,
  Z_BEST_COMPRESSION,
  Z_DEFAULT_COMPRESSION,

# Compression strategy (zlib_h.jl)
  Z_FILTERED,
  Z_HUFFMAN_ONLY,
  Z_RLE,
  Z_FIXED,
  Z_DEFAULT_STRATEGY

load("zlib_h.jl")

# Expected line length for strings
const GZ_LINE_BUFSIZE = 256

# Wrapper around gzFile
type GZipStream <: IO
    name::String
    gz_file::Ptr{Void}
    buf_size::Int

    _closed::Bool

    GZipStream(name::String, gz_file::Ptr{Void}, buf_size::Int) =
        (x = new(name, gz_file, buf_size, false); finalizer(x, close); x)
end
GZipStream(name::String, gz_file::Ptr{Void}) = GZipStream(name, gz_file, Z_DEFAULT_BUFSIZE)

# gzerror
function gzerror(s::GZipStream)
    e = Int32[0]
    msg_p = ccall(dlsym(_zlib, :gzerror), Ptr{Uint8}, (Ptr{Void}, Ptr{Int32}),
                  s.gz_file, e)
    msg = (msg_p == C_NULL ? "" : bytestring(msg_p))
    (e[1], msg)
end

type GZError <: Exception
    err::Int32
    err_str::String

    GZError(e::Integer, str::String) = new(int32(e), str)
    GZError(s::GZipStream) = (a = gzerror(s); new(a[1], a[2]))
end

# show
show(io, s::GZipStream) = print(io, "GZipStream(", s.name, ")")

macro test_eof_gzerr(s, cc, val)
    quote
        ret = $(esc(cc))
        if ret == $(esc(val))
            if eof($(esc(s)))  throw(EOFError())  else  throw(GZError($(esc(s))))  end
        end
        ret
    end
end

macro test_eof_gzerr2(s, cc, val)
    quote
        ret = $(esc(cc))
        if ret == $(esc(val)) && !eof($(esc(s))) throw(GZError($(esc(s)))) end
        ret
    end
end

macro test_gzerror(s, cc, val)
    quote
        ret = $(esc(cc))
        if ret == $(esc(val)) throw(GZError($(esc(s)))) end
        ret
    end
end

macro test_z_ok(cc)
    quote
        ret = $(esc(cc))
        if (ret != Z_OK) throw(ZError(ret)) end
        ret
    end
end

# Easy access to gz reading/writing functions (Internal)
gzgetc(s::GZipStream) =
    @test_eof_gzerr(s, ccall(dlsym(_zlib, :gzgetc), Int32, (Ptr{Void},), s.gz_file), -1)

gzungetc(c::Integer, s::GZipStream) =
    @test_eof_gzerr(s, ccall(dlsym(_zlib, :gzungetc), Int32, (Int32, Ptr{Void}), c, s.gz_file), -1)

gzgets(s::GZipStream, a::Array{Uint8}) =
    @test_eof_gzerr2(s, ccall(dlsym(_zlib, :gzgets), Ptr{Uint8}, (Ptr{Void}, Ptr{Uint8}, Int32),
                              s.gz_file, a, int32(length(a))),          C_NULL)

gzgets(s::GZipStream, p::Ptr{Uint8}, len::Integer) =
    @test_eof_gzerr2(s, ccall(dlsym(_zlib, :gzgets), Ptr{Uint8}, (Ptr{Void}, Ptr{Uint8}, Int32),
                              s.gz_file, p, int32(len)),                C_NULL)

gzputc(s::GZipStream, c::Integer) =
    @test_gzerror(s, ccall(dlsym(_zlib, :gzputc), Int32, (Ptr{Void}, Int32),
                           s.gz_file, int32(c)),                        -1)

gzwrite(s::GZipStream, p::Ptr, len::Integer) =
    @test_gzerror(s, ccall(dlsym(_zlib, :gzwrite), Int32, (Ptr{Void}, Ptr{Void}, Uint32),
                           s.gz_file, p, len),                           0)

gzread(s::GZipStream, p::Ptr, len::Integer) =
    @test_gzerror(s, ccall(dlsym(_zlib, :gzread), Int32, (Ptr{Void}, Ptr{Void}, Uint32),
                           s.gz_file, p, len),                          -1)

gzbuffer(gz_file::Ptr, gz_buf_size::Integer) =
    ccall(dlsym(_zlib, :gzbuffer), Int32, (Ptr{Void}, Uint32), gz_file, gz_buf_size)

#####

function gzopen(fname::String, gzmode::String, gz_buf_size::Integer)
    # gzmode can contain extra characters specifying
    # * compression level (0-9)
    # * strategy ('f' => filtered data, 'h' -> Huffman-only compression,
    #             'R' -> run-length encoding, 'F' -> fixed code compression)
    #
    # '+' is also not allowed

    # For windows, force binary mode; doesn't hurt on unix
    if !contains(gzmode, 'b')
        gzmode *= "b"
    end

    gz_file = ccall(dlsym(_zlib, :gzopen), Ptr{Void}, (Ptr{Uint8}, Ptr{Uint8}), fname, gzmode)
    if gz_file == C_NULL
        throw(GZError(-1, "gzopen failed"))
    end
    if gz_buf_size != Z_DEFAULT_BUFSIZE
        if gzbuffer(gz_file, gz_buf_size) == -1
            # Generally a non-fatal error, although it shouldn't happen here
            gz_buf_size = Z_DEFAULT_BUFSIZE
        end
    end
    return GZipStream("<file $fname>", gz_file, gz_buf_size)
end
gzopen(fname::String, gzmode::String) = gzopen(fname, gzmode, Z_DEFAULT_BUFSIZE)
gzopen(fname::String) = gzopen(fname, "rb", Z_DEFAULT_BUFSIZE)

function gzopen(f::Function, args...)
    io = gzopen(args...)
    x = try f(io) catch err
        close(io)
        throw(err)
    end
    close(io)
    return x
end

function gzdopen(name::String, fd::Integer, gzmode::String, gz_buf_size::Integer)
    if !contains(gzmode, 'b')
        gzmode *= "b"
    end

    # Duplicate the file descriptor, since we have no way to tell gzclose()
    # not to close the original fd
    # TODO: this should work, but some early testing showed some problems...
    dup_fd = ccall(:dup, Int32, (Int32,), fd)

    gz_file = ccall(dlsym(_zlib, :gzdopen), Ptr{Void}, (Int32, Ptr{Uint8}), dup_fd, gzmode)
    if gz_file == C_NULL
        throw(GZError(-1, "gzdopen failed"))
    end
    if gz_buf_size != Z_DEFAULT_BUFSIZE
        if gzbuffer(gz_file, gz_buf_size) == -1
            # Generally a non-fatal error, although it shouldn't happen here
            gz_buf_size = Z_DEFAULT_BUFSIZE
        end
    end
    return GZipStream(name, gz_file, gz_buf_size)
end
gzdopen(fd::Integer, gzmode::String, gz_buf_size::Integer) = gzdopen(string("<fd ",fd,">"), fd, gzmode, gz_buf_size)
gzdopen(fd::Integer, gz_buf_size::Integer) = gzdopen(fd, "rb", gz_buf_size)
gzdopen(fd::Integer, gzmode::String) = gzdopen(fd, gzmode, Z_DEFAULT_BUFSIZE)
gzdopen(fd::Integer) = gzdopen(fd, "rb", Z_DEFAULT_BUFSIZE)
gzdopen(s::IOStream, args...) = gzdopen(fd(s), args...)


fd(s::GZipStream) = error("fd is not supported for GZipStreams")

function close(s::GZipStream)
    if s._closed
        return Z_STREAM_ERROR
    end

    ret = (@test_z_ok ccall(dlsym(_zlib, :gzclose), Int32, (Ptr{Void},), s.gz_file))
    s._closed = true
    s.name *= " (closed)"

    return ret
end

flush(s::GZipStream, fl::Integer) =
    @test_z_ok ccall(dlsym(_zlib, :gzflush), Int32, (Ptr{Void}, Int32), s.gz_file, int32(fl))
flush(s::GZipStream) = flush(s, Z_SYNC_FLUSH)

truncate(s::GZipStream, n::Integer) = error("truncate is not supported for GZipStreams")

# Note: seeks to byte position within uncompressed data stream
seek(s::GZipStream, n::Integer) =
    (ccall(dlsym(_zlib, :gzseek), FileOffset, (Ptr{Void}, FileOffset, Int32),
           s.gz_file, n, SEEK_SET)!=-1 || # Mimick behavior of seek(s::IOStream, n)
    error("seek (gzseek) failed"))

seek_end(s::GZipStream) = error("seek_end is not supported for GZipStreams")

# Note: skips bytes within uncompressed data stream
skip(s::GZipStream, n::Integer) =
    (ccall(dlsym(_zlib, :gzseek), FileOffset, (Ptr{Void}, FileOffset, Int32),
           s.gz_file, n, SEEK_CUR)!=-1 ||
     error("skip (gzseek) failed")) # Mimick behavior of skip(s::IOStream, n)

position(s::GZipStream) =
    ccall(dlsym(_zlib, :gztell), FileOffset, (Ptr{Void},), s.gz_file)

eof(s::GZipStream) = bool(ccall(dlsym(_zlib, :gzeof), Int32, (Ptr{Void},), s.gz_file))

# Mimics read(s::IOStream, a::Array{T})
function read{T<:Union(Int8,Uint8,Int16,Uint16,Int32,Uint32,Int64,Uint64,
                       Int128,Uint128,Float32,Float64,Complex64,Complex128)}(s::GZipStream, a::Array{T})
    nb = numel(a)*sizeof(T)
    # Note: this will overflow and succeed without warning if nb > 4GB
    ret = ccall(dlsym(_zlib, :gzread), Int32,
                (Ptr{Void}, Ptr{Void}, Uint32), s.gz_file, a, nb)
    if ret == -1
        throw(GZError(s))
    end
    if ret < nb
        throw(EOFError())  # TODO: Do we have/need a way to read without throwing an error near the end of the file?
    end
    a
end

function read(s::GZipStream, ::Type{Uint8})
    ret = gzgetc(s)
    if ret == -1
        throw(GZError(s))
    end
    uint8(ret)
end


# For this function, it's really unfortunate that zlib is
# not integrated with ios
# TODO: are we coping the buffer on return?  If so, figure out how not to.
function readall(s::GZipStream, bufsize::Int)
    buf = Array(Uint8, bufsize)
    len = 0
    while true
        ret = gzread(s, pointer(buf)+len, bufsize)
        if ret == 0
            ## *** Disabled, to allow the function to return the buffer ***
            ## *** Truncation error will be generated on gzclose... ***

            # check error status to make sure stream was not truncated
            # (we won't normally get an error until the close, because it's
            # possible that the file is still being written to.)

            #(err, msg) = gzerror(s)
            #if err != Z_OK
            #    throw(GZError(err, msg))
            #end

            # Resize buffer to exact length
            if length(buf) > len
                grow(buf, len-length(buf))
            end
            return bytestring(buf)
        end
        len += ret
        # Grow the buffer so that bufsize bytes will fit
        grow(buf, bufsize-(length(buf)-len))
    end
end
readall(s::GZipStream) = readall(s, Z_BIG_BUFSIZE)

# TODO: Create a c-wrapper based on gzreadline
function readuntil(s::GZipStream, delim)
    if delim == '\n'
        return readline(s)
    else
        buf = memio(GZ_LINE_BUFSIZE, false)
        c = read(s, Char)
        print(buf, c)
        while c != delim && !eof(s)
            try
                c = read(s, Char)
                print(buf, c)
            catch e
                if !isa(e, EOFError)
                    throw(e)
                end
            end
        end
        # Force eof to be set...
        try
            c = gzgetc(s)
            gzungetc(c, s)
        catch e
            if !isa(e, EOFError)
                throw(e)
            end
        end
        takebuf_string(buf)
    end
end


function readline(s::GZipStream)
    buf = Array(Uint8, GZ_LINE_BUFSIZE)
    pos = 1

    if gzgets(s, buf) == C_NULL      # Throws an exception on error
        return ""
    end

    while(true)
        # since gzgets didn't return C_NULL, there must be a \0 in the buffer
        eos = memchr(buf, '\0', pos)
        if eos == 1 || buf[eos-1] == '\n'
            return bytestring(buf[1:eos-1])
        end

        # If we're at the end of the file, return the string
        if eof(s)  return bytestring(buf[1:eos-1])  end

        # Otherwise, append to the end of the previous buffer

        # Grow the buffer so that there's room for GZ_LINE_BUFSIZE chars
        add_len = GZ_LINE_BUFSIZE - (length(buf)-eos+1)
        grow(buf, add_len)
        pos = eos

        # Read in the next chunk
        if gzgets(s, pointer(buf)+pos-1, GZ_LINE_BUFSIZE) == C_NULL
            # eof(s); remove extra buffer space
            grow(buf, -GZ_LINE_BUFSIZE)
            return bytestring(buf)
        end
    end
end

write(s::GZipStream, b::Uint8) = gzputc(s, b)

function write{T}(s::GZipStream, a::Array{T})
    if isa(T,BitsKind)
        return gzwrite(s, pointer(a), numel(a)*sizeof(T))
    else
        invoke(write, (Any, Array), s, a)
    end
end

write(s::GZipStream, p::Ptr, nb::Integer) = gzwrite(s, p, nb)

function write{T,N}(s::GZipStream, a::SubArray{T,N,Array})
    if !isa(T,BitsKind) || stride(a,1)!=1
        return invoke(write, (Any, AbstractArray), s, a)
    end
    colsz = size(a,1)*sizeof(T)
    if N==1
        write(s, pointer(a, 1), colsz)
    else
        cartesian_map((idxs...)->write(s, pointer(a, idxs), colsz),
                      tuple(1, size(a)[2:]...))
    end
end

## Text I/O ##

# These versions are equivalent to write(s::IO, c::Char) and
# read(s::IO, ::Type{Char}, except that they convert
# Char -> Integer instead of Char -> Uint8 -> Integer

function write(s::GZipStream, c::Char)
    if c < 0x80
        gzputc(s, c)
        return 1
    elseif c < 0x800
        gzputc(s, ( c >> 6          ) | 0xC0)
        gzputc(s, ( c        & 0x3F ) | 0x80)
        return 2
    elseif c < 0x10000
        gzputc(s, ( c >> 12         ) | 0xE0)
        gzputc(s, ((c >> 6)  & 0x3F ) | 0x80)
        gzputc(s, ( c        & 0x3F ) | 0x80)
        return 3
    elseif c < 0x110000
        gzputc(s, ( c >> 18         ) | 0xF0)
        gzputc(s, ((c >> 12) & 0x3F ) | 0x80)
        gzputc(s, ((c >> 6)  & 0x3F ) | 0x80)
        gzputc(s, ( c        & 0x3F ) | 0x80)
        return 4
    end
    error("write(GZipStream, Char): illegal character $(uint(c))")
end

function read(s::GZipStream, ::Type{Char})
    ch = gzgetc(s)
    if ch < 0x80
        return char(ch)
    end

    # mimic utf8.next function
    trailing = Base._jl_utf8_trailing[ch+1]
    c = uint32(0)
    for j = 1:trailing
        c += ch
        c <<= 6
        ch = gzgetc(s)
    end
    c += ch
    c -= Base._jl_utf8_offset[trailing+1]
    char(c)
end

end # module GZip
