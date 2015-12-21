# This file is a part of Julia. License is MIT: http://julialang.org/license

module StringEncoder
import Base: close, eof, flush, read, readall, write
import Base.Libc: errno, strerror
export StringEncodePipe, StringDecodePipe, encode, decode

## iconv wrappers

const E2BIG = 7
const EINVAL = 22
const EILSEQ = 84

type IConv
    p::Ptr{Void}
end

function iconv_close(cd::IConv)
    cd.p != C_NULL || return
    ret = ccall((:iconv_close, :libc), Cint, (Ptr{Void},), cd.p)
    ret == 0 || error("failed to call iconv_close: error $(errno()) ($(strerror(errno())))")
    cd.p = C_NULL
    nothing
end

function iconv_open(tocode, fromcode)
    p = ccall((:iconv_open, :libc), Ptr{Void}, (Cstring, Cstring), tocode, fromcode)
    if p != Ptr{Void}(-1)
        obj = IConv(p)
        finalizer(obj, iconv_close)
        return obj
    elseif errno() == EINVAL
        error("conversion from $fromcode to $tocode not supported by iconv implementation, check that specified encodings are correct")
    else
       error("iconv_open error $(errno()): $(strerror(errno()))")
    end
end


## StringEncodePipe and StringDecodePipe common functions

const BUFSIZE = 100

type StringEncodePipe{S<:IO} <: IO
    ostream::S
    cd::IConv
    inbuf::Vector{UInt8}
    outbuf::Vector{UInt8}
    inbytesleft::Ref{Csize_t}
    outbytesleft::Ref{Csize_t}
end

type StringDecodePipe{S<:IO} <: IO
    istream::S
    cd::IConv
    inbuf::Vector{UInt8}
    outbuf::Vector{UInt8}
    inbytesleft::Ref{Csize_t}
    outbytesleft::Ref{Csize_t}
    skip::Int
end

function iconv!(cd::IConv, inbuf::Vector{UInt8}, outbuf::Vector{UInt8},
                inbytesleft::Ref{Csize_t}, outbytesleft::Ref{Csize_t})
    inbuf2_orig = pointer(inbuf, 1)
    outbuf2_orig = pointer(outbuf, 1)

    inbytesleft_orig = inbytesleft[]
    outbytesleft[] = BUFSIZE

    inbuf2 = Ptr{UInt8}[inbuf2_orig]
    outbuf2 = Ptr{UInt8}[outbuf2_orig]

    ret = ccall((:iconv, :libc), Csize_t,
                (Ptr{Void}, Ptr{Ptr{UInt8}}, Ref{Csize_t}, Ptr{Ptr{UInt8}}, Ref{Csize_t}),
                cd.p, pointer(inbuf2, 1), inbytesleft, pointer(outbuf2, 1), outbytesleft)

    if ret == reinterpret(Csize_t, -1)
        err = errno()

        # Should never happen unless a very small buffer is used
        if err == E2BIG && outbytesleft[] == BUFSIZE
            error("iconv error: ran out of space in the output buffer")
        # Output buffer is full, or sequence is incomplete:
        # copy remaining bytes to the start of the input buffer for next time
        elseif err == E2BIG || err == EINVAL
            copy!(inbuf, 1, inbuf, inbytesleft_orig-inbytesleft[]+1, inbytesleft[])
        elseif err == EILSEQ
            b = inbuf[(inbytesleft_orig-inbytesleft[]+1):inbytesleft_orig]
            error("iconv error: byte sequence 0x$(bytes2hex(b)) is invalid in source encoding or cannot be represented in target encoding")
        else
            error("iconv error $(errno()): $(strerror(errno()))")
        end
    end

    BUFSIZE - outbytesleft[]
end

# Reset iconv to initial state
# Returns the number of bytes written into the output buffer, if any
function iconv_reset!(s::Union{StringEncodePipe, StringDecodePipe})
    s.cd.p != C_NULL || return 0

    if is(s, StringDecodePipe)
        s.skip = 0
    end

    outbuf2 = Ptr{UInt8}[pointer(s.outbuf, 1)]
    s.outbytesleft[] = BUFSIZE
    ret = ccall((:iconv, :libc), Csize_t,
                (Ptr{Void}, Ptr{Ptr{UInt8}}, Ref{Csize_t}, Ptr{Ptr{UInt8}}, Ref{Csize_t}),
                s.cd.p, C_NULL, C_NULL, pointer(outbuf2, 1), s.outbytesleft)

    if ret == reinterpret(Csize_t, -1)
        err = errno()
        if err == EINVAL
            error("iconv error: incomplete byte sequence at end of input")
        elseif err == E2BIG
            error("iconv error: ran out of space in the output buffer")
        elseif err == EILSEQ
            error("iconv error: invalid byte sequence in input")
        else
            error("iconv error $(errno()): $(strerror(errno()))")
        end
    end

    BUFSIZE - s.outbytesleft[]
end


## StringEncodePipe

"""
  StringEncodePipe(istream, to, from="UTF-8")

Returns a new write-only I/O stream, which converts any text in the encoding `from`
written to it into text in the encoding `to` written to ostream. Calling `close` on the
stream is necessary to complete the encoding (but does not close `ostream`).
"""
function StringEncodePipe(ostream::IO, to::ASCIIString, from::ASCIIString="UTF-8")
    cd = iconv_open(to, from)
    inbuf = Vector{UInt8}(BUFSIZE)
    outbuf = Vector{UInt8}(BUFSIZE)
    s = StringEncodePipe(ostream, cd, inbuf, outbuf, Ref{Csize_t}(0), Ref{Csize_t}(BUFSIZE))
    finalizer(s, close)
    s
end

# Flush input buffer and convert it into output buffer
# Returns the number of bytes written to output buffer
function flush(s::StringEncodePipe)
    s.cd.p != C_NULL || return s

    # We need to retry several times in case output buffer is too small to convert
    # all of the input. Even so, some incomplete sequences may remain in the input
    # until more data is written, which will only trigger an error on close().
    s.outbytesleft[] = 0
    while s.outbytesleft[] < BUFSIZE
        iconv!(s.cd, s.inbuf, s.outbuf, s.inbytesleft, s.outbytesleft)
        write(s.ostream, sub(s.outbuf, 1:(BUFSIZE - s.outbytesleft[])))
    end

    s
end

function close(s::StringEncodePipe)
    s.cd.p != C_NULL || return s
    flush(s)
    iconv_reset!(s)
    finalize(s.cd)
    # flush() wasn't able to empty input buffer, which cannot happen with correct data
    s.inbytesleft[] == 0 || error("iconv error: incomplete byte sequence at end of input")
end

function write(s::StringEncodePipe, x::UInt8)
    s.inbytesleft[] >= length(s.inbuf) && flush(s)
    s.inbuf[s.inbytesleft[]+=1] = x
    1
end


## StringDecodePipe

"""
  StringDecodePipe(istream, from, to="UTF-8")

Returns a new read-only I/O stream, which converts text in the encoding `from`
read from `istream` into text in the encoding `to`.
"""
function StringDecodePipe(istream::IO, from::ASCIIString, to::ASCIIString="UTF-8")
    cd = iconv_open(to, from)
    inbuf = Vector{UInt8}(BUFSIZE)
    outbuf = Vector{UInt8}(BUFSIZE)
    s = StringDecodePipe(istream, cd, inbuf, outbuf, Ref{Csize_t}(0), Ref{Csize_t}(BUFSIZE), 0)
    finalizer(s, close)
    s
end

# Fill input buffer and convert it into output buffer
# Returns the number of bytes written to output buffer
function fill_buffer!(s::StringDecodePipe)
    s.cd.p != C_NULL || return 0

    s.skip = 0

    # Input buffer and input stream empty
    if s.inbytesleft[] == 0 && eof(s.istream)
        i = iconv_reset!(s)
        return i
    end

    s.inbytesleft[] += readbytes!(s.istream, sub(s.inbuf, (s.inbytesleft[]+1):BUFSIZE))
    iconv!(s.cd, s.inbuf, s.outbuf, s.inbytesleft, s.outbytesleft)
end

# In order to know whether more data is available, we need to:
# 1) check whether the output buffer contains data
# 2) if not, actually try to fill it (this is the only way to find out whether input
#    data contains only state control sequences which may be converted to nothing)
# 3) reset iconv to initial state, which may generate data
function eof(s::StringDecodePipe)
    length(s.outbuf) - s.outbytesleft[] == s.skip &&
        fill_buffer!(s) == 0 &&
        iconv_reset!(s) == 0
end

function close(s::StringDecodePipe)
    s.cd.p != C_NULL || return s
    finalize(s.cd)
    # fill_buffer!() wasn't able to empty input buffer, which cannot happen with correct data
    s.inbytesleft[] == 0 || error("iconv error: incomplete byte sequence at end of input")
end

function read(s::StringDecodePipe, ::Type{UInt8})
    eof(s) ? throw(EOFError()) : s.outbuf[s.skip+=1]
end

function readall(filename::AbstractString, encoding::ASCIIString)
    open(s -> readall(StringDecodePipe(s, encoding)), filename)
end


## Functions to encode/decode strings

encoding_string(::Type{ASCIIString}) = "ASCII"
encoding_string(::Type{UTF8String}) = "UTF-8"
encoding_string(::Type{UTF16String}) = "UTF-16"
encoding_string(::Type{UTF32String}) = "UTF-32"

"""
    decode(a::Vector{UInt8}, enc::ASCIIString)

Convert an array of bytes `a` representing text in encoding `enc` to a string.
"""
function decode(a::Vector{UInt8}, enc::ASCIIString)
    b = IOBuffer(a)
    try
        d = readbytes(StringDecodePipe(b, enc, "UTF-8"))
        # Skip final null bytes if needed
        # FIXME: find a better solution?
        i = length(d)
        while i >= 1
            d[i] != 0 && break
            i -= 1
        end
        UTF8String(d[1:i])
    finally
        close(b)
    end
end

"""
    encode(s::AbstractString, enc::ASCIIString)

Convert string `s` to an array of bytes representing text in encoding `enc`.
"""
function encode(s::AbstractString, enc::ASCIIString)
    b = IOBuffer()
    p = StringEncodePipe(b, enc, encoding_string(typeof(s)))
    write(p, s)
    close(p)
    takebuf_array(b)
end

end # module
