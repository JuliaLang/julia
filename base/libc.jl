module Libc

export FILE, TmStruct, strftime, strptime, getpid, gethostname, free, malloc, calloc, realloc,
    errno, strerror, flush_cstdio, systemsleep, time,
    MS_ASYNC, MS_INVALIDATE, MS_SYNC, mmap, munmap, msync

include("errno.jl")

## FILE ##

immutable FILE
    ptr::Ptr{Void}
end

modestr(s::IO) = modestr(isreadable(s), iswritable(s))
modestr(r::Bool, w::Bool) = r ? (w ? "r+" : "r") : (w ? "w" : throw(ArgumentError("neither readable nor writable")))

function FILE(s::IO)
    @unix_only FILEp = ccall(:fdopen, Ptr{Void}, (Cint, Ptr{UInt8}), convert(Cint, fd(s)), modestr(s))
    @windows_only FILEp = ccall(:_fdopen, Ptr{Void}, (Cint, Ptr{UInt8}), convert(Cint, fd(s)), modestr(s))
    systemerror("fdopen", FILEp == C_NULL)
    seek(FILE(FILEp), position(s))
end

Base.convert(::Type{FILE}, s::IO) = FILE(s)

function Base.seek(h::FILE, offset::Integer)
    systemerror("fseek", ccall(:fseek, Cint, (Ptr{Void}, Clong, Cint),
                               h.ptr, offset, 0) != 0)
    h
end

Base.position(h::FILE) = ccall(:ftell, Clong, (Ptr{Void},), h.ptr)

# flush C stdio output from external libraries
flush_cstdio() = ccall(:jl_flush_cstdio, Void, ())

## time-related functions ##

# TODO: check for usleep errors?
@unix_only systemsleep(s::Real) = ccall(:usleep, Int32, (UInt32,), round(UInt32,s*1e6))
@windows_only systemsleep(s::Real) = (ccall(:Sleep, stdcall, Void, (UInt32,), round(UInt32,s*1e3)); return Int32(0))

type TmStruct
    sec::Int32
    min::Int32
    hour::Int32
    mday::Int32
    month::Int32
    year::Int32
    wday::Int32
    yday::Int32
    isdst::Int32
    # on some platforms the struct is 14 words, even though 9 are specified
    _10::Int32
    _11::Int32
    _12::Int32
    _13::Int32
    _14::Int32

    TmStruct(sec, min, hour, mday, month, year, wday, yday, isdst) =
        new(sec, min, hour, mday, month, year, wday, yday, isdst, 0,0,0,0,0)
    TmStruct() = new(0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    function TmStruct(t::Real)
        t = floor(t)
        tm = TmStruct()
        # TODO: add support for UTC via gmtime_r()
        ccall(:localtime_r, Ptr{TmStruct}, (Ptr{Int}, Ptr{TmStruct}), &t, &tm)
        return tm
    end
end

strftime(t) = strftime("%c", t)
strftime(fmt::AbstractString, t::Real) = strftime(fmt, TmStruct(t))
function strftime(fmt::AbstractString, tm::TmStruct)
    timestr = Array(UInt8, 128)
    n = ccall(:strftime, Int, (Ptr{UInt8}, Int, Ptr{UInt8}, Ptr{TmStruct}),
              timestr, length(timestr), fmt, &tm)
    if n == 0
        return ""
    end
    bytestring(pointer(timestr), n)
end

strptime(timestr::AbstractString) = strptime("%c", timestr)
function strptime(fmt::AbstractString, timestr::AbstractString)
    tm = TmStruct()
    r = ccall(:strptime, Ptr{UInt8}, (Ptr{UInt8}, Ptr{UInt8}, Ptr{TmStruct}),
              timestr, fmt, &tm)
    # the following would tell mktime() that this is a local time, and that
    # it should try to guess the timezone. not sure if/how this should be
    # exposed in the API.
    # tm.isdst = -1
    if r == C_NULL
        #TODO: better error message
        throw(ArgumentError("invalid arguments"))
    end
    @osx_only begin
        # if we didn't explicitly parse the weekday or year day, use mktime
        # to fill them in automatically.
        if !ismatch(r"([^%]|^)%(a|A|j|w|Ow)", fmt)
            ccall(:mktime, Int, (Ptr{TmStruct},), &tm)
        end
    end
    tm
end

# system date in seconds
time(tm::TmStruct) = Float64(ccall(:mktime, Int, (Ptr{TmStruct},), &tm))
time() = ccall(:clock_now, Float64, ())

## process-related functions ##

getpid() = ccall(:jl_getpid, Int32, ())

## network functions ##

function gethostname()
    hn = Array(UInt8, 256)
    @unix_only err=ccall(:gethostname, Int32, (Ptr{UInt8}, UInt), hn, length(hn))
    @windows_only err=ccall(:gethostname, stdcall, Int32, (Ptr{UInt8}, UInt32), hn, length(hn))
    systemerror("gethostname", err != 0)
    bytestring(pointer(hn))
end

## system error handling ##

errno() = ccall(:jl_errno, Cint, ())
errno(e::Integer) = ccall(:jl_set_errno, Void, (Cint,), e)
strerror(e::Integer) = bytestring(ccall(:strerror, Ptr{UInt8}, (Int32,), e))
strerror() = strerror(errno())

## Memory related ##

free(p::Ptr) = ccall(:free, Void, (Ptr{Void},), p)
malloc(size::Integer) = ccall(:malloc, Ptr{Void}, (Csize_t,), size)
realloc(p::Ptr, size::Integer) = ccall(:realloc, Ptr{Void}, (Ptr{Void}, Csize_t), p, size)
calloc(num::Integer, size::Integer) = ccall(:calloc, Ptr{Void}, (Csize_t, Csize_t), num, size)

## mmap ##

msync{T}(A::Array{T}) = msync(pointer(A), length(A)*sizeof(T))

msync(B::BitArray) = msync(pointer(B.chunks), length(B.chunks)*sizeof(UInt64))

@unix_only begin
# Low-level routines
# These are needed for things like MAP_ANONYMOUS
function mmap(len::Integer, prot::Integer, flags::Integer, fd, offset::Integer)
    const pagesize::Int = ccall(:jl_getpagesize, Clong, ())
    # Check that none of the computations will overflow
    if len < 0
        throw(ArgumentError("requested size must be ≥ 0, got $len"))
    end
    if len > typemax(Int)-pagesize
        throw(ArgumentError("requested size must be ≤ $(typemax(Int)-pagesize), got $len"))
    end
    # Set the offset to a page boundary
    offset_page::FileOffset = floor(Integer,offset/pagesize)*pagesize
    len_page::Int = (offset-offset_page) + len
    # Mmap the file
    p = ccall(:jl_mmap, Ptr{Void}, (Ptr{Void}, Csize_t, Cint, Cint, Cint, FileOffset), C_NULL, len_page, prot, flags, fd, offset_page)
    systemerror("memory mapping failed", reinterpret(Int,p) == -1)
    # Also return a pointer that compensates for any adjustment in the offset
    return p, Int(offset-offset_page)
end

function munmap(p::Ptr,len::Integer)
    systemerror("munmap", ccall(:munmap,Cint,(Ptr{Void},Int),p,len) != 0)
end

const MS_ASYNC = 1
const MS_INVALIDATE = 2
const MS_SYNC = 4
function msync(p::Ptr, len::Integer, flags::Integer)
    systemerror("msync", ccall(:msync, Cint, (Ptr{Void}, Csize_t, Cint), p, len, flags) != 0)
end
msync(p::Ptr, len::Integer) = msync(p, len, MS_SYNC)
end


@windows_only begin
function munmap(viewhandle::Ptr, mmaphandle::Ptr)
    status = Bool(ccall(:UnmapViewOfFile, stdcall, Cint, (Ptr{Void},), viewhandle))
    status |= Bool(ccall(:CloseHandle, stdcall, Cint, (Ptr{Void},), mmaphandle))
    if !status
        error("could not unmap view: $(FormatMessage())")
    end
end

function msync(p::Ptr, len::Integer)
    status = Bool(ccall(:FlushViewOfFile, stdcall, Cint, (Ptr{Void}, Csize_t), p, len))
    if !status
        error("could not msync: $(FormatMessage())")
    end
end

end

end # module
