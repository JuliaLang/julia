## time-related functions ##

# TODO: check for usleep errors?
@unix_only systemsleep(s::Real) = ccall(:usleep, Int32, (Uint32,), uint32(iround(s*1e6)))
@windows_only systemsleep(s::Real) = (ccall(:Sleep, stdcall, Void, (Uint32,), uint32(iround(s*1e3))); return int32(0))

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
        ccall(:localtime_r, Ptr{Void}, (Ptr{Int}, Ptr{Void}), &t, &tm)
        return tm
    end
end

strftime(t) = strftime("%c", t)
strftime(fmt::ByteString, t::Real) = strftime(fmt, TmStruct(t))
function strftime(fmt::ByteString, tm::TmStruct)
    timestr = Array(Uint8, 128)
    n = ccall(:strftime, Int, (Ptr{Uint8}, Int, Ptr{Uint8}, Ptr{Void}),
              timestr, length(timestr), fmt, &tm)
    if n == 0
        return ""
    end
    bytestring(convert(Ptr{Uint8},timestr))
end

strptime(timestr::ByteString) = strptime("%c", timestr)
function strptime(fmt::ByteString, timestr::ByteString)
    tm = TmStruct()
    r = ccall(:strptime, Ptr{Uint8}, (Ptr{Uint8}, Ptr{Uint8}, Ptr{Void}),
              timestr, fmt, &tm)
    # the following would tell mktime() that this is a local time, and that
    # it should try to guess the timezone. not sure if/how this should be
    # exposed in the API.
    # tm.isdst = -1
    if r == C_NULL
        error("strptime: invalid arguments")
    end
    tm
end

time(tm::TmStruct) = float64(ccall(:mktime, Int, (Ptr{Void},), &tm))

## process-related functions ##

@windows_only getpid() = ccall(:GetCurrentProcessId, stdcall, Uint32, ())
@unix_only getpid() = ccall(:getpid, Int32, ())

## network functions ##

function gethostname()
    hn = Array(Uint8, 256)
    @unix_only err=ccall(:gethostname, Int32, (Ptr{Uint8}, Uint), hn, length(hn))
    @windows_only err=ccall(:gethostname, stdcall, Int32, (Ptr{Uint8}, Uint32), hn, length(hn))
    if err != 0
        error("gethostname")
    end
    bytestring(convert(Ptr{Uint8},hn))
end

function getipaddr()
    ip = Array(Uint8, 128)
    ccall(:getlocalip, Void, (Ptr{Uint8}, Uint), ip, length(ip))
    bytestring(convert(Ptr{Uint8},ip))
end

## Memory related ##

c_free(p::Ptr) = ccall(:free, Void, (Ptr{Void},), p)
c_malloc(size::Integer) = ccall(:malloc, Ptr{Void}, (Csize_t,), size)
