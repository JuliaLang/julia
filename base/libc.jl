## time-related functions ##

# TODO: check for usleep errors?
sleep(s::Real) = ccall(:usleep, Int32, (Uint32,), uint32(iround(s*1e6)))

strftime(t) = strftime("%c", t)
function strftime(fmt::ByteString, t)
    tmstruct = Array(Int32, 14)
    ccall(:localtime_r, Ptr{Void}, (Ptr{Int}, Ptr{Int32}), &t, tmstruct)
    timestr = Array(Uint8, 128)
    n = ccall(:strftime, Int, (Ptr{Uint8}, Int, Ptr{Uint8}, Ptr{Int32}),
              timestr, length(timestr), fmt, tmstruct)
    if n == 0
        return ""
    end
    bytestring(convert(Ptr{Uint8},timestr))
end

strptime(timestr::ByteString) = strptime("%c", timestr)
function strptime(fmt::ByteString, timestr::ByteString)
    tmstruct = Array(Int32, 14)
    r = ccall(:strptime, Ptr{Uint8}, (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}),
              timestr, fmt, tmstruct)
    if r == C_NULL
        error("strptime: invalid arguments")
    end
    float64(ccall(:mktime, Int, (Ptr{Int32},), tmstruct))
end

## process-related functions ##

getpid() = int(ccall(:getpid, Int32, ()))
system(cmd::String) = int(ccall(:system, Int32, (Ptr{Uint8},), cmd))

## network functions ##

function gethostname()
    hn = Array(Uint8, 128)
    ccall(:gethostname, Int32, (Ptr{Uint8}, Uint), hn, length(hn))
    bytestring(convert(Ptr{Uint8},hn))
end

function getipaddr()
    ip = Array(Uint8, 128)
    ccall(:getlocalip, Void, (Ptr{Uint8}, Uint), ip, length(ip))
    bytestring(convert(Ptr{Uint8},ip))
end

## get a temporary file name ##

tmpnam() = bytestring(ccall(:tmpnam, Ptr{Uint8}, (Ptr{Uint8},), C_NULL))

## Memory related ##

c_free(p::Ptr) = ccall(:free, Void, (Ptr{Void},), p)
