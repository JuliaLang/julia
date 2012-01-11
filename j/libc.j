# libc = dlopen("libc")

## time-related functions ##

sleep(s::Real) = ccall(dlsym(libc, :usleep), Uint32, (Uint32,), uint32(iround(s*1e6)))

strftime(t) = strftime("%c", t)
function strftime(fmt::ByteString, t)
    tmstruct = Array(Int32, 13)
    ccall(:localtime_r, Ptr{Void}, (Ptr{Int}, Ptr{Int32}), int(t), tmstruct)
    timestr = Array(Uint8, 128)
    n = ccall(:strftime, Int, (Ptr{Uint8}, Int, Ptr{Uint8}, Ptr{Int32}),
              timestr, length(timestr), fmt, tmstruct)
    if n == 0
        return ""
    end
    cstring(convert(Ptr{Uint8},timestr))
end

strptime(timestr::ByteString) = strptime("%c", timestr)
function strptime(fmt::ByteString, timestr::ByteString)
    tmstruct = Array(Int32, 13)
    r = ccall(:strptime, Ptr{Uint8}, (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}),
              timestr, fmt, tmstruct)
    if r == C_NULL
        error("strptime: invalid arguments")
    end
    float64(ccall(:mktime, Int, (Ptr{Int32},), tmstruct))
end

## process-related functions ##

getpid() = ccall(dlsym(libc, :getpid), Uint32, ())
system(cmd::String) = ccall(dlsym(libc, :system), Int32, (Ptr{Uint8},), cstring(cmd))

## network functions ##

function gethostname()
    hn = Array(Uint8, 128)
    ccall(dlsym(libc,:gethostname), Int32, (Ptr{Uint8}, Uint),
          hn, uint(length(hn)))
    cstring(convert(Ptr{Uint8},hn))
end

function getipaddr()
    ip = Array(Uint8, 128)
    ccall(:getlocalip, Void, (Ptr{Uint8}, Uint),
          ip, uint(length(ip)))
    cstring(convert(Ptr{Uint8},ip))
end

## file and directory ##

function getcwd()
    b = Array(Uint8,1024)
    p = ccall(dlsym(libc, :getcwd), Ptr{Uint8}, (Ptr{Uint8}, Uint),
              b, uint(length(b)))
    if p == C_NULL
        error("path too long")
    end
    cstring(p)
end

function setcwd(p::String)
    if ccall(dlsym(libc, :chdir), Int32, (Ptr{Uint8},), p) == -1
        throw(SystemError("setcwd"))
    end
    getcwd()
end

## Memory related ##

_jl_free(p::Ptr{Void}) = ccall(dlsym(libc, :free), Void, (Ptr{Void},), p)
