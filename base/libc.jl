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
        error("invalid arguments")
    end
    tm
end

time(tm::TmStruct) = float64(ccall(:mktime, Int, (Ptr{Void},), &tm))

## process-related functions ##

getpid() = ccall(:jl_getpid, Int32, ())


# provides Julia functions to manipulate process resources provided by libc and
# as specified in getrlimit(2) and setrlimit(2)

@linux_only immutable Resources
    RLIMIT_CPU::Int # 0 cpu time per process
    RLIMIT_FSIZE::Int # 1 largest file size in bytes
    RLIMIT_DATA::Int # 2 data segment size in bytes
    RLIMIT_STACK::Int # 3 stack size in bytes
    RLIMIT_CORE::Int # 4 core file size in bytes
    RLIMIT_RSS::Int # 5 resident set size affects swapping processes that
                    # are exceeding their resident set size will be more
                    # likely to have physical memory taken from them
    RLIMIT_NPROC::Int # 6 number of processes
    RLIMIT_NOFILE::Int # 7 number of open files
    RLIMIT_MEMLOCK::Int # 8 locked-in-memory address space
    RLIMIT_AS::Int # 9 address space in bytes
    RLIMIT_LOCKS::Int # 10 Maximum number of file locks
    RLIMIT_SIGPENDING::Int # 11 Maximum number of pending signals
    RLIMIT_MSGQUEUE::Int # 12 Maximum bytes in POSIX message queues
    RLIMIT_NICE::Int # 13 Maximum nice priority allowed to raise to.
                     # Nice levels 19 .. -20 correspond to 0 .. 39
                     # values of this resource limit
    RLIMIT_RTPRIO::Int # 14 Maximum realtime priority allowed for
                        # non-priviledged processes
    RLIMIT_RTTIME::Int # 15 Maximum CPU time in µs that a process scheduled
                       # under a real-time scheduling policy may consume without
                       # making a blocking system call before being forcibly descheduled
    RLIM_NLIMITS::Int # 16 total number of resource limits
    Resources() = new(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
end

@osx_only immutable Resources
    RLIMIT_CPU::Int # 0 cpu time per process
    RLIMIT_FSIZE::Int # 1 file size
    RLIMIT_DATA::Int # 2 data segment size
    RLIMIT_STACK::Int # 3 stack size
    RLIMIT_CORE::Int # 4 core file size
    RLIMIT_AS::Int # 5 address space
    RLIMIT_RSS::Int # 5 resident set size
    RLIMIT_MEMLOCK::Int # 6 locked-in-memory address space
    RLIMIT_NPROC::Int # 7 number of processes
    RLIMIT_NOFILE::Int # 8 number of open files
    RLIM_NLIMITS::Int # 9 total number of resource limits
    Resources() = new(0,1,2,3,4,5,5,6,7,8,9)
end

type RLimit
    rlim_cur::Clong
    rlim_max::Clong
    RLimit() = new(0,0)
    RLimit(cur,max) = new(cur,max)
end

# perhaps an exception might be better
function verify_resource(resource::Int)
    if !in(resource,0:Resources().RLIM_NLIMITS)
        err = names(Resources) |>
              _->_[1:end-1] |>
              _->enumerate(_) |>
              _->[string(i[2]," = ",i[1] - 1) for i in _]
        error("Invalid resource specified: should be one of the following:\n$err")
    end
end

function getrlimit(resource::Int)
    verify_resource(resource)
    rlim = RLimit()
    rc = ccall(:getrlimit, Int, (Int, Ptr{RLimit}), resource, &rlim)
    if rc == 0
        rlim
    else
        error("failed with errorcode:",rc)
    end
end

function setrlimit(resource::Int,settings_tup::(Int,Int))
    verify_resource(resource)
    if !isa(settings_tup,(Int,Int))
        error("Expected a tuple of two integers (current,maximum)")
    end
    rlim = RLimit(settings_tup[1],settings_tup[2])
    rc = ccall(:setrlimit, Int, (Int, Ptr{RLimit}), resource, &rlim)
    if rc == 0
        rlim
    else
        error("failed with errorcode:",rc)
    end
end

@linux_only prlimit(pid::Int32, resource::Int) = prlimit(pid, resource, (nothing, nothing))
@linux_only function prlimit(pid::Int32, resource::Int, limits_tup)
    verify_resource(resource)
    rlim_old = RLimit()
    if isa(limits_tup,(Nothing,Nothing))
        rc = ccall(:prlimit, Int, (Int, Int, Ptr{RLimit}, Ptr{RLimit}),
                                   pid, resource, C_NULL, &rlim_old)
    elseif isa(limits_tup,(Int,Int))
        rlim_new = RLimit(limits_tup[1],limits_tup[2])
        rc = ccall(:prlimit, Int, (Int, Int, Ptr{RLimit}, Ptr{RLimit}),
                                   pid, resource, &rlim_new, &rlim_old)
    else
        error("Expected a tuple of two integers (current,maximum)")
    end

    if rc == 0
        (rlim_old.rlim_cur,rlim_old.rlim_max)
    else
        error("failed with errorcode:",rc)
    end
end

# override default show for RLimit type
import Base.show
show(io::IO, limit::RLimit) = print(io, string("Resource Limits(Current: ",
                                    limit.rlim_cur == typemax(Clong) ?
                                        "unlimited" : limit.rlim_cur,
                                    ", Maximum: ",
                                    limit.rlim_max == typemax(Clong) ?
                                        "unlimited" : limit.rlim_max,")"))


## network functions ##

function gethostname()
    hn = Array(Uint8, 256)
    @unix_only err=ccall(:gethostname, Int32, (Ptr{Uint8}, Uint), hn, length(hn))
    @windows_only err=ccall(:gethostname, stdcall, Int32, (Ptr{Uint8}, Uint32), hn, length(hn))
    systemerror("gethostname", err != 0)
    bytestring(convert(Ptr{Uint8},hn))
end

## Memory related ##

c_free(p::Ptr) = ccall(:free, Void, (Ptr{Void},), p)
c_malloc(size::Integer) = ccall(:malloc, Ptr{Void}, (Csize_t,), size)
c_realloc(p::Ptr, size::Integer) = ccall(:realloc, Ptr{Void}, (Ptr{Void}, Csize_t), p, size)
c_calloc(num::Integer, size::Integer) = ccall(:calloc, Ptr{Void}, (Csize_t, Csize_t), num, size)
