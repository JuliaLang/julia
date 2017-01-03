
"""
    strftime([format], time)
Convert time, given as a number of seconds since the epoch or a `HumanTime`, to a formatted
string using the given format. Supported formats are the same as those in the standard C
library.
"""
strftime(t) = strftime("%c", t)
strftime(fmt::AbstractString, t::Real) = strftime(fmt, HumanTime(t))
function strftime(fmt::AbstractString, tm::HumanTime)
    timestr = Array{UInt8}(128)
    n = ccall(:strftime, Int, (Ptr{UInt8}, Int, Cstring, Ptr{HumanTime}),
              timestr, length(timestr), fmt, &tm)
    if n == 0
        return ""
    end
    return String(timestr[1:n])
end

"""
    strptime([format], timestr)
Parse a formatted time string into a `HumanTime` giving the seconds, minute, hour, date, etc.
Supported formats are the same as those in the standard C library. On some platforms,
timezones will not be parsed correctly. If the result of this function will be passed to
`time` to convert it to seconds since the epoch, the `isdst` field should be filled in
manually. Setting it to `-1` will tell the C library to use the current system settings to
determine the timezone.
"""
strptime(timestr::AbstractString) = strptime("%c", timestr)
function strptime(fmt::AbstractString, timestr::AbstractString)
    tm = HumanTime()
    r = ccall(:strptime, Cstring, (Cstring, Cstring, Ptr{HumanTime}),
              timestr, fmt, &tm)
    # the following would tell mktime() that this is a local time, and that
    # it should try to guess the timezone. not sure if/how this should be
    # exposed in the API.
    # tm.isdst = -1
    if r == C_NULL
        # TODO: better error message
        throw(ArgumentError("invalid arguments"))
    end
    @static if is_apple()
        # if we didn't explicitly parse the weekday or year day, use mktime
        # to fill them in automatically.
        if !ismatch(r"([^%]|^)%(a|A|j|w|Ow)", fmt)
            ccall(:mktime, Int, (Ptr{HumanTime},), &tm)
        end
    end
    return tm
end
