#
# ICU - International Components for Unicode
# 
# Example:
#
#   import ICU.*
#   uppercase("testingß")  # "TESTINGSS"
#   set_locale("tr")       # set locale to Turkish
#   uppercase("testingß")  # "TESTİNGSS"
#
# Note that "ß" gets converted to "SS" in the first call to uppercase,
# and "i" gets converted to "İ" (dotted capital I) in the second call
# after the locale is set to Turkish.
#

load("utf16.jl")

module ICU
import Base.*
import UTF16.*

export foldcase,
       lowercase,
       set_locale,
       titlecase,
       uppercase

load("openlib.jl")
const iculib = openlib(OS_NAME == :Darwin ? "libicucore" : "libicuuc")
const iculibi18n = OS_NAME == :Darwin ? iculib : openlib("libicui18n")

for suffix in ["", ["_"*string(i) for i in 42:50]]
    if dlsym(iculib, "u_strToUpper"*suffix) != C_NULL
        for f in (:u_strFoldCase,
                  :u_strToLower,
                  :u_strToTitle,
                  :u_strToUpper,
                  :ucal_add,
                  :ucal_clear,
                  :ucal_close,
                  :ucal_get,
                  :ucal_getMillis,
                  :ucal_getNow,
                  :ucal_open,
                  :ucal_setDate,
                  :ucal_setDateTime,
                  :ucal_setMillis,
                  :ucasemap_open,
                  :ucasemap_close,
                  :ucasemap_getBreakIterator,
                  :ucasemap_utf8FoldCase,
                  :ucasemap_utf8ToLower,
                  :ucasemap_utf8ToTitle,
                  :ucasemap_utf8ToUpper)
            @eval const $f = $(string(f) * suffix)
        end
        break
    end
end

typealias UErrorCode Int32

locale = C_NULL
casemap = C_NULL

function set_locale(s::Union(ByteString,Ptr{None}))
    global casemap
    if casemap != C_NULL
        ccall(dlsym(iculib,ucasemap_close), Void, (Ptr{Void},), casemap)
    end
    err = UErrorCode[0]
    casemap = ccall(dlsym(iculib,ucasemap_open), Ptr{Void},
        (Ptr{Uint8},Int32,Ptr{UErrorCode}), s, 0, err)
    if casemap != C_NULL
        global locale = s
    end
end
set_locale(locale)

for (a,b) in [(:lowercase,:u_strToLower),
              (:uppercase,:u_strToUpper)]
    @eval begin
        function ($a)(s::UTF16String)
            src = s.data
            destsiz = int32(2*numel(src))
            dest = zeros(Uint16, destsiz)
            err = UErrorCode[0]
            n = ccall(dlsym(iculib,$b), Int32,
                (Ptr{Uint16},Int32,Ptr{Uint16},Int32,Ptr{Uint8},Ptr{UErrorCode}),
                dest, destsiz, src, numel(src), locale, err)
            return UTF16String(dest[1:n])
        end
    end
end

function foldcase(s::UTF16String)
    src = s.data
    destsiz = int32(2*numel(src))
    dest = zeros(Uint16, destsiz)
    err = UErrorCode[0]
    n = ccall(dlsym(iculib,u_strFoldCase), Int32,
        (Ptr{Uint16},Int32,Ptr{Uint16},Int32,Uint32,Ptr{UErrorCode}),
        dest, destsiz, src, numel(src), 0, err)
    return UTF16String(dest[1:n])
end

function titlecase(s::UTF16String)
    src = s.data
    destsiz = int32(2*numel(src))
    dest = zeros(Uint16, destsiz)
    err = UErrorCode[0]
    breakiter = ccall(dlsym(iculib,ucasemap_getBreakIterator),
        Ptr{Void}, (Ptr{Void},), casemap)
    n = ccall(dlsym(iculib,u_strToTitle), Int32,
        (Ptr{Uint16},Int32,Ptr{Uint16},Int32,Ptr{Void},Ptr{Uint8},Ptr{UErrorCode}),
        dest, destsiz, src, numel(src), breakiter, locale, err)
    return UTF16String(dest[1:n])
end

for (a,b) in [(:foldcase,:ucasemap_utf8FoldCase),
              (:lowercase,:ucasemap_utf8ToLower),
              (:titlecase,:ucasemap_utf8ToTitle),
              (:uppercase,:ucasemap_utf8ToUpper)]
    @eval begin
        function ($a)(src::UTF8String)
            destsiz = int32(2*numel(src))
            dest = zeros(Uint8, destsiz)
            err = UErrorCode[0]
            n = ccall(dlsym(iculib,$b), Int32,
                (Ptr{Void},Ptr{Uint8},Int32,Ptr{Uint8},Int32,Ptr{UErrorCode}),
                casemap, dest, destsiz, src, -1, err)
            return utf8(dest[1:n])
        end
    end
end

foldcase(s::ASCIIString) = foldcase(utf8(s))
titlecase(s::ASCIIString) = titlecase(utf8(s))

function test_icustring()
    @assert uppercase("testingß") == "TESTINGSS"
    set_locale("tr")       # set locale to Turkish
    @assert uppercase("testingß") == "TESTİNGSS"
end

## calendar ##

export ICUCalendar,
       add,
       get,
       getMillis,
       getNow,
       setDate,
       setDateTime,
       setMillis

export UCAL_ERA,
       UCAL_YEAR,
       UCAL_MONTH,
       UCAL_WEEK_OF_YEAR,
       UCAL_WEEK_OF_MONTH,
       UCAL_DATE,
       UCAL_DAY_OF_YEAR,
       UCAL_DAY_OF_WEEK,
       UCAL_DAY_OF_WEEK_IN_MONTH,
       UCAL_AM_PM,
       UCAL_HOUR,
       UCAL_HOUR_OF_DAY,
       UCAL_MINUTE,
       UCAL_SECOND,
       UCAL_MILLISECOND,
       UCAL_ZONE_OFFSET,
       UCAL_DST_OFFSET,
       UCAL_YEAR_WOY,
       UCAL_DOW_LOCAL,
       UCAL_EXTENDED_YEAR,
       UCAL_JULIAN_DAY,
       UCAL_MILLISECONDS_IN_DAY,
       UCAL_IS_LEAP_MONTH

for (a,i) in enumerate([
        :UCAL_ERA,
        :UCAL_YEAR,
        :UCAL_MONTH,
        :UCAL_WEEK_OF_YEAR,
        :UCAL_WEEK_OF_MONTH,
        :UCAL_DATE,
        :UCAL_DAY_OF_YEAR,
        :UCAL_DAY_OF_WEEK,
        :UCAL_DAY_OF_WEEK_IN_MONTH,
        :UCAL_AM_PM,
        :UCAL_HOUR,
        :UCAL_HOUR_OF_DAY,
        :UCAL_MINUTE,
        :UCAL_SECOND,
        :UCAL_MILLISECOND,
        :UCAL_ZONE_OFFSET,
        :UCAL_DST_OFFSET,
        :UCAL_YEAR_WOY,
        :UCAL_DOW_LOCAL,
        :UCAL_EXTENDED_YEAR,
        :UCAL_JULIAN_DAY,
        :UCAL_MILLISECONDS_IN_DAY,
        :UCAL_IS_LEAP_MONTH
    ])
    @eval const $a = int32($i - 1)
end

typealias UDate Float64

type ICUCalendar
    ptr::Ptr{Void}
    ICUCalendar(p::Ptr) = (self = new(p); finalizer(self, close); self)
end

function ICUCalendar(timezone::String)
    tz_u16 = utf16(timezone)
    err = UErrorCode[0]
    p = ccall(dlsym(iculibi18n,ucal_open), Ptr{Void},
        (Ptr{Uint16},Int32,Ptr{Uint8},Int32,Ptr{UErrorCode}),
        tz_u16.data, length(tz_u16.data), locale, 0, err)
    ICUCalendar(p)
end
ICUCalendar() = ICUCalendar("UTC")

close(cal::ICUCalendar) =
    ccall(dlsym(iculibi18n,ucal_close), Void, (Ptr{Void},), cal.ptr)

getNow() = ccall(dlsym(iculibi18n,ucal_getNow), UDate, ())

function getMillis(cal::ICUCalendar)
    err = UErrorCode[0]
    ccall(dlsym(iculibi18n,ucal_getMillis), UDate, (Ptr{Void},Ptr{UErrorCode}),
        cal.ptr, err)
end

function setMillis(cal::ICUCalendar, millis::UDate)
    err = UErrorCode[0]
    ccall(dlsym(iculibi18n,ucal_setMillis), Void, (Ptr{Void},UDate,Ptr{UErrorCode}),
        cal.ptr, millis, err)
end

function setDate(cal::ICUCalendar, y::Integer, m::Integer, d::Integer)
    err = UErrorCode[0]
    ccall(dlsym(iculibi18n,ucal_setDate), Void,
        (Ptr{Void},Int32,Int32,Int32,Ptr{UErrorCode}),
        cal.ptr, y, m, d, err)
end

function setDateTime(cal::ICUCalendar, y::Integer, m::Integer, d::Integer, h::Integer, m::Integer, s::Integer)
    err = UErrorCode[0]
    ccall(dlsym(iculibi18n,ucal_setDateTime), Void,
        (Ptr{Void},Int32,Int32,Int32,Int32,Int32,Int32,Ptr{UErrorCode}),
        cal.ptr, y, m, d, h, m, s, err)
end

function clear(cal::ICUCalendar)
    err = UErrorCode[0]
    ccall(dlsym(iculibi18n,ucal_clear), Void, (Ptr{Void},Ptr{UErrorCode}), cal.ptr, err)
end

function get(cal::ICUCalendar, field::Int32)
    err = UErrorCode[0]
    ccall(dlsym(iculibi18n,ucal_get), Int32,
        (Ptr{Void},Int32,Ptr{UErrorCode}),
        cal.ptr, field, err)
end
get(cal::ICUCalendar, fields::Array{Int32,1}) = [get(cal,f) for f in fields]

function add(cal::ICUCalendar, field::Int32, amount::Integer)
    err = UErrorCode[0]
    ccall(dlsym(iculibi18n,ucal_add), Int32,
        (Ptr{Void},Int32,Int32,Ptr{UErrorCode}),
        cal.ptr, field, amount, err)
end

function test_icucalendar()
    cal = ICUCalendar("America/Los_Angeles")
    setMillis(cal, getNow())
    fields = [UCAL_YEAR, UCAL_MONTH, UCAL_DATE,
              UCAL_HOUR_OF_DAY, UCAL_MINUTE, UCAL_SECOND]
    println(get(cal,fields))
    clear(cal)
end

end # module
