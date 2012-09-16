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

module ICU
import Base.*

export ICUString,
    foldcase,
    lowercase,
    set_locale,
    titlecase,
    uppercase

load("openlib.jl")
const iculib = openlib(OS_NAME == :Darwin ? "libicucore" : "libicuuc")
const iculibi18n = OS_NAME == :Darwin ? iculib : openlib("libicui18n")

for suffix in ["", ["_"*string(i) for i in 42:50]]
    if dlsym(iculib, "u_strToUpper"*suffix) != C_NULL
        for f in (:u_strFromUTF8,
                  :u_strToUTF8,
                  :u_strFoldCase,
                  :u_strToLower,
                  :u_strToTitle,
                  :u_strToUpper,
                  :u_countChar32,
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

const UChar = Uint16
const UErrorCode = Int32

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

type ICUString <: String
    data::Array{UChar,1}
end

function ICUString(str::ByteString)
    bufsiz = int32(length(str))
    buf = zeros(UChar, bufsiz)
    err = UErrorCode[0]
    pn = Int32[0]
    ccall(dlsym(iculib,u_strFromUTF8), Ptr{UChar},
        (Ptr{UChar},Int32,Ptr{Int32},Ptr{Uint8},Int32,Ptr{UErrorCode}),
        buf, bufsiz, pn, bytestring(str), -1, err)
    n = pn[1]
    @assert n <= bufsiz
    return ICUString(buf[1:n])
end

strlen(s::ICUString) =
    ccall(dlsym(iculib,u_countChar32), Int32, (Ptr{UChar},Int32), s.data, length(s.data))

length(icu::ICUString) = length(icu.data)

utf16_is_lead(c::Uint16) = (c & 0xfc00) == 0xd800
utf16_is_trail(c::Uint16) = (c & 0xfc00) == 0xdc00
utf16_is_surrogate(c::Uint16) = (c & 0xf800) == 0xd800
utf16_get_supplementary(lead::Uint16, trail::Uint16) = char((lead-0xd7f7)<<10 + trail)

function next(s::ICUString, i::Int)
    if !utf16_is_surrogate(s.data[i])
        return char(s.data[i]), i+1
    elseif length(s.data) > i && utf16_is_lead(s.data[i]) && utf16_is_trail(s.data[i+1])
        return utf16_get_supplementary(s.data[i], s.data[i+1]), i+2
    end
    error("invalid UTF-16 character index")
end

function utf8(src::ICUString)
    destsiz = int32(2*length(src))
    dest = zeros(Uint8, destsiz)
    err = UErrorCode[0]
    pn = Int32[0]
    ccall(dlsym(iculib,u_strToUTF8), Ptr{Uint8},
        (Ptr{Uint8},Int32,Ptr{Int32},Ptr{UChar},Int32,Ptr{UErrorCode}),
        dest, destsiz, pn, src.data, numel(src.data), err)
    n = pn[1]
    @assert n <= destsiz
    utf8(dest[1:n])
end

for (a,b) in [(:lowercase,:u_strToLower),
              (:uppercase,:u_strToUpper)]
    @eval begin
        function ($a)(s::ICUString)
            src = s.data
            destsiz = int32(2*numel(src))
            dest = zeros(UChar, destsiz)
            err = UErrorCode[0]
            n = ccall(dlsym(iculib,$b), Int32,
                (Ptr{UChar},Int32,Ptr{UChar},Int32,Ptr{Uint8},Ptr{UErrorCode}),
                dest, destsiz, src, numel(src), locale, err)
            return ICUString(dest[1:n])
        end
    end
end

function foldcase(s::ICUString)
    src = s.data
    destsiz = int32(2*numel(src))
    dest = zeros(UChar, destsiz)
    err = UErrorCode[0]
    n = ccall(dlsym(iculib,u_strFoldCase), Int32,
        (Ptr{UChar},Int32,Ptr{UChar},Int32,Uint32,Ptr{UErrorCode}),
        dest, destsiz, src, numel(src), 0, err)
    return ICUString(dest[1:n])
end

function titlecase(s::ICUString)
    src = s.data
    destsiz = int32(2*numel(src))
    dest = zeros(UChar, destsiz)
    err = UErrorCode[0]
    breakiter = ccall(dlsym(iculib,ucasemap_getBreakIterator),
        Ptr{Void}, (Ptr{Void},), casemap)
    n = ccall(dlsym(iculib,u_strToTitle), Int32,
        (Ptr{UChar},Int32,Ptr{UChar},Int32,Ptr{Void},Ptr{Uint8},Ptr{UErrorCode}),
        dest, destsiz, src, numel(src), breakiter, locale, err)
    return ICUString(dest[1:n])
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

function test_icustring()
    s = "𝕥𝟶f𠂊"
    t = ICUString(s)
    @assert strlen(t) == 4
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
    tz_u16 = ICUString(timezone)
    err = UErrorCode[0]
    p = ccall(dlsym(iculibi18n,ucal_open), Ptr{Void},
        (Ptr{UChar},Int32,Ptr{Uint8},Int32,Ptr{UErrorCode}),
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
