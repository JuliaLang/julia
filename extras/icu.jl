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

for suffix in ["", ["_"*string(i) for i in 42:50]]
    if dlsym(iculib, "u_strToUpper"*suffix) != C_NULL
        for f in (:u_strFromUTF8,
                  :u_strToUTF8,
                  :u_strFoldCase,
                  :u_strToLower,
                  :u_strToTitle,
                  :u_strToUpper,
                  :u_strlen,
                  :ucasemap_open,
                  :ucasemap_close,
                  :ucasemap_getBreakIterator,
                  :ucasemap_utf8FoldCase,
                  :ucasemap_utf8ToLower,
                  :ucasemap_utf8ToTitle,
                  :ucasemap_utf8ToUpper)
            @eval const $f = $string(f) * $suffix
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
    ccall(dlsym(iculib,u_strlen), Int32, (Ptr{UChar},), s.data)

length(icu::ICUString) = length(icu.data)

function next(s::ICUString, i::Int)
    # XXX:fixme
    char(s.data[i]), i+1
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

end # module
