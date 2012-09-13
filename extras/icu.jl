#
# ICU - International Components for Unicode
# 
# Examples:
#
# 1) ICU.upper("testingß") -> "TESTINGSS"
# 2) ICU.upper("testingß", "tr") ->  "TESTİNGSS"
#
# Note that "ß" gets converted to "SS" in the first example, and "i" gets
# converted to "İ" (dotted capital I) in the second example when the locale
# is set to Turkish.
#

module ICU
import Base.*
export lower, upper

load("openlib.jl")
const iculib = openlib(OS_NAME == :Darwin ? "libicucore" : "libicuuc")

u_strFromUTF8 = "u_strFromUTF8"
u_strToUTF8 = "u_strToUTF8"
u_strToLower = "u_strToLower"
u_strToUpper = "u_strToUpper"

for suffix in ["", ["_"*string(i) for i in 42:50]]
    if dlsym(iculib, "u_strToUpper"*suffix) != C_NULL
        u_strFromUTF8 *= suffix
        u_strToUTF8 *= suffix
        u_strToLower *= suffix
        u_strToUpper *= suffix
        break
    end
end

const UErrorCode = Int32

function _str_to_u16(str::String)
    buf16siz = int32(2*strlen(str))
    buf16 = zeros(Uint16, buf16siz)
    err = UErrorCode[0]
    ccall(dlsym(iculib,u_strFromUTF8), Ptr{Uint16},
        (Ptr{Uint16},Int32,Ptr{Int32},Ptr{Uint8},Int32,Ptr{UErrorCode}),
        buf16, buf16siz, C_NULL, bytestring(str), -1, err)
    return buf16
end

function _u16_to_str(buf16::Array{Uint16,1})
    buf8siz = int32(2*size(buf16,1))
    buf8 = zeros(Uint8, buf8siz)
    err = UErrorCode[0]
    ccall(dlsym(iculib,u_strToUTF8), Ptr{Uint8},
        (Ptr{Uint8},Int32,Ptr{Int32},Ptr{Uint16},Int32,Ptr{UErrorCode}),
        buf8, buf8siz, C_NULL, buf16, size(buf16,1), err)
    str = utf8(buf8)
    return str[1:strchr(str,'\0',1)-1]
end

function lower(s::String, locale::String)
    src = _str_to_u16(s)
    destsiz = int32(2*size(src,1))
    dest = zeros(Uint16, destsiz)
    err = UErrorCode[0]
    ccall(dlsym(iculib,u_strToLower), Int32,
        (Ptr{Uint16},Int32,Ptr{Uint16},Int32,Ptr{Uint8},Ptr{UErrorCode}),
        dest, destsiz, src, size(src,1), strlen(locale) > 0 ? bytestring(locale) : C_NULL, err)
    return _u16_to_str(dest)
end
lower(s) = lower(s, "")

function upper(s::String, locale::String)
    src = _str_to_u16(s)
    destsiz = int32(2*size(src,1))
    dest = zeros(Uint16, destsiz)
    err = UErrorCode[0]
    ccall(dlsym(iculib,u_strToUpper), Int32,
        (Ptr{Uint16},Int32,Ptr{Uint16},Int32,Ptr{Uint8},Ptr{UErrorCode}),
        dest, destsiz, src, size(src,1), strlen(locale) > 0 ? bytestring(locale) : C_NULL, err)
    return _u16_to_str(dest)
end
upper(s) = upper(s, "")

end # module
