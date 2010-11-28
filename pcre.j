load("pcre_h.j")

libpcre = dlopen("libpcre")

PCRE_VERSION = string((()->ccall(dlsym(libpcre,"pcre_version"), Ptr{Uint8}, ()))())

## regexp compilation ##

function pcre_compile(pattern::String, options::Int32)
    errstr = Array(Ptr{Uint8},1)
    erroff = Array(Int32,1)
    regexp = (()->ccall(dlsym(libpcre,"pcre_compile"), Ptr{Void},
                       (Ptr{Uint8}, Int32, Ptr{Ptr{Uint8}}, Ptr{Int32}, Ptr{Uint8}),
                       cstring(pattern), options, errstr, erroff, C_NULL))()
    if regexp == C_NULL
        error("pcre_compile: ", string(errstr[1]),
              " at position ", erroff[1]+1,
              " in \"", pattern, "\"")
    end
    regexp
end

pcre_compile(pattern::String) = pcre_compile(pattern, 0)

## regexp info ##

function pcre_info{T}(regexp::Ptr{Void}, what::Int32, ::Type{T})
    buf = Array(Uint8,sizeof(T))
    ret = ccall(dlsym(libpcre,"pcre_fullinfo"), Int32,
                (Ptr{Void}, Ptr{Void}, Int32, Ptr{Uint8}),
                regexp, C_NULL, what, buf)
    if ret != 0
        error("pcre_info: ",
              ret == PCRE_ERROR_NULL      ? "NULL regex object" :
              ret == PCRE_ERROR_BADMAGIC  ? "invalid regex object" :
              ret == PCRE_ERROR_BADOPTION ? "invalid option flags" :
                                            "unknown error")
    end
    reinterpret(T,buf)[1]
end

pcre_capture_count(r) = pcre_info(r, PCRE_INFO_CAPTURECOUNT, Int32)

## regexp matching ##

function pcre_exec(regexp::Ptr{Void}, string::String, offset::Index, options::Int32)
    cstr = cstring(string)
    ovec = Array(Int32, 3*(pcre_capture_count(regexp)+1))
    n = ccall(dlsym(libpcre,"pcre_exec"), Int32,
                (Ptr{Void}, Ptr{Void}, Ptr{Uint8}, Int32, Int32,
                Int32, Ptr{Int32}, Int32),
                regexp, C_NULL, cstr, length(string), offset-1,
                options, ovec, length(ovec))
    if n < -1
        error("pcre_exec: error ", n)
    end
    n < 0 ? Array(Int32,0) : ovec[1:2n]
end

pcre_exec(r::Ptr{Void}, s::String, o::Index) = pcre_exec(r, s, o, 0)
pcre_exec(r::Ptr{Void}, s::String)           = pcre_exec(r, s, 1, 0)
