## low-level pcre interface ##

load("pcre_h.j")

libpcre = dlopen("libpcre")

PCRE_VERSION = cstring(ccall(dlsym(libpcre, :pcre_version), Ptr{Uint8}, ()))

# supported options for different use cases

PCRE_COMPILE_OPTIONS = [
    PCRE_ANCHORED
    PCRE_CASELESS
    PCRE_DOLLAR_ENDONLY
    PCRE_DOTALL
    PCRE_EXTENDED
    PCRE_FIRSTLINE
    PCRE_MULTILINE
    PCRE_NEWLINE_ANY
    PCRE_NEWLINE_ANYCRLF
    PCRE_NEWLINE_CR
    PCRE_NEWLINE_CRLF
    PCRE_NEWLINE_LF
    PCRE_NO_AUTO_CAPTURE
    PCRE_NO_START_OPTIMIZE
    PCRE_NO_UTF8_CHECK
    PCRE_UNGREEDY
]

PCRE_EXECUTE_OPTIONS = [
    PCRE_NEWLINE_ANY
    PCRE_NEWLINE_ANYCRLF
    PCRE_NEWLINE_CR
    PCRE_NEWLINE_CRLF
    PCRE_NEWLINE_LF
    PCRE_NOTBOL
    PCRE_NOTEMPTY
    PCRE_NOTEMPTY_ATSTART
    PCRE_NOTEOL
    PCRE_NO_START_OPTIMIZE
    PCRE_NO_UTF8_CHECK
    PCRE_PARTIAL_HARD
    PCRE_PARTIAL_SOFT
]

PCRE_OPTIONS = [PCRE_COMPILE_OPTIONS,PCRE_EXECUTE_OPTIONS]

PCRE_COMPILE_MASK = (|)(PCRE_COMPILE_OPTIONS...)
PCRE_EXECUTE_MASK = (|)(PCRE_EXECUTE_OPTIONS...)
PCRE_OPTIONS_MASK = PCRE_COMPILE_MASK | PCRE_EXECUTE_MASK

function pcre_compile(pattern::String, options::Int)
    errstr = Array(Ptr{Uint8},1)
    erroff = Array(Int32,1)
    regex = (()->ccall(dlsym(libpcre, :pcre_compile), Ptr{Void},
                       (Ptr{Uint8}, Int32, Ptr{Ptr{Uint8}}, Ptr{Int32}, Ptr{Uint8}),
                       cstring(pattern), int32(options), errstr, erroff, C_NULL))()
    if regex == C_NULL
        error("pcre_compile: $(errstr[1])",
              " at position $(erroff[1]+1)",
              " in $(quote_string(pattern))")
    end
    regex
end

# NOTE: options should always be zero in current PCRE

function pcre_study(regex::Ptr{Void}, options::Int)
    errstr = Array(Ptr{Uint8},1)
    extra = (()->ccall(dlsym(libpcre, :pcre_study), Ptr{Void},
                       (Ptr{Void}, Int32, Ptr{Ptr{Uint8}}),
                       regex, int32(options), errstr))()
    if errstr[1] != C_NULL
        error("pcre_study: $(errstr[1])")
    end
    extra
end

function pcre_info{T}(regex::Ptr{Void}, extra::Ptr{Void}, what::Int, ::Type{T})
    buf = Array(Uint8,sizeof(T))
    ret = ccall(dlsym(libpcre, :pcre_fullinfo), Int32,
                (Ptr{Void}, Ptr{Void}, Int32, Ptr{Uint8}),
                regex, extra, int32(what), buf)
    if ret != 0
        error("pcre_info: ",
              ret == PCRE_ERROR_NULL      ? "NULL regex object" :
              ret == PCRE_ERROR_BADMAGIC  ? "invalid regex object" :
              ret == PCRE_ERROR_BADOPTION ? "invalid option flags" :
                                            "unknown error")
    end
    reinterpret(T,buf)[1]
end

function pcre_exec(regex::Ptr{Void}, extra::Ptr{Void},
                   str::ByteString, offset::Index, options::Int)
    ncap = pcre_info(regex, extra, PCRE_INFO_CAPTURECOUNT, Int32)
    ovec = Array(Int32, 3(ncap+1))
    n = ccall(dlsym(libpcre, :pcre_exec), Int32,
                (Ptr{Void}, Ptr{Void}, Ptr{Uint8}, Int32,
                 Int32, Int32, Ptr{Int32}, Int32),
                regex, extra, str, int32(length(str)),
                int32(offset-1), int32(options), ovec, int32(length(ovec)))
    if n < -1
        error("pcre_exec: error $n")
    end
    n < 0 ? Array(Int32,0) : ovec[1:2(ncap+1)]
end
