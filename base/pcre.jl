## low-level pcre interface ##

include("pcre_h.jl")

_jl_libpcre = dlopen("libpcre")

const PCRE_VERSION = cstring(ccall(dlsym(_jl_libpcre, :pcre_version), Ptr{Uint8}, ()))

# supported options for different use cases

const PCRE_COMPILE_MASK      =
      PCRE_ANCHORED          |
      PCRE_CASELESS          |
      PCRE_DOLLAR_ENDONLY    |
      PCRE_DOTALL            |
      PCRE_EXTENDED          |
      PCRE_FIRSTLINE         |
      PCRE_MULTILINE         |
      PCRE_NEWLINE_ANY       |
      PCRE_NEWLINE_ANYCRLF   |
      PCRE_NEWLINE_CR        |
      PCRE_NEWLINE_CRLF      |
      PCRE_NEWLINE_LF        |
      PCRE_NO_AUTO_CAPTURE   |
      PCRE_NO_START_OPTIMIZE |
      PCRE_NO_UTF8_CHECK     |
      PCRE_UNGREEDY          |
      PCRE_UTF8

const PCRE_EXECUTE_MASK      =
      PCRE_NEWLINE_ANY       |
      PCRE_NEWLINE_ANYCRLF   |
      PCRE_NEWLINE_CR        |
      PCRE_NEWLINE_CRLF      |
      PCRE_NEWLINE_LF        |
      PCRE_NOTBOL            |
      PCRE_NOTEMPTY          |
      PCRE_NOTEMPTY_ATSTART  |
      PCRE_NOTEOL            |
      PCRE_NO_START_OPTIMIZE |
      PCRE_NO_UTF8_CHECK     |
      PCRE_PARTIAL_HARD      |
      PCRE_PARTIAL_SOFT

const PCRE_OPTIONS_MASK = PCRE_COMPILE_MASK | PCRE_EXECUTE_MASK

function pcre_info{T}(
    regex::Union(Ptr{Void},Vector{Uint8}),
    extra::Ptr{Void}, what::Integer, ::Type{T}
)
    buf = Array(Uint8,sizeof(T))
    ret = ccall(dlsym(_jl_libpcre, :pcre_fullinfo), Int32,
                (Ptr{Void}, Ptr{Void}, Int32, Ptr{Uint8}),
                regex, extra, what, buf)
    if ret != 0
        error("pcre_info: ",
              ret == PCRE_ERROR_NULL      ? "NULL regex object" :
              ret == PCRE_ERROR_BADMAGIC  ? "invalid regex object" :
              ret == PCRE_ERROR_BADOPTION ? "invalid option flags" :
                                            "unknown error")
    end
    reinterpret(T,buf)[1]
end

function pcre_compile(pattern::String, options::Integer)
    errstr = Array(Ptr{Uint8},1)
    erroff = Array(Int32,1)
    re_ptr = (()->ccall(dlsym(_jl_libpcre, :pcre_compile), Ptr{Void},
                        (Ptr{Uint8}, Int32, Ptr{Ptr{Uint8}}, Ptr{Int32}, Ptr{Uint8}),
                        pattern, options, errstr, erroff, C_NULL))()
    if re_ptr == C_NULL
        error("pcre_compile: $(errstr[1])",
              " at position $(erroff[1]+1)",
              " in $(quote_string(pattern))")
    end
    size = pcre_info(re_ptr, C_NULL, PCRE_INFO_SIZE, Int32)
    regex = Array(Uint8,size)
    ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Uint), regex, re_ptr, size)
    regex
end

function pcre_study(regex::Array{Uint8}, options::Integer)
    # NOTE: options should always be zero in current PCRE
    errstr = Array(Ptr{Uint8},1)
    extra = (()->ccall(dlsym(_jl_libpcre, :pcre_study), Ptr{Void},
                       (Ptr{Void}, Int32, Ptr{Ptr{Uint8}}),
                       regex, options, errstr))()
    if errstr[1] != C_NULL
        error("pcre_study: $(errstr[1])")
    end
    extra
end
pcre_study(re::Array{Uint8}) = pcre_study(re, int32(0))

function pcre_exec(regex::Array{Uint8}, extra::Ptr{Void},
                   str::ByteString, offset::Integer, options::Integer, cap::Bool)
    if offset > length(str) error("pcre_exec: index out of range") end
    ncap = pcre_info(regex, extra, PCRE_INFO_CAPTURECOUNT, Int32)
    ovec = Array(Int32, 3(ncap+1))
    n = ccall(dlsym(_jl_libpcre, :pcre_exec), Int32,
              (Ptr{Void}, Ptr{Void}, Ptr{Uint8}, Int32,
               Int32, Int32, Ptr{Int32}, Int32),
              regex, extra, str, length(str),
              offset, options, ovec, length(ovec))
    if n < -1
        error("pcre_exec: error $n")
    end
    cap ? ((n > -1 ? ovec[1:2(ncap+1)] : Array(Int32,0)), ncap) : n > -1
end
