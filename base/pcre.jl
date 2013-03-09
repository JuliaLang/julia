## low-level pcre interface ##

module PCRE

include("pcre_h.jl")

const VERSION = bytestring(ccall((:pcre_version, :libpcre), Ptr{Uint8}, ()))

# supported options for different use cases

const COMPILE_MASK      =
      ANCHORED          |
      CASELESS          |
      DOLLAR_ENDONLY    |
      DOTALL            |
      EXTENDED          |
      FIRSTLINE         |
      JAVASCRIPT_COMPAT |
      MULTILINE         |
      NEWLINE_ANY       |
      NEWLINE_ANYCRLF   |
      NEWLINE_CR        |
      NEWLINE_CRLF      |
      NEWLINE_LF        |
      NO_AUTO_CAPTURE   |
      NO_START_OPTIMIZE |
      NO_UTF8_CHECK     |
      UNGREEDY          |
      UTF8

const EXECUTE_MASK      =
      NEWLINE_ANY       |
      NEWLINE_ANYCRLF   |
      NEWLINE_CR        |
      NEWLINE_CRLF      |
      NEWLINE_LF        |
      NOTBOL            |
      NOTEMPTY          |
      NOTEMPTY_ATSTART  |
      NOTEOL            |
      NO_START_OPTIMIZE |
      NO_UTF8_CHECK     |
      PARTIAL_HARD      |
      PARTIAL_SOFT

const OPTIONS_MASK = COMPILE_MASK | EXECUTE_MASK

function info{T}(
    regex::Union(Ptr{Void},Vector{Uint8}),
    extra::Ptr{Void}, what::Integer, ::Type{T}
)
    buf = Array(Uint8,sizeof(T))
    ret = ccall((:pcre_fullinfo, :libpcre), Int32,
                (Ptr{Void}, Ptr{Void}, Int32, Ptr{Uint8}),
                regex, extra, what, buf)
    if ret != 0
        error("info: ",
              ret == ERROR_NULL      ? "NULL regex object" :
              ret == ERROR_BADMAGIC  ? "invalid regex object" :
              ret == ERROR_BADOPTION ? "invalid option flags" :
                                       "unknown error $ret")
    end
    reinterpret(T,buf)[1]
end

function compile(pattern::String, options::Integer)
    errstr = Array(Ptr{Uint8},1)
    erroff = Array(Int32,1)
    re_ptr = (()->ccall((:pcre_compile, :libpcre), Ptr{Void},
                        (Ptr{Uint8}, Int32, Ptr{Ptr{Uint8}}, Ptr{Int32}, Ptr{Uint8}),
                        pattern, options, errstr, erroff, C_NULL))()
    if re_ptr == C_NULL
        error("compile: $(bytestring(errstr[1]))",
              " at position $(erroff[1]+1)",
              " in $(quote_string(pattern))")
    end
    size = info(re_ptr, C_NULL, INFO_SIZE, Int32)
    regex = Array(Uint8,size)
    ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Uint), regex, re_ptr, size)
    regex
end

function study(regex::Array{Uint8}, options::Integer)
    # NOTE: options should always be zero in current PCRE
    errstr = Array(Ptr{Uint8},1)
    extra = ccall((:pcre_study, :libpcre), Ptr{Void},
                  (Ptr{Void}, Int32, Ptr{Ptr{Uint8}}),
                  regex, options, errstr)
    if errstr[1] != C_NULL
        error("study: $(bytestring(errstr[1]))")
    end
    extra
end
study(re::Array{Uint8}) = study(re, int32(0))

free_study(extra::Ptr{Void}) =
    ccall((:pcre_free_study, :libpcre), Void, (Ptr{Void},), extra)

function exec(regex::Array{Uint8}, extra::Ptr{Void},
              str::ByteString, offset::Integer, options::Integer, cap::Bool)
    if offset < 0 || length(str.data) < offset
        error(BoundsError)
    end
    ncap = info(regex, extra, INFO_CAPTURECOUNT, Int32)
    ovec = Array(Int32, 3(ncap+1))
    n = ccall((:pcre_exec, :libpcre), Int32,
              (Ptr{Void}, Ptr{Void}, Ptr{Uint8}, Int32,
               Int32, Int32, Ptr{Int32}, Int32),
              regex, extra, str, length(str.data),
              offset, options, ovec, length(ovec))
    if n < -1
        error("exec: error $n")
    end
    cap ? ((n > -1 ? ovec[1:2(ncap+1)] : Array(Int32,0)), ncap) : n > -1
end

end # module
