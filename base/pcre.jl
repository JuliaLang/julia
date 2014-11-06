## low-level pcre interface ##

module PCRE

include("pcre_h.jl")

const VERSION = bytestring(ccall((:pcre_version, :libpcre), Ptr{UInt8}, ()))

global JIT_STACK = C_NULL
function __init__()
    JIT_STACK_START_SIZE = 32768
    JIT_STACK_MAX_SIZE = 1048576
    global JIT_STACK = ccall((:pcre_jit_stack_alloc, :libpcre), Ptr{Void},
                             (Cint, Cint), JIT_STACK_START_SIZE, JIT_STACK_MAX_SIZE)
end

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
    regex::Ptr{Void},
    extra::Ptr{Void}, what::Integer, ::Type{T}
)
    buf = Array(UInt8,sizeof(T))
    ret = ccall((:pcre_fullinfo, :libpcre), Int32,
                (Ptr{Void}, Ptr{Void}, Int32, Ptr{UInt8}),
                regex, extra, what, buf)
    if ret != 0
        error(ret == ERROR_NULL      ? "NULL regex object" :
              ret == ERROR_BADMAGIC  ? "invalid regex object" :
              ret == ERROR_BADOPTION ? "invalid option flags" :
                                       "unknown error $ret")
    end
    reinterpret(T,buf)[1]
end

function config{T}(what::Integer, ::Type{T})
    buf = Array(UInt8, sizeof(T))
    ret = ccall((:pcre_config, :libpcre), Int32,
                (Int32, Ptr{UInt8}),
                what, buf)

    if ret != 0
        error("error $n")
    end
    reinterpret(T,buf)[1]
end

function compile(pattern::AbstractString, options::Integer)
    errstr = Array(Ptr{UInt8},1)
    erroff = Array(Int32,1)
    re_ptr = ccall((:pcre_compile, :libpcre), Ptr{Void},
                    (Ptr{UInt8}, Int32, Ptr{Ptr{UInt8}}, Ptr{Int32}, Ptr{UInt8}),
                    pattern, options, errstr, erroff, C_NULL)
    if re_ptr == C_NULL
        error("$(bytestring(errstr[1]))",
              " at position $(erroff[1]+1)",
              " in $(repr(pattern))")
    end

    re_ptr
end

function study(regex::Ptr{Void}, options::Integer)
    # NOTE: options should always be zero in current PCRE
    errstr = Array(Ptr{UInt8},1)
    extra = ccall((:pcre_study, :libpcre), Ptr{Void},
                  (Ptr{Void}, Int32, Ptr{Ptr{UInt8}}),
                  regex, options, errstr)
    if errstr[1] != C_NULL
        error("$(bytestring(errstr[1]))")
    end

    ccall((:pcre_assign_jit_stack, :libpcre), Void,
          (Ptr{Void}, Ptr{Void}, Ptr{Void}),
          extra, C_NULL, JIT_STACK)
    extra
end

study(re::Ptr{Void}) = study(re, int32(0))

free_study(extra::Ptr{Void}) =
    ccall((:pcre_free_study, :libpcre), Void, (Ptr{Void},), extra)
free(regex::Ptr{Void}) =
    ccall(unsafe_load(cglobal((:pcre_free, :libpcre),Ptr{Void})), Void, (Ptr{Void},), regex)

function exec(regex::Ptr{Void}, extra::Ptr{Void}, str::SubString, offset::Integer,
              options::Integer, ovec::Vector{Int32})
    return exec(regex, extra, str.string, str.offset, offset, sizeof(str),
                options, ovec)
end

function exec(regex::Ptr{Void}, extra::Ptr{Void}, str::ByteString, offset::Integer,
              options::Integer, ovec::Vector{Int32})
    return exec(regex, extra, str, 0, offset, sizeof(str), options, ovec)
end

function exec(regex::Ptr{Void}, extra::Ptr{Void},
              str::ByteString, shift::Integer, offset::Integer,
              len::Integer, options::Integer,
              ovec::Vector{Int32})
    if offset < 0 || len < offset || len+shift > sizeof(str)
        error(BoundsError)
    end
    n = ccall((:pcre_exec, :libpcre), Int32,
              (Ptr{Void}, Ptr{Void}, Ptr{UInt8}, Int32,
               Int32, Int32, Ptr{Int32}, Int32),
              regex, extra, pointer(str.data,shift+1), len,
              offset, options, ovec, length(ovec))
    if n < -1
        error("error $n")
    end
    return n > -1
end

end # module
