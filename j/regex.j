load("pcre_h.j")

libpcre = dlopen("libpcre")

PCRE_VERSION = cstring(ccall(dlsym(libpcre, :pcre_version), Ptr{Uint8}, ()))

## masks for supported sets of options ##

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

## low-level PCRE interface ##

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

## object-oriented Regex interface ##

type Regex
    pattern::ByteString
    options::Int32
    regex::Ptr{Void}
    extra::Ptr{Void}

    function Regex(pat::String, opts::Int, study::Bool)
        pat = cstring(pat); opts = int32(opts)
        if (opts & ~PCRE_OPTIONS_MASK) != 0
            error("invalid regex option(s)")
        end
        re = pcre_compile(pat, opts & PCRE_COMPILE_MASK)
        ex = study ? pcre_study(re, 0) : C_NULL
        new(pat, opts, re, ex)
    end
end
Regex(p::String, s::Bool) = Regex(p, 0, s)
Regex(p::String, o::Int)  = Regex(p, o, true)
Regex(p::String)          = Regex(p, 0, true)

# TODO: make sure thing are escaped in a way PCRE
# likes so that Julia all the Julia string quoting
# constructs are correctly handled.

begin
    local i = PCRE_CASELESS
    local m = PCRE_MULTILINE
    local s = PCRE_DOTALL
    local x = PCRE_EXTENDED

    macro r_str(p);     Regex(p);          end
    macro ri_str(p);    Regex(p, i);       end
    macro rm_str(p);    Regex(p, m);       end
    macro rs_str(p);    Regex(p, s);       end
    macro rx_str(p);    Regex(p, x);       end
    macro rim_str(p);   Regex(p, i|m);     end
    macro ris_str(p);   Regex(p, i|s);     end
    macro rix_str(p);   Regex(p, i|x);     end
    macro rms_str(p);   Regex(p, m|s);     end
    macro rmx_str(p);   Regex(p, m|x);     end
    macro rsx_str(p);   Regex(p, s|x);     end
    macro rims_str(p);  Regex(p, i|m|s);   end
    macro rimx_str(p);  Regex(p, i|m|x);   end
    macro risx_str(p);  Regex(p, i|s|x);   end
    macro rmsx_str(p);  Regex(p, m|s|x);   end
    macro rimsx_str(p); Regex(p, i|m|s|x); end

    function show(re::Regex)
        if (re.options & ~(i|m|s|x)) == 0
            print('r')
            if (re.options & i) != 0; print('i'); end
            if (re.options & m) != 0; print('m'); end
            if (re.options & s) != 0; print('s'); end
            if (re.options & x) != 0; print('x'); end
            print_quoted_literal(re.pattern)
        else
            print("Regex(")
            show(re.pattern)
            print(',')
            show(re.options)
            print(')')
        end
    end
end

type RegexMatch
    match::Union(Nothing,ByteString)
    captures::Tuple
    offset::Index
end

show(m::RegexMatch) = show(m.match)

function match(re::Regex, str::String, opts::Int)
    cstr = cstring(str)
    m = pcre_exec(re.regex, C_NULL, cstr, 1, int32(opts))
    if isempty(m); return RegexMatch(nothing,(),-1); end
    mat = cstr[m[1]+1:m[2]]
    n = pcre_info(re.regex, re.extra, PCRE_INFO_CAPTURECOUNT, Int32)
    cap = ntuple(n, i->(m[2i+1] < 0 ? nothing : cstr[m[2i+1]+1:m[2i+2]]))
    RegexMatch(mat, cap, m[1]+1)
end

match(re::Regex, str::String) =
    match(re, str, re.options & PCRE_EXECUTE_MASK)
