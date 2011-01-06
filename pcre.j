load("pcre_h.j")

libpcre = dlopen("libpcre")

PCRE_VERSION = string(ccall(dlsym(libpcre,"pcre_version"), Ptr{Uint8}, ()))

## low-level PCRE interface ##

function pcre_compile(pattern::String, options::Int32)
    errstr = Array(Ptr{Uint8},1)
    erroff = Array(Int32,1)
    regex = (()->ccall(dlsym(libpcre,"pcre_compile"), Ptr{Void},
                       (Ptr{Uint8}, Int32, Ptr{Ptr{Uint8}}, Ptr{Int32}, Ptr{Uint8}),
                       cstring(pattern), options, errstr, erroff, C_NULL))()
    if regex == C_NULL
        error("pcre_compile: $(errstr[1])",
              " at position $(erroff[1]+1)",
              " in $(quote_string(pattern))")
    end
    regex
end

function pcre_study(regex::Ptr{Void}, options::Int32)
    errstr = Array(Ptr{Uint8},1)
    extra = (()->ccall(dlsym(libpcre,"pcre_study"), Ptr{Void},
                      (Ptr{Void}, Int32, Ptr{Ptr{Uint8}}),
                      regex, options, errstr))()
    if errstr[1] != C_NULL
        error("pcre_study: ", string(errstr[1]))
    end
    extra
end

function pcre_info{T}(regex::Ptr{Void}, extra::Ptr{Void}, what::Int32, ::Type{T})
    buf = Array(Uint8,sizeof(T))
    ret = ccall(dlsym(libpcre,"pcre_fullinfo"), Int32,
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

function pcre_exec(regex::Ptr{Void}, extra::Ptr{Void},
                   string::ByteString,
                   offset::Index, options::Int32)
    ncap = pcre_info(regex, extra, PCRE_INFO_CAPTURECOUNT, Int32)
    ovec = Array(Int32, 3*(ncap+1))
    n = ccall(dlsym(libpcre,"pcre_exec"), Int32,
                (Ptr{Void}, Ptr{Void}, Ptr{Uint8}, Int32, Int32,
                Int32, Ptr{Int32}, Int32),
                regex, extra, string, length(string), offset-1,
                options, ovec, length(ovec))
    if n < -1
        error("pcre_exec: error ", n)
    end
    n < 0 ? Array(Int32,0) : ovec[1:2n]
end

## object-oriented Regex interface ##

struct Regex
    pattern::String
    options::Int32
    regex::Ptr{Void}
    extra::Ptr{Void}

    function Regex(p::String, o::Int, s::Bool)
        re = new(p, int32(o), C_NULL, C_NULL)
        re.regex = pcre_compile(re.pattern, re.options)
        if s; re.extra = pcre_study(re.regex, re.options); end
        re
    end
    Regex(p::String, o::Int)  = Regex(p, o, true)
    Regex(p::String, s::Bool) = Regex(p, 0, s)
    Regex(p::String)          = Regex(p, 0, true)
end

# TODO: make sure thing are escaped in a way PCRE
# likes so that Julia all the Julia string quoting
# constructs are correctly handled.

macro r_str(s)
    Regex(s)
end

function show(re::Regex)
    print("Regex(")
    show(re.pattern)
    print(',')
    show(re.options)
    print(')')
end

struct RegexMatch
    match::Union((),String)
    captures::Tuple
    offset::Index
end

show(m::RegexMatch) = show(m.match)

function match(re::Regex, str::String)
    cstr = cstring(str)
    m = pcre_exec(re.regex, re.extra, cstr, 1, re.options)
    if isempty(m); return RegexMatch((),(),-1); end
    mat = cstr[m[1]+1:m[2]]
    cap = ntuple(div(length(m),2)-1,
                 i->cstr[m[2i+1]+1:m[2i+2]])
    RegexMatch(mat, cap, m[1]+1)
end

match(pattern::String, str::String) = match(Regex(pattern, false), str)
