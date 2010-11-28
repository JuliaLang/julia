load("pcre_h.j")

libpcre = dlopen("libpcre")

function pcre_compile(pattern::String)
    errstr = Array(Ptr{Uint8},1)
    erroff = Array(Int32,1)
    regexp = (()->ccall(dlsym(libpcre,"pcre_compile"), Ptr{Void},
                       (Ptr{Uint8}, Int32, Ptr{Ptr{Uint8}}, Ptr{Int32}, Ptr{Uint8}),
                       cstring(pattern), 0, errstr, erroff, C_NULL))()
    if regexp == C_NULL
        error("pcre: ", string(errstr[1]),
              " at position ", erroff[1]+1,
              " in \"", pattern, "\"")
    end
    regexp
end
