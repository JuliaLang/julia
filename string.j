symbol(s::String) =
    ccall(dlsym(JuliaDLHandle,"jl_symbol"), Any, (Ptr{Char},), s)::Symbol

gensym() = ccall(dlsym(JuliaDLHandle,"jl_gensym"), Any, ())::Symbol

string(x) =
    ccall(dlsym(JuliaDLHandle,"jl_cstr_to_array"), Any, (Ptr{Char},),
          ccall(dlsym(JuliaDLHandle,"jl_print_to_string"), Ptr{Char}, (Any,),
                x))::String

function strcat(ss::String...)
    nn = apply(+,map(length,ss))
    c = Array(Uint8,nn)
    i = 1
    for s = ss
        for j = 1:length(s)
            c[i] = s[j]
            i += 1
        end
    end
    c
end

function lpad(s,n,char)
    k = length(s)
    if k >= n
        return s
    end
    p = Array(Uint8,n)
    for i=1:n-k; p[i] = char; end
    for i=1:k; p[n-k+i] = s[i]; end
    p
end

function rpad(s,n,char)
    k = length(s)
    if k >= n
        return s
    end
    p = Array(Uint8,n)
    for i=1:k; p[i] = s[i]; end
    for i=1:n-k; p[k+i] = char; end
    p
end

## string to integer functions ##

function digit(c::Uint8)
    "0"[1] <= c <= "9"[1] ? int32(c - "0"[1]) :
    "A"[1] <= c <= "Z"[1] ? int32(c - "A"[1]) + 10 :
    "a"[1] <= c <= "z"[1] ? int32(c - "a"[1]) + 10 :
    error("non alphanumeric digit")
end

function parse_int(T::Type{Int}, str::String, base::Int32)
    n = zero(T)
    b = one(T)
    for p = 0:length(str)-1
        d = digit(str[length(str)-p])
        if base <= d
            error("digit not valid in base")
        end
        n += d*b
        b *= base
    end
    return n
end

bin(str::String) = parse_int(Int64, str,  2)
oct(str::String) = parse_int(Int64, str,  8)
dec(str::String) = parse_int(Int64, str, 10)
hex(str::String) = parse_int(Int64, str, 16)

## integer to string ##

function uint2str(n::Int, base::Int)
    ndig = n==convert(typeof(n),0) ? 1 : int32(floor(log(n)/log(base)+1))
    sz = ndig+1
    str = Array(Uint8, sz)
    ccall(dlsym(JuliaDLHandle,"uint2str"), Ptr{Uint8},
          (Ptr{Uint8}, Size, Uint64, Uint32),
          str, sz, uint64(n), uint32(base))
    str[:(sz-1)]  # cut out terminating nul
end
