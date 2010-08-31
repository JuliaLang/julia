symbol(s::ArrayString) =
    ccall(dlsym(JuliaDLHandle,"jl_symbol"), Any, (Ptr{Uint8},), s.data)::Symbol

gensym() = ccall(dlsym(JuliaDLHandle,"jl_gensym"), Any, ())::Symbol

function string(x)
    cstr = ccall(dlsym(JuliaDLHandle,"jl_print_to_string"),
                 Ptr{Uint8}, (Any,), x)
    data = ccall(dlsym(JuliaDLHandle,"jl_cstr_to_array"),
                 Any, (Ptr{Uint8},), cstr)::Array{Uint8,1}
    ArrayString(data)
end

length(s::ArrayString) = length(s.data)
inspect(s::ArrayString) = print(quote_string(s).data)
strcat(ss::ArrayString...) = ArrayString(vcat(map(s->s.data, ss)...))
ref(s::ArrayString, i::Index) = ArrayString([s.data[i]])
ref(s::ArrayString, x) = ArrayString(s.data[x])
chr(c::Int) = ArrayString([uint8(c)])
ord(s::ArrayString) = s.data[1]

function cmp(a::ArrayString, b::ArrayString)
    for i = 1:min(length(a),length(b))
        if a.data[i] != b.data[i]
            return a.data[i] < b.data[i] ? -1 : +1
        end
    end
    length(a) < length(b) ? -1 :
    length(a) > length(b) ? +1 : 0
end

(<) (a::ArrayString, b::ArrayString) = cmp(a,b) < 0
(>) (a::ArrayString, b::ArrayString) = cmp(a,b) > 0
(==)(a::ArrayString, b::ArrayString) = cmp(a,b) == 0
(<=)(a::ArrayString, b::ArrayString) = cmp(a,b) <= 0
(>=)(a::ArrayString, b::ArrayString) = cmp(a,b) >= 0

(+)(ss::ArrayString...) = strcat(ss...)

global escape_strings_with_hex = false

function escape_string(raw::ArrayString)
    esc = ""
    for i = 1:length(raw)
        c = ord(raw[i])
        z = i < length(raw) && "0" <= raw[i+1] <= "7" ?
            (escape_strings_with_hex ? "\\x00" : "\\000") : "\\0"
        e = c == 0 ? z :
            c == ord("\\") ? "\\\\" :
            c == 27 ? "\\e" :
            31 < c < 127 ? chr(c) :
            7 <= c <= 13 ? "\\" + "abtnvfr"[c-6] :
            escape_strings_with_hex ?
                "\\x" + uint2str(c,16,2) :
                "\\"  + uint2str(c, 8,3)
        esc += e
    end
    esc
end

function unescape_string(esc::ArrayString)
    raw = ""
    i = 1
    while i <= length(esc)
        if i < length(esc) && esc[i] == "\\"
            e = esc[i+1]
            i += 2
            c = e == "a" ?  7 :
                e == "b" ?  8 :
                e == "t" ?  9 :
                e == "n" ? 10 :
                e == "v" ? 11 :
                e == "f" ? 12 :
                e == "r" ? 13 :
                e == "e" ? 27 :
                e == "x" ? begin
                    x = 0
                    m = min(i+1,length(esc))
                    while i <= m
                        if "0" <= esc[i] <= "9"
                            x = 16*x + ord(esc[i]) - ord("0")
                        elseif "a" <= esc[i] <= "f"
                            x = 16*x + ord(esc[i]) - ord("a") + 10
                        elseif "A" <= esc[i] <= "F"
                            x = 16*x + ord(esc[i]) - ord("A") + 10
                        else
                            break
                        end
                        i += 1
                    end
                    if esc[i-1] == "x"
                        error("\\x used with no following hex digits")
                    end
                    x
                end :
                "0" <= e <= "7" ? begin
                    x = ord(e) - ord("0")
                    m = min(i+1,length(esc))
                    while i <= m && "0" <= esc[i] <= "7"
                        x = 8*x + ord(esc[i]) - ord("0")
                        i += 1
                    end
                    if x > 255
                        error("octal escape sequence out of range")
                    end
                    x
                end : ord(e)
            raw += chr(c)
        else
            raw += esc[i]
            i += 1
        end
    end
    raw
end

function quote_string(raw::ArrayString)
    esc = escape_string(raw)
    quo = "\""
    for i = 1:length(esc)
       quo += esc[i] == "\"" ? "\\\"" : esc[i]
    end
    quo += "\""
end

function lpad(s::ArrayString, n::Int, p::ArrayString)
    if n <= length(s)
        return s
    end
    ps = s
    while length(ps) < n
        ps = p + ps
    end
    ps[length(ps)-n+1:length(ps)]
end

function rpad(s::ArrayString, n::Int, p::ArrayString)
    if n <= length(s)
        return s
    end
    ps = s
    while length(ps) < n
        ps += p
    end
    ps[1:n]
end

## string to integer functions ##

function parse_int(T::Type{Int}, str::ArrayString, base::Int)
    n = zero(T)
    base = convert(T,base)
    for i = 1:length(str)
        c = str[i]
        d = "0" <= c <= "9" ? int32(ord(c) - ord("0")) :
            "A" <= c <= "Z" ? int32(ord(c) - ord("A")) + 10 :
            "a" <= c <= "z" ? int32(ord(c) - ord("a")) + 10 :
            error("non alphanumeric digit")
        d = convert(T,d)
        if base <= d
            error("digit not valid in base")
        end
        n = n*base + d
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
    data = Array(Uint8, sz)
    ccall(dlsym(JuliaDLHandle,"uint2str"), Ptr{Uint8},
          (Ptr{Uint8}, Size, Uint64, Uint32),
          data, sz, uint64(n), uint32(base))
    ArrayString(data[:(sz-1)]) # cut out terminating NUL
end

uint2str(n::Int, base::Int, len::Int) = lpad(uint2str(n,base),len,"0")
