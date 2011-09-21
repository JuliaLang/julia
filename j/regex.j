load("pcre.j")

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
    match::ByteString
    captures::Tuple
    offset::Index
    offsets::Vector{Index}
end

function show(m::RegexMatch)
    print("RegexMatch(")
    show(m.match)
    if !isempty(m.captures)
        print(", ")
        for i = 1:length(m.captures)
            print(i, "=")
            show(m.captures[i])
            if i < length(m.captures)
                print(", ")
            end
        end
    end
    print(")")
end

matches(r::Regex, s::String, o::Int) = pcre_exec(r.regex, C_NULL, cstring(s), 1, int32(o), false)
matches(r::Regex, s::String) = matches(r, s, r.options & PCRE_EXECUTE_MASK)

function match(re::Regex, str::String, offset::Int, opts::Int)
    cstr = cstring(str)
    m, n = pcre_exec(re.regex, C_NULL, cstr, offset, opts, true)
    if isempty(m); return nothing; end
    mat = cstr[m[1]+1:m[2]]
    cap = ntuple(n, i->(m[2i+1] < 0 ? nothing : cstr[m[2i+1]+1:m[2i+2]]))
    off = map(i->m[2i+1]+1, [1:n])
    RegexMatch(mat, cap, m[1]+1, off)
end
match(r::Regex, s::String, o::Int) = match(r, s, o, r.options & PCRE_EXECUTE_MASK)
match(r::Regex, s::String)         = match(r, s, 1)
