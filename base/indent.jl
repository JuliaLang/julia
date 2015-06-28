# This file is a part of Julia. License is MIT: http://julialang.org/license

## Count indentation, unindent ##

function blank_width(c::Char)
    c == ' '   ? 1 :
    c == '\t'  ? 8 :
    throw(ArgumentError("$(repr(c)) not a blank character"))
end

# width of leading blank space, also check if string is blank
function indentation(s::AbstractString)
    count = 0
    for c in s
        if c == ' ' || c == '\t'
            count += blank_width(c)
        else
            return count, false
        end
    end
    count, true
end

function unindent(s::AbstractString, indent::Int)
    indent == 0 && return s
    buf = IOBuffer(Array(UInt8,endof(s)), true, true)
    truncate(buf,0)
    a = i = start(s)
    cutting = false
    cut = 0
    while !done(s,i)
        c,i_ = next(s,i)
        if cutting && (c == ' ' || c == '\t')
            a = i_
            cut += blank_width(c)
            if cut == indent
                cutting = false
            elseif cut > indent
                cutting = false
                for _ = (indent+1):cut write(buf, ' ') end
            end
        elseif c == '\n'
            print(buf, s[a:i])
            a = i_
            cutting = true
            cut = 0
        else
            cutting = false
        end
        i = i_
    end
    print(buf, s[a:end])
    takebuf_string(buf)
end
