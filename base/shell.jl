# This file is a part of Julia. License is MIT: http://julialang.org/license

## shell-like command parsing ##

function shell_parse(s::ByteString, interpolate::Bool)
    ccall(:jl_parse_shell_line, Any, (Cstring, Csize_t, Cint),
                                     s, sizeof(s), interpolate)
end
shell_parse(s::AbstractString) = shell_parse(bytestring(s), true)

function shell_split(s::AbstractString)
    parsed = shell_parse(bytestring(s), false)
    [e.args[1]::AbstractString for e in parsed.args]
end

function print_shell_word(io::IO, word::AbstractString)
    if isempty(word)
        print(io, "''")
    end
    has_single = false
    has_special = false
    for c in word
        if isspace(c) || c=='\\' || c=='\'' || c=='"' || c=='$'
            has_special = true
            if c == '\''
                has_single = true
            end
        end
    end
    if !has_special
        print(io, word)
    elseif !has_single
        print(io, '\'', word, '\'')
    else
        print(io, '"')
        for c in word
            if c == '"' || c == '$'
                print(io, '\\')
            end
            print(io, c)
        end
        print(io, '"')
    end
end

function print_shell_escaped(io::IO, cmd::AbstractString, args::AbstractString...)
    print_shell_word(io, cmd)
    for arg in args
        print(io, ' ')
        print_shell_word(io, arg)
    end
end
print_shell_escaped(io::IO) = nothing

shell_escape(args::AbstractString...) = sprint(print_shell_escaped, args...)
