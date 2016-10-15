# This file is a part of Julia. License is MIT: http://julialang.org/license

## shell-like command parsing ##

function shell_parse(raw::AbstractString, interp::Bool)
    s = lstrip(raw)
    #Strips the end but respects the space when the string endswith "\\ "
    r = RevString(s)
    i = start(r)
    c_old = nothing
    while !done(r,i)
        c, j = next(r,i)
        if isequal(c, '\\') && isequal(c_old, ' ')
            i -= 1
            break
        elseif !(c in _default_delims)
            break
        end
        i = j
        c_old = c
    end
    s = s[1:end-i+1]

    last_parse = 0:-1
    isempty(s) && return interp ? (Expr(:tuple,:()),last_parse) : ([],last_parse)

    in_single_quotes = false
    in_double_quotes = false

    args::Vector{Any} = []
    arg::Vector{Any} = []
    i = start(s)
    j = i

    function update_arg(x)
        if !isa(x,AbstractString) || !isempty(x)
            push!(arg, x)
        end
    end
    function append_arg()
        if isempty(arg); arg = Any["",]; end
        push!(args, arg)
        arg = []
    end

    while !done(s,j)
        c, k = next(s,j)
        if !in_single_quotes && !in_double_quotes && isspace(c)
            update_arg(s[i:j-1])
            append_arg()
            j = k
            while !done(s,j)
                c, k = next(s,j)
                if !isspace(c)
                    i = j
                    break
                end
                j = k
            end
        elseif interp && !in_single_quotes && c == '$'
            update_arg(s[i:j-1]); i = k; j = k
            if done(s,k)
                error("\$ right before end of command")
            end
            if isspace(s[k])
                error("space not allowed right after \$")
            end
            stpos = j
            ex, j = parse(s,j,greedy=false)
            last_parse = stpos:j
            update_arg(esc(ex)); i = j
        else
            if !in_double_quotes && c == '\''
                in_single_quotes = !in_single_quotes
                update_arg(s[i:j-1]); i = k
            elseif !in_single_quotes && c == '"'
                in_double_quotes = !in_double_quotes
                update_arg(s[i:j-1]); i = k
            elseif c == '\\'
                if in_double_quotes
                    if done(s,k)
                        error("unterminated double quote")
                    end
                    if s[k] == '"' || s[k] == '$' || s[k] == '\\'
                        update_arg(s[i:j-1]); i = k
                        c, k = next(s,k)
                    end
                elseif !in_single_quotes
                    if done(s,k)
                        error("dangling backslash")
                    end
                    update_arg(s[i:j-1]); i = k
                    c, k = next(s,k)
                end
            end
            j = k
        end
    end

    if in_single_quotes; error("unterminated single quote"); end
    if in_double_quotes; error("unterminated double quote"); end

    update_arg(s[i:end])
    append_arg()

    if !interp
        return (args,last_parse)
    end

    # construct an expression
    ex = Expr(:tuple)
    for arg in args
        push!(ex.args, Expr(:tuple, arg...))
    end
    (ex,last_parse)
end
shell_parse(s::AbstractString) = shell_parse(s,true)

function shell_split(s::AbstractString)
    parsed = shell_parse(s,false)[1]
    args = AbstractString[]
    for arg in parsed
       push!(args, string(arg...))
    end
    args
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
