# This file is a part of Julia. License is MIT: http://julialang.org/license

## shell-like command parsing ##

const shell_special = "#{}()[]<>|&*?~;"

# needs to be factored out so depwarn only warns once
@noinline warn_shell_special(special) =
    depwarn("special characters \"$special\" should now be quoted in commands", :warn_shell_special)

function shell_parse(
        str::AbstractString,
        interpolate::Bool=true;
        special::AbstractString=""
    )
    s = lstrip(str)
    # strips the end but respects the space when the string ends with "\\ "
    r = RevString(s)
    i = start(r)
    c_old = nothing
    while !done(r,i)
        c, j = next(r,i)
        if c == '\\' && c_old == ' '
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
    isempty(s) && return interpolate ? (Expr(:tuple,:()),last_parse) : ([],last_parse)

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
        elseif interpolate && !in_single_quotes && c == '$'
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
            elseif !in_single_quotes && !in_double_quotes && c in special
                warn_shell_special(special) # noinline depwarn
            end
            j = k
        end
    end

    if in_single_quotes; error("unterminated single quote"); end
    if in_double_quotes; error("unterminated double quote"); end

    update_arg(s[i:end])
    append_arg()

    interpolate || return args, last_parse

    # construct an expression
    ex = Expr(:tuple)
    for arg in args
        push!(ex.args, Expr(:tuple, arg...))
    end
    return ex, last_parse
end

function shell_split(s::AbstractString)
    parsed = shell_parse(s, false)[1]
    args = AbstractString[]
    for arg in parsed
       push!(args, string(arg...))
    end
    args
end

function print_shell_word(io::IO, word::AbstractString, special::AbstractString = "")
    if isempty(word)
        print(io, "''")
    end
    has_single = false
    has_special = false
    for c in word
        if isspace(c) || c=='\\' || c=='\'' || c=='"' || c=='$' || c in special
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

function print_shell_escaped(
        io::IO, cmd::AbstractString, args::AbstractString...;
        special::AbstractString=""
    )
    print_shell_word(io, cmd, special)
    for arg in args
        print(io, ' ')
        print_shell_word(io, arg, special)
    end
end
print_shell_escaped(io::IO; special::String="") = nothing

"""
    shell_escape(args::Union{Cmd,AbstractString...}; special::AbstractString="")

The unexported `shell_escape` function is the inverse of the unexported `shell_split` function:
it takes a string or command object and escapes any special characters in such a way that calling
`shell_split` on it would give back the array of words in the original command. The `special`
keyword argument controls what characters in addition to whitespace, backslashes, quotes and
dollar signs are considered to be special (default: none). Examples:

    julia> Base.shell_escape("cat", "/foo/bar baz", "&&", "echo", "done")
    "cat '/foo/bar baz' && echo done"

    julia> Base.shell_escape("echo", "this", "&&", "that")
    "echo this '&&' that"
"""
shell_escape(args::AbstractString...; special::AbstractString="") =
    sprint(io->print_shell_escaped(io, args..., special=special))
