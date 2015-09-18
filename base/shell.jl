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

"Quote by enclosing in single quotes"
function _quote_via_single_quote(word::AbstractString)
    # Quote by enclosing in single quotes, and replace all single
    # quotes with backslash-quote
    quoted = "'" * replace(word, r"'", "'\\''") * "'"
    # Remove empty leading or trailing quotes
    quoted = replace(quoted, r"^''", "")
    quoted = replace(quoted, r"'''$(?!\n)", "'")
    if isempty(quoted)
        quoted = "''"
    end
    quoted
end
"Quote by enclosing in double quotes"
function _quote_via_double_quote(word::AbstractString)
    # Quote by enclosing in double quotes and escaping certain special
    # characters with a backslash, and by escaping all exclamation
    # marks with a backslash
    quoted = "\"" * replace(word, r"(?=[$`\"\\])", "\\") * "\""
    quoted = replace(quoted, r"!", "\"\\!\"\"")
    # Remove empty leading or trailing quotes
    quoted = replace(quoted, r"^\"\"", "")
    quoted = replace(quoted, r"!\"\"$(?!\n)", "!")
    if isempty(quoted)
        quoted = "''"
    end
    quoted
end
"Quote by escaping with backslashes"
function _quote_via_backslash(word::AbstractString)
    # Quote by escaping all non-alphanumeric characters with a
    # backslash, and by enclosing all newlines with single quotes
    quoted = replace(word, r"(?=[^-0-9a-zA-Z_./\n])", "\\")
    quoted = replace(quoted, r"\n", "'\\n'")
    if isempty(quoted)
        quoted = "''"
    end
    quoted
end
function _quote_shell_word(word::AbstractString)
    # Return the shortest string
    quotes = [_quote_via_single_quote(word),
              _quote_via_double_quote(word),
              _quote_via_backslash(word)]
    shortest = findmin(map(length, quotes))[2]
    return quotes[shortest]
end

function print_shell_word(io::IO, word::AbstractString)
    print(io, _quote_shell_word(word))
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
