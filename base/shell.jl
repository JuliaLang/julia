# This file is a part of Julia. License is MIT: https://julialang.org/license

## shell-like command parsing ##

const shell_special = "#{}()[]<>|&*?~;"

# needs to be factored out so depwarn only warns once
# when removed, also need to update shell_escape for a Cmd to pass shell_special
# and may want to use it in the test for #10120 (currently the implementation is essentially copied there)
@noinline warn_shell_special(special) =
    depwarn("special characters \"$special\" should now be quoted in commands", :warn_shell_special)

function shell_parse(str::AbstractString, interpolate::Bool=true;
                     special::AbstractString="")
    s = lstrip(str)
    # strips the end but respects the space when the string ends with "\\ "
    r = reverse(s)
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
            update_arg(s[i:prevind(s, j)])
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
            update_arg(s[i:prevind(s, j)]); i = k; j = k
            if done(s,k)
                error("\$ right before end of command")
            end
            if isspace(s[k])
                error("space not allowed right after \$")
            end
            stpos = j
            ex, j = Meta.parse(s,j,greedy=false)
            last_parse = stpos:j
            update_arg(ex); i = j
        else
            if !in_double_quotes && c == '\''
                in_single_quotes = !in_single_quotes
                update_arg(s[i:prevind(s, j)]); i = k
            elseif !in_single_quotes && c == '"'
                in_double_quotes = !in_double_quotes
                update_arg(s[i:prevind(s, j)]); i = k
            elseif c == '\\'
                if in_double_quotes
                    if done(s,k)
                        error("unterminated double quote")
                    end
                    if s[k] == '"' || s[k] == '$' || s[k] == '\\'
                        update_arg(s[i:prevind(s, j)]); i = k
                        c, k = next(s,k)
                    end
                elseif !in_single_quotes
                    if done(s,k)
                        error("dangling backslash")
                    end
                    update_arg(s[i:prevind(s, j)]); i = k
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
    args = String[]
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

function print_shell_escaped(io::IO, cmd::AbstractString, args::AbstractString...;
                             special::AbstractString="")
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
dollar signs are considered to be special (default: none).

# Examples
```jldoctest
julia> Base.shell_escape("cat", "/foo/bar baz", "&&", "echo", "done")
"cat '/foo/bar baz' && echo done"

julia> Base.shell_escape("echo", "this", "&&", "that")
"echo this && that"
```
"""
shell_escape(args::AbstractString...; special::AbstractString="") =
    sprint(io->print_shell_escaped(io, args..., special=special))


function print_shell_escaped_posixly(io::IO, args::AbstractString...)
    first = true
    for arg in args
        first || print(io, ' ')
        # avoid printing quotes around simple enough strings
        # that any (reasonable) shell will definitely never consider them to be special
        have_single = false
        have_double = false
        function isword(c::Char)
            if '0' <= c <= '9' || 'a' <= c <= 'z' || 'A' <= c <= 'Z'
                # word characters
            elseif c == '_' || c == '/' || c == '+' || c == '-'
                # other common characters
            elseif c == '\''
                have_single = true
            elseif c == '"'
                have_double && return false # switch to single quoting
                have_double = true
            elseif !first && c == '='
                # equals is special if it is first (e.g. `env=val ./cmd`)
            else
                # anything else
                return false
            end
            return true
        end
        if all(isword, arg)
            have_single && (arg = replace(arg, '\'', "\\'"))
            have_double && (arg = replace(arg, '"', "\\\""))
            print(io, arg)
        else
            print(io, '\'', replace(arg, '\'', "'\\''"), '\'')
        end
        first = false
    end
end

"""
    shell_escape_posixly(args::Union{Cmd,AbstractString...})

The unexported `shell_escape_posixly` function
takes a string or command object and escapes any special characters in such a way that
it is safe to pass it as an argument to a posix shell.

# Examples
```jldoctest
julia> Base.shell_escape_posixly("cat", "/foo/bar baz", "&&", "echo", "done")
"cat '/foo/bar baz' '&&' echo done"

julia> Base.shell_escape_posixly("echo", "this", "&&", "that")
"echo this '&&' that"
```
"""
shell_escape_posixly(args::AbstractString...) =
    sprint(io->print_shell_escaped_posixly(io, args...))
