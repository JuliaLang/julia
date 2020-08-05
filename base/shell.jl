# This file is a part of Julia. License is MIT: https://julialang.org/license

## shell-like command parsing ##

const shell_special = "#{}()[]<>|&*?~;"

# strips the end but respects the space when the string ends with "\\ "
function rstrip_shell(s::AbstractString)
    c_old = nothing
    for (i, c) in Iterators.reverse(pairs(s))
        i::Int; c::AbstractChar
        ((c == '\\') && c_old == ' ') && return SubString(s, 1, i+1)
        isspace(c) || return SubString(s, 1, i)
        c_old = c
    end
    SubString(s, 1, 0)
end

function shell_parse(str::AbstractString, interpolate::Bool=true;
                     special::AbstractString="")
    s = SubString(str, firstindex(str))
    s = rstrip_shell(lstrip(s))

    # N.B.: This is used by REPLCompletions
    last_parse = 0:-1
    isempty(s) && return interpolate ? (Expr(:tuple,:()),last_parse) : ([],last_parse)

    in_single_quotes = false
    in_double_quotes = false

    args::Vector{Any} = []
    arg::Vector{Any} = []
    i = firstindex(s)
    st = Iterators.Stateful(pairs(s))

    function update_arg(x)
        if !isa(x,AbstractString) || !isempty(x)
            push!(arg, x)
        end
    end
    function consume_upto(s, i, j)
        update_arg(s[i:prevind(s, j)::Int])
        something(peek(st), (lastindex(s)::Int+1,'\0'))[1]
    end
    function append_arg()
        if isempty(arg); arg = Any["",]; end
        push!(args, arg)
        arg = []
    end

    for (j, c) in st
        j::Int; c::AbstractChar
        if !in_single_quotes && !in_double_quotes && isspace(c)
            i = consume_upto(s, i, j)
            append_arg()
            while !isempty(st)
                # We've made sure above that we don't end in whitespace,
                # so updating `i` here is ok
                (i, c) = peek(st)
                isspace(c) || break
                popfirst!(st)
            end
        elseif interpolate && !in_single_quotes && c == '$'
            i = consume_upto(s, i, j)
            isempty(st) && error("\$ right before end of command")
            stpos, c = popfirst!(st)
            isspace(c) && error("space not allowed right after \$")
            if startswith(SubString(s, stpos), "var\"")
                # Disallow var"#" syntax in cmd interpolations.
                # TODO: Allow only identifiers after the $ for consistency with
                # string interpolation syntax (see #3150)
                ex, j = :var, stpos+3
            else
                ex, j = Meta.parse(s,stpos,greedy=false)
            end
            last_parse = (stpos:prevind(s, j)) .+ s.offset
            update_arg(ex);
            s = SubString(s, j)
            Iterators.reset!(st, pairs(s))
            i = firstindex(s)
        else
            if !in_double_quotes && c == '\''
                in_single_quotes = !in_single_quotes
                i = consume_upto(s, i, j)
            elseif !in_single_quotes && c == '"'
                in_double_quotes = !in_double_quotes
                i = consume_upto(s, i, j)
            elseif c == '\\'
                if in_double_quotes
                    isempty(st) && error("unterminated double quote")
                    k, c′ = peek(st)
                    if c′ == '"' || c′ == '$' || c′ == '\\'
                        i = consume_upto(s, i, j)
                        _ = popfirst!(st)
                    end
                elseif !in_single_quotes
                    isempty(st) && error("dangling backslash")
                    i = consume_upto(s, i, j)
                    _ = popfirst!(st)
                end
            elseif !in_single_quotes && !in_double_quotes && c in special
                error("parsing command `$str`: special characters \"$special\" must be quoted in commands")
            end
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
    if isempty(word)
        print(io, "''")
    elseif !has_special
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
    nothing
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
    sprint((io, args...) -> print_shell_escaped(io, args..., special=special), args...)


function print_shell_escaped_posixly(io::IO, args::AbstractString...)
    first = true
    for arg in args
        first || print(io, ' ')
        # avoid printing quotes around simple enough strings
        # that any (reasonable) shell will definitely never consider them to be special
        have_single = false
        have_double = false
        function isword(c::AbstractChar)
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
        if isempty(arg)
            print(io, "''")
        elseif all(isword, arg)
            have_single && (arg = replace(arg, '\'' => "\\'"))
            have_double && (arg = replace(arg, '"' => "\\\""))
            print(io, arg)
        else
            print(io, '\'', replace(arg, '\'' => "'\\''"), '\'')
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
    sprint(print_shell_escaped_posixly, args...)


function print_shell_escaped_winsomely(io::IO, args::AbstractString...)
    first = true
    for arg in args
        first || write(io, ' ')
        first = false
        # Quote any arg that contains a whitespace (' ' or '\t') or a double quote mark '"'.
        # It's also valid to quote an arg with just a whitespace,
        # but the following may be 'safer', and both implementations are valid anyways.
        quotes = any(c -> c in (' ', '\t', '"'), arg) || isempty(arg)
        quotes && write(io, '"')
        backslashes = 0
        for c in arg
            if c == '\\'
                backslashes += 1
            else
                # escape all backslashes and the following double quote
                c == '"' && (backslashes = backslashes * 2 + 1)
                for j = 1:backslashes
                    # backslashes aren't special here
                    write(io, '\\')
                end
                backslashes = 0
                write(io, c)
            end
        end
        # escape all backslashes, letting the terminating double quote we add below to then be interpreted as a special char
        quotes && (backslashes *= 2)
        for j = 1:backslashes
            write(io, '\\')
        end
        quotes && write(io, '"')
    end
    return nothing
end


"""
     shell_escaped_winsomely(args::Union{Cmd,AbstractString...})::String

Convert the collection of strings `args` into single string suitable for passing as the argument
string for a Windows command line. Windows passes the entire command line as a single string to
the application (unlike POSIX systems, where the list of arguments are passed separately).
Many Windows API applications (including julia.exe), use the conventions of the [Microsoft C
runtime](https://docs.microsoft.com/en-us/cpp/c-language/parsing-c-command-line-arguments) to
split that command line into a list of strings. This function implements the inverse of such a
C runtime command-line parser. It joins command-line arguments to be passed to a Windows console
application into a command line, escaping or quoting meta characters such as space,
double quotes and backslash where needed. This may be useful in concert with the `windows_verbatim`
flag to [`Cmd`](@ref) when constructing process pipelines.

# Example
```jldoctest
julia> println(shell_escaped_winsomely("A B\\", "C"))
"A B\\" C
"""
shell_escape_winsomely(args::AbstractString...) =
    sprint(print_shell_escaped_winsomely, args..., sizehint=(sum(length, args)) + 3*length(args))
