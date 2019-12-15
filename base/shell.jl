# This file is a part of Julia. License is MIT: https://julialang.org/license

## shell-like command parsing ##

const shell_special = "#{}()[]<>|&*?~;"

# strips the end but respects the space when the string ends with "\\ "
function rstrip_shell(s::AbstractString)
    c_old = nothing
    for (i, c) in Iterators.reverse(pairs(s))
        ((c == '\\') && c_old == ' ') && return SubString(s, 1, i+1)
        isspace(c) || return SubString(s, 1, i)
        c_old = c
    end
    SubString(s, 1, 0)
end


# needs to be factored out so depwarn only warns once
# when removed, also need to update shell_escape for a Cmd to pass shell_special
# and may want to use it in the test for #10120 (currently the implementation is essentially copied there)
@noinline warn_shell_special(str,special) =
    depwarn("Parsing command \"$str\". Special characters \"$special\" should now be quoted in commands", :warn_shell_special)

function shell_parse(str::AbstractString, interpolate::Bool=true;
                     special::AbstractString="")
    s::SubString = SubString(str, firstindex(str))
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
    function consume_upto(j)
        update_arg(s[i:prevind(s, j)])
        i = something(peek(st), (lastindex(s)+1,'\0'))[1]
    end
    function append_arg()
        if isempty(arg); arg = Any["",]; end
        push!(args, arg)
        arg = []
    end

    for (j, c) in st
        if !in_single_quotes && !in_double_quotes && isspace(c)
            consume_upto(j)
            append_arg()
            while !isempty(st)
                # We've made sure above that we don't end in whitespace,
                # so updating `i` here is ok
                (i, c) = peek(st)
                isspace(c) || break
                popfirst!(st)
            end
        elseif interpolate && !in_single_quotes && c == '$'
            consume_upto(j)
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
                consume_upto(j)
            elseif !in_single_quotes && c == '"'
                in_double_quotes = !in_double_quotes
                consume_upto(j)
            elseif c == '\\'
                if in_double_quotes
                    isempty(st) && error("unterminated double quote")
                    k, c′ = peek(st)
                    if c′ == '"' || c′ == '$' || c′ == '\\'
                        consume_upto(j)
                        _ = popfirst!(st)
                    end
                elseif !in_single_quotes
                    isempty(st) && error("dangling backslash")
                    consume_upto(j)
                    _ = popfirst!(st)
                end
            elseif !in_single_quotes && !in_double_quotes && c in special
                warn_shell_special(str,special) # noinline depwarn
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


"""
    shell_escape_wincmd(s::AbstractString)
    shell_escape_wincmd(io, s::AbstractString)

This function escapes the meta characters `()!^<>&|` processed by the
Windows `cmd.exe` shell: it places a `^` in front of any metacharacter
that follows an even number of quotation marks on the command line.

The percent sign (`%`) is not escaped, therefore shell variable
references (like `%USER%`) will still be substituted by `cmd.exe`.

Input strings should avoid ASCII control characters, as many of these
cannot be escaped (e.g., NUL, CR, LF).

See also: [`escape_microsoft_c_args`](@ref), [`shell_escape_posixly`](@ref)

# Example
```jldoctest
julia> println(shell_escape_wincmd(escape_microsoft_c_args("^\\\"^\\", "^ C")))
"^\\\\\\"^^\\\\" "^^ C"
```
"""
function shell_escape_wincmd(io, s::AbstractString)
    # https://stackoverflow.com/a/4095133/1990689
    quoted = false
    for c in s
        if c == '"'
            quoted = !quoted
        else
            if !quoted && c in ( '(', ')', '!', '^', '<', '>', '&', '|' )
                write(io, '^')
            end
        end
        write(io, c)
    end
end
shell_escape_wincmd(s::AbstractString) = sprint(shell_escape_wincmd, s;
                                                sizehint = sizeof(s))

"""
    escape_microsoft_c_args(args::Union{Cmd,AbstractString...})
    escape_microsoft_c_args(io, args::Union{Cmd,AbstractString...})

Convert a collection of string arguments into a string that can be
passed to many Windows command-line applications.

Microsoft Windows passes the entire command line as a single string to
the application (unlike POSIX systems, where the shell splits the
command line into a list of arguments). Many Windows API applications
(including julia.exe), use the conventions of the [Microsoft C/C++
runtime](https://docs.microsoft.com/en-us/cpp/c-language/parsing-c-command-line-arguments)
to split that command line into a list of strings.

This function implements the inverse of such a C runtime command-line
parser. It joins command-line arguments to be passed to a Windows
C/C++/Julia application into a command line, escaping or quoting the
meta characters space, TAB, double quote and backslash where needed.

See also: [`shell_escape_wincmd`](@ref), [`escape_raw_string`](@ref)
"""
function escape_microsoft_c_args(io, args::AbstractString...)
    # http://daviddeley.com/autohotkey/parameters/parameters.htm#WINCRULES
    first = true
    for arg in args
        if first
            first = false
        else
            write(io, ' ')  # separator
        end
        # Any use of r"[ \t\"]" below causes "error during bootstrap"
        # when make builds target usr/lib/julia/sys.ji !?!
        if isempty(arg) ||
            (occursin(' ', arg) ||
             occursin('\t', arg) ||
             occursin('\"', arg))
            # Julia raw strings happen to use the same escaping convention
            # as the argv[] parser in Microsoft's C runtime library.
            escape_raw_string(io, arg)
        else
            write(io, arg)
        end
    end
end
escape_microsoft_c_args(args::AbstractString...) =
    sprint(escape_microsoft_c_args, args...;
           sizehint = (sum(sizeof.(args)) + 3*length(args)))
# The following two lines also cause "error during bootstrap" !?!
#escape_microsoft_c_args(cmd::Cmd) = escape_microsoft_c_args(cmd.exec...)
#escape_microsoft_c_args(io, cmd::Cmd) = escape_microsoft_c_args(io, cmd.exec...)

# alias for an earlier implementation (to be removed)
shell_escape_winsomely(args::AbstractString...) =
    escape_microsoft_c_args(args...)
print_shell_escaped_winsomely(io::IO, args::AbstractString...) =
    escape_microsoft_c_args(io, args...)
