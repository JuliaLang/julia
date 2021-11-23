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
                     special::AbstractString="", filename="none")
    s = SubString(str, firstindex(str))
    s = rstrip_shell(lstrip(s))

    # N.B.: This is used by REPLCompletions
    last_parse = 0:-1
    isempty(s) && return interpolate ? (Expr(:tuple,:()),last_parse) : ([],last_parse)

    in_single_quotes = false
    in_double_quotes = false

    args = []
    arg = []
    i = firstindex(s)
    st = Iterators.Stateful(pairs(s))

    function push_nonempty!(list, x)
        if !isa(x,AbstractString) || !isempty(x)
            push!(list, x)
        end
        return nothing
    end
    function consume_upto!(list, s, i, j)
        push_nonempty!(list, s[i:prevind(s, j)::Int])
        something(peek(st), lastindex(s)::Int+1 => '\0').first::Int
    end
    function append_2to1!(list, innerlist)
        if isempty(innerlist); push!(innerlist, ""); end
        push!(list, copy(innerlist))
        empty!(innerlist)
    end

    C = eltype(str)
    P = Pair{Int,C}
    for (j, c) in st
        j, c = j::Int, c::C
        if !in_single_quotes && !in_double_quotes && isspace(c)
            i = consume_upto!(arg, s, i, j)
            append_2to1!(args, arg)
            while !isempty(st)
                # We've made sure above that we don't end in whitespace,
                # so updating `i` here is ok
                (i, c) = peek(st)::P
                isspace(c) || break
                popfirst!(st)
            end
        elseif interpolate && !in_single_quotes && c == '$'
            i = consume_upto!(arg, s, i, j)
            isempty(st) && error("\$ right before end of command")
            stpos, c = popfirst!(st)::P
            isspace(c) && error("space not allowed right after \$")
            if startswith(SubString(s, stpos), "var\"")
                # Disallow var"#" syntax in cmd interpolations.
                # TODO: Allow only identifiers after the $ for consistency with
                # string interpolation syntax (see #3150)
                ex, j = :var, stpos+3
            else
                # use parseatom instead of parse to respect filename (#28188)
                ex, j = Meta.parseatom(s, stpos, filename=filename)
            end
            last_parse = (stpos:prevind(s, j)) .+ s.offset
            push_nonempty!(arg, ex)
            s = SubString(s, j)
            Iterators.reset!(st, pairs(s))
            i = firstindex(s)
        else
            if !in_double_quotes && c == '\''
                in_single_quotes = !in_single_quotes
                i = consume_upto!(arg, s, i, j)
            elseif !in_single_quotes && c == '"'
                in_double_quotes = !in_double_quotes
                i = consume_upto!(arg, s, i, j)
            elseif !in_single_quotes && c == '\\'
                if !isempty(st) && (peek(st)::P)[2] in ('\n', '\r')
                    i = consume_upto!(arg, s, i, j) + 1
                    if popfirst!(st)[2] == '\r' && (peek(st)::P)[2] == '\n'
                        i += 1
                        popfirst!(st)
                    end
                    while !isempty(st) && (peek(st)::P)[2] in (' ', '\t')
                        i = nextind(str, i)
                        _ = popfirst!(st)
                    end
                elseif in_double_quotes
                    isempty(st) && error("unterminated double quote")
                    k, c′ = peek(st)::P
                    if c′ == '"' || c′ == '$' || c′ == '\\'
                        i = consume_upto!(arg, s, i, j)
                        _ = popfirst!(st)
                    end
                else
                    isempty(st) && error("dangling backslash")
                    i = consume_upto!(arg, s, i, j)
                    _ = popfirst!(st)
                end
            elseif !in_single_quotes && !in_double_quotes && c in special
                error("parsing command `$str`: special characters \"$special\" must be quoted in commands")
            end
        end
    end

    if in_single_quotes; error("unterminated single quote"); end
    if in_double_quotes; error("unterminated double quote"); end

    push_nonempty!(arg, s[i:end])
    append_2to1!(args, arg)

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
    shell_escape_csh(args::Union{Cmd,AbstractString...})
    shell_escape_csh(io::IO, args::Union{Cmd,AbstractString...})

This function quotes any metacharacters in the string arguments such
that the string returned can be inserted into a command-line for
interpretation by the Unix C shell (csh, tcsh), where each string
argument will form one word.

In contrast to a POSIX shell, csh does not support the use of the
backslash as a general escape character in double-quoted strings.
Therefore, this function wraps strings that might contain
metacharacters in single quotes, except for parts that contain single
quotes, which it wraps in double quotes instead. It switches between
these types of quotes as needed. Linefeed characters are escaped with
a backslash.

This function should also work for a POSIX shell, except if the input
string contains a linefeed (`"\\n"`) character.

See also: [`shell_escape_posixly`](@ref)
"""
function shell_escape_csh(io::IO, args::AbstractString...)
    first = true
    for arg in args
        first || write(io, ' ')
        first = false
        i = 1
        while true
            for (r,e) = (r"^[A-Za-z0-9/\._-]+\z" => "",
                         r"^[^']*\z" => "'", r"^[^\$\`\"]*\z" => "\"",
                         r"^[^']+"  => "'", r"^[^\$\`\"]+"  => "\"")
                if ((m = match(r, SubString(arg, i))) !== nothing)
                    write(io, e)
                    write(io, replace(m.match, '\n' => "\\\n"))
                    write(io, e)
                    i += ncodeunits(m.match)
                    break
                end
            end
            i <= lastindex(arg) || break
        end
    end
end
shell_escape_csh(args::AbstractString...) =
    sprint(shell_escape_csh, args...;
           sizehint = sum(sizeof.(args)) + length(args) * 3)

"""
    shell_escape_wincmd(s::AbstractString)
    shell_escape_wincmd(io::IO, s::AbstractString)

The unexported `shell_escape_wincmd` function escapes Windows `cmd.exe` shell
meta characters. It escapes `()!^<>&|` by placing a `^` in front. An `@` is
only escaped at the start of the string. Pairs of `"` characters and the
strings they enclose are passed through unescaped. Any remaining `"` is escaped
with `^` to ensure that the number of unescaped `"` characters in the result
remains even.

Since `cmd.exe` substitutes variable references (like `%USER%`) _before_
processing the escape characters `^` and `"`, this function makes no attempt to
escape the percent sign (`%`), the presence of `%` in the input may cause
severe breakage, depending on where the result is used.

Input strings with ASCII control characters that cannot be escaped (NUL, CR,
LF) will cause an `ArgumentError` exception.

The result is safe to pass as an argument to a command call being processed by
`CMD.exe /S /C " ... "` (with surrounding double-quote pair) and will be
received verbatim by the target application if the input does not contain `%`
(else this function will fail with an ArgumentError). The presence of `%` in
the input string may result in command injection vulnerabilities and may
invalidate any claim of suitability of the output of this function for use as
an argument to cmd (due to the ordering described above), so use caution when
assembling a string from various sources.

This function may be useful in concert with the `windows_verbatim` flag to
[`Cmd`](@ref) when constructing process pipelines.

```julia
wincmd(c::String) =
   run(Cmd(Cmd(["cmd.exe", "/s /c \\" \$c \\""]);
           windows_verbatim=true))
wincmd_echo(s::String) =
   wincmd("echo " * Base.shell_escape_wincmd(s))
wincmd_echo("hello \$(ENV["USERNAME"]) & the \\"whole\\" world! (=^I^=)")
```

But take note that if the input string `s` contains a `%`, the argument list
and echo'ed text may get corrupted, resulting in arbitrary command execution.
The argument can alternatively be passed as an environment variable, which
avoids the problem with `%` and the need for the `windows_verbatim` flag:

```julia
cmdargs = Base.shell_escape_wincmd("Passing args with %cmdargs% works 100%!")
run(setenv(`cmd /C echo %cmdargs%`, "cmdargs" => cmdargs))
```

!warning
    The argument parsing done by CMD when calling batch files (either inside
    `.bat` files or as arguments to them) is not fully compatible with the
    output of this function. In particular, the processing of `%` is different.

!important
    Due to a peculiar behavior of the CMD parser/interpreter, each command
    after a literal `|` character (indicating a command pipeline) must have
    `shell_escape_wincmd` applied twice since it will be parsed twice by CMD.
    This implies ENV variables would also be expanded twice!
    For example:
    ```julia
    to_print = "All for 1 & 1 for all!"
    to_print_esc = Base.shell_escape_wincmd(Base.shell_escape_wincmd(to_print))
    run(Cmd(Cmd(["cmd", "/S /C \\" break | echo \$(to_print_esc) \\""]), windows_verbatim=true))
    ```

With an I/O stream parameter `io`, the result will be written there,
rather than returned as a string.

See also [`escape_microsoft_c_args`](@ref), [`shell_escape_posixly`](@ref).

# Example
```jldoctest
julia> Base.shell_escape_wincmd("a^\\"^o\\"^u\\"")
"a^^\\"^o\\"^^u^\\""
```
"""
function shell_escape_wincmd(io::IO, s::AbstractString)
    # https://stackoverflow.com/a/4095133/1990689
    occursin(r"[\r\n\0]", s) &&
        throw(ArgumentError("control character unsupported by CMD.EXE"))
    i = 1
    len = ncodeunits(s)
    if len > 0 && s[1] == '@'
        write(io, '^')
    end
    while i <= len
        c = s[i]
        if c == '"' && (j = findnext('"', s, nextind(s,i))) !== nothing
            write(io, SubString(s,i,j))
            i = j
        else
            if c in ('"', '(', ')', '!', '^', '<', '>', '&', '|')
                write(io, '^', c)
            else
                write(io, c)
            end
        end
        i = nextind(s,i)
    end
end
shell_escape_wincmd(s::AbstractString) = sprint(shell_escape_wincmd, s;
                                                sizehint = 2*sizeof(s))

"""
    escape_microsoft_c_args(args::Union{Cmd,AbstractString...})
    escape_microsoft_c_args(io::IO, args::Union{Cmd,AbstractString...})

Convert a collection of string arguments into a string that can be
passed to many Windows command-line applications.

Microsoft Windows passes the entire command line as a single string to
the application (unlike POSIX systems, where the shell splits the
command line into a list of arguments). Many Windows API applications
(including julia.exe), use the conventions of the [Microsoft C/C++
runtime](https://docs.microsoft.com/en-us/cpp/c-language/parsing-c-command-line-arguments)
to split that command line into a list of strings.

This function implements an inverse for a parser compatible with these rules.
It joins command-line arguments to be passed to a Windows
C/C++/Julia application into a command line, escaping or quoting the
meta characters space, TAB, double quote and backslash where needed.

See also [`shell_escape_wincmd`](@ref), [`escape_raw_string`](@ref).
"""
function escape_microsoft_c_args(io::IO, args::AbstractString...)
    # http://daviddeley.com/autohotkey/parameters/parameters.htm#WINCRULES
    first = true
    for arg in args
        if first
            first = false
        else
            write(io, ' ')  # separator
        end
        if isempty(arg) || occursin(r"[ \t\"]", arg)
            # Julia raw strings happen to use the same escaping convention
            # as the argv[] parser in Microsoft's C runtime library.
            write(io, '"')
            escape_raw_string(io, arg)
            write(io, '"')
        else
            write(io, arg)
        end
    end
end
escape_microsoft_c_args(args::AbstractString...) =
    sprint(escape_microsoft_c_args, args...;
           sizehint = (sum(sizeof.(args)) + 3*length(args)))
