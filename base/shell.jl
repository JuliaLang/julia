# This file is a part of Julia. License is MIT: https://julialang.org/license

## shell-like command parsing ##

const shell_special = "#()[]<>|&*?;"

# Characters that need quoting in backtick display.  Includes {} so that a Cmd
# with literal braces round-trips correctly (they would otherwise be
# brace-expanded when the displayed string is re-parsed).
const _shell_display_special = shell_special * "{}"

(@doc raw"""
    rstrip_shell(s::AbstractString)

Strip trailing whitespace from a shell command string, while respecting a trailing backslash followed by a whitespace character (e.g. "\\ ").

```jldoctest
julia> Base.rstrip_shell("echo 'Hello World' \\ ")
"echo 'Hello World' \\ "

julia> Base.rstrip_shell("echo 'Hello World'    ")
"echo 'Hello World'"
```
"""
function rstrip_shell(s::AbstractString)
    c_old = nothing
    for (i, c) in Iterators.reverse(pairs(s))
        i::Int; c::AbstractChar
        ((c == '\\') && c_old !== nothing) && return SubString(s, 1, nextind(s, i))
        isspace(c) || return SubString(s, 1, i)
        c_old = c
    end
    SubString(s, 1, 0)
end)

# Expand a brace range pattern like "1..10", "a..z", "1..10..2", or "01..10".
# Returns a `Vector{String}`, or `nothing` if the content is not a valid range.
# As in bash, the sign of the step is ignored: the direction comes from the endpoints.
# The vector is spliced into the parsed expression as a value, so that a range of any
# length takes the same `arg_gen` method instead of specializing on a tuple type.
function _brace_range_expand(content::AbstractString)
    parts = split(content, "..")
    (length(parts) == 2 || length(parts) == 3) || return nothing
    a_str, b_str = parts[1], parts[2]
    step_str = length(parts) == 3 ? parts[3] : nothing

    # Character range: single ASCII letter endpoints, both same case
    if length(a_str) == 1 && length(b_str) == 1
        a_ch, b_ch = a_str[1], b_str[1]
        if isletter(a_ch) && isletter(b_ch) && isascii(a_ch) && isascii(b_ch)
            if isuppercase(a_ch) != isuppercase(b_ch)
                return nothing  # cross-case ranges like {a..Z} are an error
            end
            step = 1
            if step_str !== nothing
                step = tryparse(Int, step_str)
                step === nothing && return nothing
                step == 0 && return nothing
            end
            step = a_ch <= b_ch ? abs(step) : -abs(step)
            return String[string(c) for c in a_ch:step:b_ch]
        end
    end

    # Integer range
    a = tryparse(Int, a_str)
    b = tryparse(Int, b_str)
    (a === nothing || b === nothing) && return nothing
    step = 1
    if step_str !== nothing
        step = tryparse(Int, step_str)
        step === nothing && return nothing
        step == 0 && return nothing
    end
    step = a <= b ? abs(step) : -abs(step)

    # Explicit plus sign: if either endpoint starts with +, show + on positive values.
    show_plus = (a_str[1] == '+') || (b_str[1] == '+')

    # Zero-padding: if either endpoint has leading zeros (including after a
    # sign character), pad all values to the same total width (matching bash/zsh).
    _has_leading_zero(s) = (length(s) > 1 && s[1] == '0') ||
                           (length(s) > 2 && s[1] in ('-', '+') && s[2] == '0')
    pad = (_has_leading_zero(a_str) || _has_leading_zero(b_str)) ?
        max(length(a_str), length(b_str)) : 0

    if pad > 0
        return String[let s = string(abs(n))
            if n < 0
                "-" * lpad(s, pad - 1, '0')
            elseif show_plus
                "+" * lpad(s, pad - 1, '0')
            else
                lpad(s, pad, '0')
            end
        end for n in a:step:b]
    elseif show_plus
        return String[(n < 0 ? string(n) : "+" * string(n)) for n in a:step:b]
    else
        return String[string(n) for n in a:step:b]
    end
end

## Parsing backtick commands ##

# The state of a `shell_parse` call. `s` is the command without its surrounding whitespace,
# and positions are indices into `s`; adding `s.offset` gives an index into the original string.
mutable struct _ShellParser{S<:AbstractString}
    const s::S
    const str::AbstractString # the original command, for error messages
    const interpolate::Bool
    const partial::Bool       # whether the command may end in the middle, for REPLCompletions
    const sequence::Bool      # whether `;`, `&&` and `||` separate commands, for REPL shell mode
    const special::String
    const filename::String
    pos::Int                  # the next character to read
    lit::Int                  # start of the literal text not yet added to the current word
    last_arg::Int             # start of the last argument in `str`, for REPLCompletions
    update_last_arg::Bool     # whether the next character of a word starts a new argument
    in_brace::Int             # how many brace expansions are being parsed
end

_at_end(p::_ShellParser) = p.pos > lastindex(p.s)
_char(p::_ShellParser) = p.s[p.pos]
_next!(p::_ShellParser) = (p.pos = nextind(p.s, p.pos); nothing)
_at(p::_ShellParser, c::AbstractChar) = !_at_end(p) && _char(p) == c

# An unquoted glob character, `*`, `?`, `[` or `]`, in a word being parsed.
struct _GlobChar
    c::Char
end

# A brace expansion with a glob character in one of its alternatives, which are kept as parts
# until the whole word is parsed, since whether the word is a glob depends on all of it.
struct _GlobBrace
    alts::Vector{Vector{Any}}
end

# One argument a glob word produces: its literal text, the same text as a pattern, with the
# literal parts escaped, and whether it has a glob character, in which case it is globbed.
const _GlobArg = Tuple{String,String,Bool}

# Whether the character `c` at `p.pos` starts a `;` or `&&`, which end a command in a sequence.
_at_sequence_op(p::_ShellParser, c::AbstractChar) =
    c == ';' || (c == '&' && (n = nextind(p.s, p.pos); n <= lastindex(p.s) && p.s[n] == '&'))

# Throw an error for the `;`, `&&` or `||` `op` outside a sequence, as in a command literal.
function _sequence_error(p::_ShellParser, op::AbstractString)
    alt = op == ";" ? "`run(a); run(b)`" : "`success(a) $op run(b)`"
    error("parsing command `$(p.str)`: `$op` between commands is only supported in REPL shell ",
          "mode; in code, use Julia's `$op` instead, as in $alt, or quote `$op` to pass it as an argument")
end

# Throw an error for shell syntax that shell mode does not support, if `c` starts some.
function _shell_mode_syntax_error(p::_ShellParser, c::AbstractChar)
    if c == '(' || c == ')'
        error("parsing command `$(p.str)`: subshells with `( ... )` are not supported in shell ",
              "mode; quote the parentheses to pass them as arguments")
    elseif c == '&'
        error("parsing command `$(p.str)`: running a command in the background with `&` is not ",
              "supported in shell mode; quote `&` to pass it as an argument")
    end
    return nothing
end

# Add the literal text from `p.lit` up to, but not including, position `j` to `word`.
function _flush!(p::_ShellParser, word::Vector{Any}, j::Int = p.pos)
    j > p.lit && push!(word, String(p.s[p.lit:prevind(p.s, j)]))
    return nothing
end

# Record that an argument starts at the pending literal text if the previous one has ended.
# Called for every character of a word except a `$`; positions inside brace expansions are
# not recorded.
function _mark_arg!(p::_ShellParser)
    if p.update_last_arg && p.in_brace == 0
        p.last_arg = p.lit + p.s.offset
        p.update_last_arg = false
    end
    return nothing
end

# Skip a line continuation: `p.pos` is at the newline after a backslash. Consumes the
# newline (`\n` or `\r\n`) and any spaces and tabs that follow it.
function _skip_continuation!(p::_ShellParser)
    c = _char(p)
    _next!(p)
    c == '\r' && _at(p, '\n') && _next!(p)
    while !_at_end(p) && _char(p) in (' ', '\t')
        _next!(p)
    end
    return nothing
end

_at_continuation(p::_ShellParser) =
    (n = nextind(p.s, p.pos); n <= lastindex(p.s) && p.s[n] in ('\n', '\r'))

# Parse the expression after a `$`; `p.pos` is just after the `$`. Returns the expression
# and its position in `str`.
function _parse_interp!(p::_ShellParser)
    _at_end(p) && error("\$ right before end of command")
    pos = p.pos
    isspace(_char(p)) && error("space not allowed right after \$")
    if startswith(SubString(p.s, pos), "var\"")
        # Disallow var"#" syntax in cmd interpolations.
        # TODO: Allow only identifiers after the $ for consistency with
        # string interpolation syntax (see #3150)
        ex, j = :var, pos + 3
    else
        # use parseatom instead of parse to respect filename (#28188)
        ex, j = Meta.parseatom(p.s, pos, filename=p.filename, mod=Base)
    end
    p.pos = p.lit = j
    return ex, pos + p.s.offset
end

# Parse a `$` interpolation into `word`; `p.pos` is at the `$`. The interpolated expression
# starts an argument, as far as REPLCompletions is concerned.
function _interpolate!(p::_ShellParser, word::Vector{Any})
    _flush!(p, word)
    _next!(p)
    ex, pos = _parse_interp!(p)
    push!(word, ex)
    p.last_arg = pos
    p.update_last_arg = true
    return nothing
end

# Parse '...'; `p.pos` is at the opening quote.
function _parse_single_quoted!(p::_ShellParser, word::Vector{Any})
    _flush!(p, word)
    _next!(p)
    p.lit = p.pos
    while true
        _at_end(p) && error("unterminated single quote")
        _mark_arg!(p)
        c = _char(p)
        if c == '\''
            _flush!(p, word)
            _next!(p)
            p.lit = p.pos
            return nothing
        end
        _next!(p)
    end
end

# Parse "..."; `p.pos` is at the opening quote.
function _parse_double_quoted!(p::_ShellParser, word::Vector{Any})
    _flush!(p, word)
    _next!(p)
    p.lit = p.pos
    while true
        _at_end(p) && error("unterminated double quote")
        c = _char(p)
        if p.interpolate && c == '$'
            _interpolate!(p, word)
            continue
        end
        _mark_arg!(p)
        if c == '"'
            _flush!(p, word)
            _next!(p)
            p.lit = p.pos
            return nothing
        elseif c == '\\'
            bs = p.pos
            if _at_continuation(p)
                _flush!(p, word)
                _next!(p)
                _skip_continuation!(p)
                p.lit = p.pos
                continue
            end
            _next!(p)
            _at_end(p) && error("unterminated double quote")
            if _char(p) in ('"', '$', '\\')
                _flush!(p, word, bs)
                p.lit = p.pos
                _next!(p)
            end
        else
            _next!(p)
        end
    end
end

# Parse a backslash outside quotes; `p.pos` is at the backslash.
function _parse_escape!(p::_ShellParser, word::Vector{Any})
    _flush!(p, word)
    if _at_continuation(p)
        _next!(p)
        _skip_continuation!(p)
        p.lit = p.pos
    else
        _next!(p)
        _at_end(p) && error("dangling backslash")
        p.lit = p.pos # the escaped character starts the next literal text
        _next!(p)
    end
    return nothing
end

# Parse a `~` or `~user` prefix at the start of a word; `p.pos` is just after the `~`.
function _parse_tilde!(p::_ShellParser, word::Vector{Any})
    user = Any[]
    p.lit = p.pos
    while !_at_end(p)
        c = _char(p)
        if c == '\\' && _at_continuation(p)
            # A line continuation joins the lines without ending the user name.
            _flush!(p, user)
            _next!(p)
            _skip_continuation!(p)
            p.lit = p.pos
            continue
        end
        (c == '/' || isspace(c) || c in "'\"\\|<>" || (p.in_brace > 0 && c in ",{}")) && break
        if c == '$'
            _flush!(p, user)
            _next!(p)
            push!(user, first(_parse_interp!(p)))
        else
            _next!(p)
        end
    end
    _flush!(p, user)
    p.lit = p.pos
    if !_at_end(p) && _char(p) in ('\'', '"', '\\')
        # As in POSIX shells, a quoted character before the first `/` disables expansion.
        push!(word, "~")
        append!(word, user)
    elseif isempty(user)
        push!(word, :(expanduser("~")))
    elseif length(user) == 1 && (u = user[1]; u isa String)
        push!(word, :(expanduser($("~" * u))))
    else
        push!(word, :(let _u = string($(user...)); isempty(_u) ? "~" : expanduser(string('~', _u)) end))
    end
    return nothing
end

# Parse one word into `word`, a list of literal strings and parts to interpolate. At the top
# level, a word ends before unquoted whitespace or a pipeline or redirection operator. In a
# brace expansion, it ends before a `,` or `}` that ends the alternative, and unquoted
# whitespace and operators are reported in `bad` instead. Returns `(; stop, special, expanded,
# dotdot, bad, glob)`: the character the word ended before (`'\0'` at the end of the command),
# whether the word has quoted, escaped or expanded text, whether it has a brace expansion,
# whether it has an unquoted `..`, the first bad character or `'\0'`, and whether the word
# is a glob, in which case `word` holds the single expression that expands it.
function _parse_word!(p::_ShellParser, word::Vector{Any})
    special = expanded = dotdot = false
    bad = '\0'
    depth = 0 # braces in a brace expansion that are literal, as in `{a,{b}}`
    while !_at_end(p)
        c = _char(p)
        if isspace(c) || (p.interpolate && c in "|<>") || (p.sequence && _at_sequence_op(p, c))
            p.in_brace == 0 && break
            bad == '\0' && (bad = c)
            _next!(p)
            continue
        elseif p.in_brace > 0 && depth == 0 && (c == ',' || c == '}')
            break
        elseif p.interpolate && c == '$'
            special = true # so that, for instance, `$("2")>` is not a file descriptor
            _interpolate!(p, word)
            continue
        end
        _mark_arg!(p)
        if c == '\''
            special = true
            _parse_single_quoted!(p, word)
        elseif c == '"'
            special = true
            _parse_double_quoted!(p, word)
        elseif c == '\\'
            # A line continuation is removed, as in POSIX shells; any other escape quotes.
            _at_continuation(p) || (special = true)
            _parse_escape!(p, word)
        elseif p.interpolate && c == '~' && p.lit == p.pos && isempty(word)
            _flush!(p, word)
            _next!(p)
            _parse_tilde!(p, word)
        elseif p.interpolate && c == '{'
            if _parse_brace!(p, word)
                special = expanded = true
            elseif p.in_brace > 0
                depth += 1
            end
        elseif p.in_brace > 0 && c == '}'
            depth -= 1
            _next!(p)
        elseif p.interpolate && !p.partial && c in "*?[]"
            _flush!(p, word)
            push!(word, _GlobChar(c))
            p.lit = nextind(p.s, p.pos)
            _next!(p)
        elseif p.in_brace > 0 && depth == 0 && c == '.' &&
                (n = nextind(p.s, p.pos); n <= lastindex(p.s) && p.s[n] == '.')
            dotdot = true
            _next!(p)
            _next!(p)
        elseif c in p.special && bad == '\0'
            # After a bad character, the braces are not an expansion and the text is parsed
            # again outside them, where this character may be allowed, as in `{ ~#`.
            if p.in_brace == 0 && p.interpolate
                _at_sequence_op(p, c) && _sequence_error(p, c == ';' ? ";" : "&&")
                p.sequence && _shell_mode_syntax_error(p, c)
            end
            _special_error(p)
        else
            _next!(p)
        end
    end
    _flush!(p, word)
    p.lit = p.pos
    stop = _at_end(p) ? '\0' : _char(p)
    glob = p.in_brace == 0 && _glob_word!(p, word)
    special |= glob
    return (; stop, special, expanded, dotdot, bad, glob)
end

_special_error(p::_ShellParser) =
    error("parsing command `$(p.str)`: special characters \"$(p.special)\" must be quoted in commands")

# Turn a word with unquoted glob characters into the expression that expands it at run time.
# A word with an unquoted `*`, `?` or `[`, including in a brace expansion, is a glob word. Each
# argument it produces that has one of these characters is a pattern, whose other text is
# escaped so that it is matched literally, and is passed to `glob`; the others are used as
# they are, as in `{a,b*}`. Without one, a `]` is an ordinary character, unless it is special.
# Returns whether the word is a glob word.
function _glob_word!(p::_ShellParser, word::Vector{Any})
    _has_glob(word, _ -> true) || return false
    if !_has_glob(word, !=(']'))
        ']' in p.special && _special_error(p)
        parts = _no_glob(word)
        empty!(word)
        append!(word, parts)
        return false
    end
    mod = Expr(:macrocall, GlobalRef(Base, Symbol("@__MODULE__")), nothing)
    ex = Expr(:call, GlobalRef(Base, :cmd_glob_expand), mod, _glob_args(word))
    empty!(word)
    push!(word, ex)
    return true
end

# Whether `parts` has a glob character `c` for which `f(c)`, including in a brace expansion.
_has_glob(parts::Vector{Any}, f) = any(parts) do x
    x isa _GlobChar ? f(x.c) : x isa _GlobBrace ? any(alt -> _has_glob(alt, f), x.alts) : false
end

# `parts` with each glob character made literal text, for a word that is not a glob.
_no_glob(parts::Vector{Any}) = Any[x isa _GlobChar ? string(x.c) :
                                   x isa _GlobBrace ? _brace_expr(Vector{Any}[_no_glob(alt) for alt in x.alts]) : x
                                   for x in parts]

# The `_GlobArg`s a glob word produces, as a vector if the word interpolates nothing and
# otherwise as an expression that computes them. They come in the order `arg_gen` gives.
function _glob_args(parts::Vector{Any})
    args = Any[_glob_args1(x) for x in parts]
    all(x -> x isa Vector{_GlobArg}, args) && return cmd_glob_product(args...)
    return Expr(:call, GlobalRef(Base, :cmd_glob_product), args...)
end

function _glob_args1(x)
    x isa String && return _GlobArg[(x, glob_escape(x), false)]
    x isa _GlobChar && return _GlobArg[(string(x.c), string(x.c), x.c != ']')]
    x isa Vector{String} && return _GlobArg[(v, glob_escape(v), false) for v in x]
    x isa _GlobBrace || return Expr(:call, GlobalRef(Base, :cmd_glob_args), x)
    alts = Any[_glob_args(alt) for alt in x.alts]
    all(x -> x isa Vector{_GlobArg}, alts) && return _GlobArg[g for alt in alts for g in alt::Vector{_GlobArg}]
    return Expr(:call, GlobalRef(Base, :vcat), alts...)
end

# Parse a brace expansion, `{a,b}` or a range such as `{1..3}`; `p.pos` is at the `{`.
# Each alternative is parsed as a word, so quoting, escapes, interpolation and nested braces
# work as they do anywhere else. Adds the expansion to `word` and returns true, or, if the
# braces do not form an expansion, adds a literal `{` and returns false, and the text after
# it is parsed again as usual.
function _parse_brace!(p::_ShellParser, word::Vector{Any})
    _flush!(p, word)
    _next!(p)
    start = p.pos
    last_arg, update_last_arg = p.last_arg, p.update_last_arg
    p.lit = start
    p.in_brace += 1
    alts = Vector{Any}[]
    local w
    bad = '\0'
    while true
        alt = Any[]
        w = _parse_word!(p, alt)
        bad == '\0' && (bad = w.bad)
        push!(alts, alt)
        w.stop == ',' || break
        _next!(p)
        p.lit = p.pos
    end
    p.in_brace -= 1
    if w.stop == '}' && length(alts) == 1 && w.dotdot
        content = String(p.s[start:prevind(p.s, p.pos)])
        expanded = _brace_range_expand(content)
        expanded === nothing &&
            error("parsing command `$(p.str)`: invalid brace range expansion {$content}")
        push!(word, expanded)
    elseif w.stop == '}' && length(alts) > 1
        bad == '\0' ||
            error("parsing command `$(p.str)`: unquoted ", isspace(bad) ? "space" : "`$bad`", " inside braces")
        push!(word, _brace_expr(alts))
    else
        p.pos = p.lit = start
        p.last_arg, p.update_last_arg = last_arg, update_last_arg
        push!(word, "{")
        return false
    end
    _next!(p)
    p.lit = p.pos
    return true
end

# The arguments a brace expansion produces, given the parts of each alternative. If no
# alternative interpolates anything, they are computed now and returned as a `Vector{String}`,
# which `arg_gen` treats like any other collection whatever its length. Otherwise each
# alternative is expanded at run time by `arg_gen`, as a word is. If an alternative has a
# glob character, they are returned as a `_GlobBrace`, for `_glob_word!` to handle.
function _brace_expr(alts::Vector{Vector{Any}})
    any(alt -> _has_glob(alt, _ -> true), alts) && return _GlobBrace(alts)
    if all(_isstatic, alts)
        vals = String[]
        for alt in alts
            append!(vals, _brace_product(alt))
        end
        return vals
    end
    return Expr(:call, GlobalRef(Base, :vcat), Any[
        _isstatic(alt) ? _brace_product(alt) : Expr(:call, GlobalRef(Base, :arg_gen), alt...)
        for alt in alts]...)
end

_isstatic(parts::Vector{Any}) = all(x -> x isa Union{String,Vector{String}}, parts)

# The arguments a word of literal strings and brace expansions produces, as `arg_gen` would.
function _brace_product(parts::Vector{Any})
    vals = [""]
    for part in parts
        xs = part isa String ? [part] : part::Vector{String}
        vals = String[v * x for v in vals for x in xs]
    end
    return vals
end

# One command of a pipeline: its words and its redirections, in the order they were written.
# Each redirection is the stream (`:stdin`, `:stdout` or `:stderr`), its target, and its
# operator: `:in` for `<`, `:out` for `>`, `:append` for `>>`, and `:dupin` and `:dupout` for
# `<&` and `>&`, whose target is a file descriptor number.
mutable struct _ShellSegment
    args::Vector{Any}
    redirects::Vector{Tuple{Symbol,Any,Symbol}}
end
_ShellSegment() = _ShellSegment(Any[], Tuple{Symbol,Any,Symbol}[])

const _STREAM_NAMES = (:stdin, :stdout, :stderr)

# Finish a word: it becomes the target of the pending redirection, if there is one, and
# otherwise an argument. A partial parse keeps redirection targets as arguments, so that they
# can be completed like any other word. Returns the redirection still waiting for a target.
function _finish_word!(p::_ShellParser, seg::_ShellSegment, word::Vector{Any}, w, redirect::Symbol, op::Symbol)
    isempty(word) && !w.special && return redirect
    if redirect === :none || p.partial
        push!(seg.args, isempty(word) ? Any[""] : copy(word))
    else
        w.expanded && error("parsing command `$(p.str)`: brace expansion in a redirection target")
        w.glob && error("parsing command `$(p.str)`: glob in a redirection target")
        target = if op === :dupin || op === :dupout
            # As in POSIX shells, the target of `>&` and `<&` is a file descriptor number.
            amp = op === :dupin ? "<&" : ">&"
            length(word) == 1 && (d = word[1]; d isa String && !isempty(d) && all(isdigit, d)) ||
                error("parsing command `$(p.str)`: `$amp` must be followed by a file descriptor number")
            d in ("0", "1", "2") || error("parsing command `$(p.str)`: unsupported fd $d in redirection")
            parse(Int, d)
        elseif length(word) == 1 && word[1] isa String
            # `/dev/null` means `devnull` on every system, including Windows
            word[1] == "/dev/null" ? GlobalRef(Base, :devnull) : word[1]
        elseif length(word) == 1
            Expr(:call, GlobalRef(Base, :cmd_redirect_target), word[1])
        else
            Expr(:call, GlobalRef(Base, :cmd_interpolate), word...)
        end
        any(r -> r[1] === redirect, seg.redirects) &&
            error("parsing command `$(p.str)`: $redirect is redirected more than once")
        push!(seg.redirects, (redirect, target, op))
    end
    empty!(word)
    return :none
end

# The redirections are applied in the order they were written, as in sh, so that in
# `cmd >log 2>&1` the stderr of `cmd` goes to `log`: the first one is outermost.
function _segment_expr(seg::_ShellSegment)
    ex = Expr(:call, GlobalRef(Base, :cmd_gen), Expr(:tuple, Any[Expr(:tuple, w...) for w in seg.args]...))
    for (stream, target, op) in Iterators.reverse(seg.redirects)
        if op === :dupin || op === :dupout
            stream_no = findfirst(==(stream), _STREAM_NAMES)::Int - 1
            ex = Expr(:call, GlobalRef(Base, :CmdRedirect), ex,
                      Expr(:call, GlobalRef(Base, :DupRedirect), target), stream_no, op === :dupin)
        else
            kwargs = Any[Expr(:kw, stream, target)]
            # A target that is a `GlobalRef` is `devnull`, which `>>` writes to like `>`.
            op === :append && !(target isa GlobalRef) && push!(kwargs, Expr(:kw, :append, true))
            ex = Expr(:call, GlobalRef(Base, :pipeline), Expr(:parameters, kwargs...), ex)
        end
    end
    return ex
end

# Whether the pipeline `segments` has no words and no redirections, like the text between two
# `;` with nothing but spaces between them.
_isempty_command(segments::Vector{_ShellSegment}) = length(segments) == 1 &&
    (seg = segments[1]; isempty(seg.args) && isempty(seg.redirects))

# The expression that builds the command or pipeline `segments`: the tuple of words of a
# command without pipes or redirections, or a call to `pipeline`.
function _pipeline_expr(p::_ShellParser, segments::Vector{_ShellSegment})
    str = p.str
    if length(segments) == 1
        seg = segments[1]
        if isempty(seg.redirects)
            return Expr(:tuple, Any[Expr(:tuple, w...) for w in seg.args]...)
        end
        isempty(seg.args) && error("parsing command `$str`: redirection without a command")
    elseif any(seg -> isempty(seg.args), segments)
        error("parsing command `$str`: empty command in pipeline")
    end
    ex = _segment_expr(segments[1])
    for k in 2:length(segments)
        ex = Expr(:call, GlobalRef(Base, :pipeline), ex, _segment_expr(segments[k]))
    end
    return ex
end

function _ShellParser(str::AbstractString, interpolate::Bool, special::AbstractString,
                      filename, partial::Bool, sequence::Bool)
    s = rstrip_shell(lstrip(SubString(str, firstindex(str))))
    return _ShellParser(s, str, interpolate, partial, sequence, String(special), String(filename),
                        firstindex(s), firstindex(s), firstindex(str), false, 0)
end

# Read the operator that starts with `c` at `p.pos`, which a word stopped before, and return
# it: `:pipe` for `|`, `:in` for `<`, `:out` for `>`, `:append` for `>>`, `:dupin` for `<&`,
# `:dupout` for `>&`, and in a sequence
# `:and` for `&&`, `:or` for `||` and `:then` for `;`.
function _parse_operator!(p::_ShellParser, c::AbstractChar)
    _next!(p)
    if c == '|'
        # While completing, a `||` is read as two pipes, so that the last command is completed.
        (!_at(p, '|') || (p.partial && !p.sequence)) && return :pipe
        p.sequence || _sequence_error(p, "||")
        _next!(p)
        return :or
    elseif c == '>'
        if _at(p, '&')
            _next!(p)
            return :dupout
        end
        _at(p, '>') || return :out
        _next!(p)
        return :append
    elseif c == '&'
        _next!(p) # the second `&`
        return :and
    end
    c == ';' && return :then
    _at(p, '&') || return :in
    _next!(p)
    return :dupin
end

_operator_string(op::Symbol) = op === :and ? "&&" : op === :or ? "||" : ";"

# Parse a pipeline, a sequence of commands joined by `|` and each with its redirections.
# Returns its segments and the operator that ended it, `:and`, `:or` or `:then` in a sequence,
# or `nothing` at the end of the command.
function _parse_pipeline!(p::_ShellParser)
    str = p.str
    segments = [_ShellSegment()]
    redirect = :none # the redirection waiting for its target
    redirect_op = :none # its operator
    word = Any[]
    op = nothing
    while true
        w = _parse_word!(p, word)
        seg = segments[end]
        # A word of digits right before `<` or `>` is a file descriptor, as in `2>`, even
        # where a filename is expected, so `2>2>f` is missing a filename, as in sh.
        fd = nothing
        if (w.stop == '<' || w.stop == '>') && !w.special &&
                length(word) == 1 && (d = word[1]; d isa String && all(isdigit, d))
            redirect === :none || error("parsing command `$str`: redirect operator without filename")
            fd = parse(Int, d::String)
            empty!(word)
        else
            redirect = _finish_word!(p, seg, word, w, redirect, redirect_op)
        end
        w.stop == '\0' && break
        if isspace(w.stop)
            while !_at_end(p) && isspace(_char(p))
                _next!(p)
            end
        else
            redirect === :none || error("parsing command `$str`: redirect operator without filename")
            op = _parse_operator!(p, w.stop)
            if op === :pipe
                push!(segments, _ShellSegment())
            elseif op === :out || op === :append || op === :dupout
                fd === nothing && (fd = 1)
                op === :dupout || fd in (1, 2) ||
                    error("parsing command `$str`: unsupported fd $fd in redirection")
            elseif op === :in || op === :dupin
                fd === nothing && (fd = 0)
                op === :dupin || fd == 0 ||
                    error("parsing command `$str`: unsupported fd $fd in redirection")
            end
            if op in (:out, :append, :dupout, :in, :dupin)
                fd in (0, 1, 2) || error("parsing command `$str`: unsupported fd $fd in redirection")
                redirect = _STREAM_NAMES[fd + 1]
                redirect_op = op
            end
            op = op in (:and, :or, :then) ? op : nothing
        end
        p.lit = p.pos
        p.update_last_arg = true
        op === nothing || break
    end
    redirect === :none || p.partial ||
        error("parsing command `$str`: redirect operator without filename")
    return segments, op
end

# Parse a command into an expression that builds it, and return it with the position in
# `str` where the last argument starts. With `partial=true`, `str` is a command still being
# typed, which may end in the middle of a pipeline or redirection, and the expression is the
# tuple of words of its last command, as for a command without pipes or redirections.
shell_parse(str::AbstractString, interpolate::Bool=true;
            special::AbstractString="", filename="none", partial::Bool=false) =
    __repl_entry_shell_parse(str, interpolate, special, filename, partial)

# N.B.: Any functions starting with __repl_entry cut off backtraces when printing in the REPL.
function __repl_entry_shell_parse(str::AbstractString, interpolate::Bool, special::AbstractString,
                                  filename, partial::Bool=false)
    p = _ShellParser(str, interpolate, special, filename, partial, false)
    isempty(p.s) && return interpolate ? (Expr(:tuple,:()), firstindex(str)) : ([], firstindex(str))
    segments, _ = _parse_pipeline!(p)
    interpolate || return segments[1].args, p.last_arg
    if partial
        args = segments[end].args
        isempty(args) && return Expr(:tuple, :()), p.last_arg
        return Expr(:tuple, Any[Expr(:tuple, w...) for w in args]...), p.last_arg
    end
    return _pipeline_expr(p, segments), p.last_arg
end

# Parse a sequence of commands separated by `;`, `&&` and `||`, as typed in REPL shell mode,
# where these operators are not special in backticks. Returns a vector with a pair for each
# command: `:then`, `:and` or `:or` for the operator before it (`:then` for the first command
# and after a `;`), and the expression that builds it, as `shell_parse` returns. A `;` may end
# the sequence. By default, the characters that command literals reject unquoted are special,
# so that a shell mode line has the same syntax as a command literal.
shell_parse_sequence(str::AbstractString; special::AbstractString=shell_special, filename="none") =
    __repl_entry_shell_parse_sequence(str, special, filename)

function __repl_entry_shell_parse_sequence(str::AbstractString, special::AbstractString, filename)
    p = _ShellParser(str, true, special, filename, false, true)
    isempty(p.s) && return Pair{Symbol,Any}[:then => Expr(:tuple, :())]
    commands = Pair{Symbol,Any}[]
    op = :then # the operator before the command being parsed
    while true
        segments, next = _parse_pipeline!(p)
        if _isempty_command(segments)
            next === nothing || error("parsing command `$str`: empty command before `$(_operator_string(next))`")
            op === :then || error("parsing command `$str`: no command after `$(_operator_string(op))`")
            break # after a final `;`
        end
        push!(commands, op => _pipeline_expr(p, segments))
        next === nothing && break
        op = next
    end
    return commands
end

"""
    shell_split(command::AbstractString)

Split a shell command string into its individual components.

# Examples
```jldoctest
julia> Base.shell_split("git commit -m 'Initial commit'")
4-element Vector{String}:
 "git"
 "commit"
 "-m"
 "Initial commit"
```
"""
function shell_split(s::AbstractString)
    parsed = shell_parse(s, false)[1]
    args = String[]
    for arg in parsed
        push!(args, string(arg...)::String)
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
            if c == '"' || c == '$' || c == '\\'
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

The unexported `shell_escape` function is the inverse of the unexported [`Base.shell_split()`](@ref) function:
it takes a string or command object and escapes any special characters in such a way that calling
[`Base.shell_split()`](@ref) on it would give back the array of words in the original command. The `special`
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
        if isempty(arg)
            print(io, "''")
        else
            have_single = false
            have_double = false
            isword = true
            for c in arg
                if '0' <= c <= '9' || 'a' <= c <= 'z' || 'A' <= c <= 'Z'
                    # word characters
                elseif c == '_' || c == '/' || c == '+' || c == '-' || c == '.'
                    # other common characters
                elseif c == '\''
                    have_single = true
                elseif c == '"'
                    if have_double
                        isword = false
                        break # switch to single quoting
                    end
                    have_double = true
                elseif !first && c == '='
                    # equals is special if it is first (e.g. `env=val ./cmd`)
                else
                    # anything else
                    isword = false
                    break
                end
            end
            if isword && !(first && arg in ("case", "do", "done", "elif", "else", "esac", "fi", "for",
                                            "if", "in", "then", "until", "while", "function", "select", "time"))
                have_single && (arg = replace(arg, '\'' => "\\'"))
                have_double && (arg = replace(arg, '"' => "\\\""))
                print(io, arg)
            else
                print(io, '\'', replace(arg, '\'' => "'\\''"), '\'')
            end
        end
        first = false
    end
end

"""
    shell_escape_posixly(args::Union{Cmd,AbstractString...})

The unexported `shell_escape_posixly` function
takes a string or command object and escapes any special characters in such a way that
it is safe to pass it as an argument to a posix shell.

See also: [`Base.shell_escape()`](@ref)

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

See also: [`Base.shell_escape_posixly()`](@ref)
"""
function shell_escape_csh(io::IO, args::AbstractString...)
    first = true
    for arg in args
        first || write(io, ' ')
        first = false
        i = 1
        while true
            for (r,e) = (r"^[A-Za-z0-9/\._-]+\z"sa => "",
                         r"^[^']*\z"sa => "'", r"^[^\$\`\"]*\z"sa => "\"",
                         r"^[^']+"sa  => "'", r"^[^\$\`\"]+"sa  => "\"")
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
           sizehint = sum(sizeof, args; init=0) + length(args) * 3)

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

!!! warning
    The argument parsing done by CMD when calling batch files (either inside
    `.bat` files or as arguments to them) is not fully compatible with the
    output of this function. In particular, the processing of `%` is different.

!!! important
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

See also [`Base.escape_microsoft_c_args()`](@ref), [`Base.shell_escape_posixly()`](@ref).

# Examples
```jldoctest
julia> Base.shell_escape_wincmd("a^\\"^o\\"^u\\"")
"a^^\\"^o\\"^^u^\\""
```
"""
function shell_escape_wincmd(io::IO, s::AbstractString)
    # https://stackoverflow.com/a/4095133/1990689
    occursin(r"[\r\n\0]"sa, s) &&
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

See also [`Base.shell_escape_wincmd()`](@ref), [`Base.escape_raw_string()`](@ref).
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
        if isempty(arg) || occursin(r"[ \t\"]"sa, arg)
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
           sizehint = (sum(sizeof, args; init=0) + 3*length(args)))
