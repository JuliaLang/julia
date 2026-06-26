# This file is a part of Julia. License is MIT: https://julialang.org/license

## shell-like command parsing ##

const shell_special = "#()[]<>|&*?;"

# Characters that need quoting in backtick display.  Includes {} so that a Cmd
# with literal braces round-trips correctly (they would otherwise be
# brace-expanded when the displayed string is re-parsed).
const _shell_display_special = shell_special * "{}"

(@doc raw"""
    rstrip_shell(s::AbstractString)

Strip trailing whitespace from a shell command string, while respecting a trailing backslash followed by a space ("\\ ").

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
        ((c == '\\') && c_old == ' ') && return SubString(s, 1, i+1)
        isspace(c) || return SubString(s, 1, i)
        c_old = c
    end
    SubString(s, 1, 0)
end)

# Scan from position `start` in string `s` (after an opening `{`) to find the
# matching `}`. Returns `(found_close, has_comma, has_dotdot, has_space)`.
function _scan_brace(s, start)
    depth = 1
    has_comma = false
    has_dotdot = false
    has_space = false
    pos = start
    @inbounds while pos <= lastindex(s)
        c = s[pos]
        if c == '\\'
            pos = nextind(s, pos)  # skip escaped char
        elseif c == '\''
            # skip single-quoted content
            pos = nextind(s, pos)
            while pos <= lastindex(s) && s[pos] != '\''
                pos = nextind(s, pos)
            end
        elseif c == '"'
            # skip double-quoted content
            pos = nextind(s, pos)
            while pos <= lastindex(s)
                s[pos] == '"' && break
                s[pos] == '\\' && (pos = nextind(s, pos))
                pos = nextind(s, pos)
            end
        elseif c == '$' && pos < lastindex(s) && s[nextind(s, pos)] == '('
            # skip $(expr) — count parentheses
            pos = nextind(s, nextind(s, pos))
            pdepth = 1
            while pos <= lastindex(s) && pdepth > 0
                s[pos] == '(' && (pdepth += 1)
                s[pos] == ')' && (pdepth -= 1)
                pdepth > 0 && (pos = nextind(s, pos))
            end
        elseif isspace(c)
            has_space = true
        elseif c == '.' && depth == 1 && pos < lastindex(s) && s[nextind(s, pos)] == '.'
            has_dotdot = true
            pos = nextind(s, pos)  # skip second dot
        elseif c == '{'
            depth += 1
        elseif c == '}'
            depth -= 1
            depth == 0 && return (true, has_comma, has_dotdot, has_space)
        elseif c == ',' && depth == 1
            has_comma = true
        end
        pos = nextind(s, pos)
    end
    return (false, false, false, false)
end

# Expand a brace range pattern like "1..10", "a..z", "1..10..2", or "01..10".
# Returns a tuple of strings, or `nothing` if the content is not a valid range.
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
            step = a_ch <= b_ch ? step : -step
            return Tuple(string(c) for c in a_ch:step:b_ch)
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
    step = a <= b ? step : -step

    # Explicit plus sign: if either endpoint starts with +, show + on positive values.
    show_plus = (a_str[1] == '+') || (b_str[1] == '+')

    # Zero-padding: if either endpoint has leading zeros (including after a
    # sign character), pad all values to the same total width (matching bash/zsh).
    _has_leading_zero(s) = (length(s) > 1 && s[1] == '0') ||
                           (length(s) > 2 && s[1] in ('-', '+') && s[2] == '0')
    pad = (_has_leading_zero(a_str) || _has_leading_zero(b_str)) ?
        max(length(a_str), length(b_str)) : 0

    if pad > 0
        return Tuple(let s = string(abs(n))
            if n < 0
                "-" * lpad(s, pad - 1, '0')
            elseif show_plus
                "+" * lpad(s, pad - 1, '0')
            else
                lpad(s, pad, '0')
            end
        end for n in a:step:b)
    elseif show_plus
        return Tuple((n < 0 ? string(n) : "+" * string(n)) for n in a:step:b)
    else
        return Tuple(string(n) for n in a:step:b)
    end
end

# Convert a list of parts (strings and expressions) for one brace alternative
# into a single expression suitable for inclusion in a tuple.
function _brace_alt_expr(parts)
    if isempty(parts)
        return ""
    end
    all_strings = all(p -> isa(p, AbstractString), parts)
    if all_strings
        return String(join(parts))
    elseif length(parts) == 1
        return parts[1]
    else
        cleaned = Any[isa(p, AbstractString) ? String(p) : p for p in parts]
        return Expr(:call, GlobalRef(Base, :cmd_interpolate), cleaned...)
    end
end

## Helper functions for shell_parse ##

function _push_nonempty!(list, x)
    if !isa(x,AbstractString) || !isempty(x)
        push!(list, x)
    end
    return nothing
end

function _consume_upto!(list, st, s, i, j)
    _push_nonempty!(list, s[i:prevind(s, j)::Int])
    something(peek(st), lastindex(s)::Int+1 => '\0').first::Int
end

function _append_word!(list, innerlist)
    if isempty(innerlist); push!(innerlist, ""); end
    push!(list, copy(innerlist))
    empty!(innerlist)
end

function _redirect_word_expr(word)
    if length(word) == 1 && isa(word[1], AbstractString)
        return String(word[1])
    else
        return Expr(:call, GlobalRef(Base, :cmd_interpolate), word...)
    end
end

# Parse the expression following a `$` interpolation marker.
# Pops the first char of the expression from `st`, parses the atom, advances
# `s` past it, and resets `st`. Returns `(expr, new_s, interp_pos)` where
# `interp_pos` is the absolute position in `str` (for REPLCompletions).
function _parse_dollar_interp(st, s, ::Type{P}, filename) where P
    isempty(st) && error("\$ right before end of command")
    stpos, c = popfirst!(st)::P
    isspace(c) && error("space not allowed right after \$")
    if startswith(SubString(s, stpos), "var\"")
        # Disallow var"#" syntax in cmd interpolations.
        # TODO: Allow only identifiers after the $ for consistency with
        # string interpolation syntax (see #3150)
        atom, j = :var, stpos+3
    else
        # use parseatom instead of parse to respect filename (#28188)
        atom, j = Meta.parseatom(s, stpos, filename=filename)
    end
    interp_pos = stpos + s.offset
    s = SubString(s, j)
    Iterators.reset!(st, pairs(s))
    return atom, s, interp_pos
end

# Redirect state for the current pipeline segment.
mutable struct _RedirectState
    stdin::Any
    stdout::Any
    stdout_append::Bool
    stderr::Any
    stderr_append::Bool
    mode::Symbol  # :none | :stdin | :stdout | :stderr
end
_RedirectState() = _RedirectState(nothing, nothing, false, nothing, false, :none)

function _reset_redirect!(r::_RedirectState)
    r.stdin = nothing
    r.stdout = nothing
    r.stdout_append = false
    r.stderr = nothing
    r.stderr_append = false
    r.mode = :none
    return nothing
end

# Finalize the current word: if we're in redirect mode, assign the word as the redirect
# filename; otherwise append it as a command argument.
function _finalize_word!(args, arg, redir::_RedirectState, word_has_special::Bool)
    if redir.mode != :none && (!isempty(arg) || word_has_special)
        re = _redirect_word_expr(arg)
        if redir.mode === :stdin; redir.stdin = re
        elseif redir.mode === :stdout; redir.stdout = re
        else; redir.stderr = re; end
        empty!(arg)
        redir.mode = :none
    elseif redir.mode == :none && (!isempty(arg) || word_has_special)
        _append_word!(args, arg)
    end
    return nothing
end

# Save the current pipeline segment and reset state for the next segment.
function _save_pipeline_segment!(pipeline_parts, args, redir::_RedirectState)
    seg_tuple = Expr(:tuple)
    for a in args; push!(seg_tuple.args, Expr(:tuple, a...)); end
    push!(pipeline_parts, (seg_tuple, redir.stdin, redir.stdout, redir.stdout_append,
                           redir.stderr, redir.stderr_append))
    empty!(args)
    _reset_redirect!(redir)
    return nothing
end

# Parse tilde expansion (~, ~user, ~$var) at the start of a word.
# `i` should point to the position just after the `~` character.
# Returns (s, i).
function _parse_tilde!(arg, st, s, i, ::Type{P}, filename) where P
    user_parts = []
    user_lit_start = i
    while !isempty(st)
        nxt = peek(st)::P
        nc = nxt[2]
        (nc == '/' || isspace(nc) || nc == '\'' || nc == '"' || nc == '\\' ||
         nc == '|' || nc == '<' || nc == '>') && break
        if nc == '$'
            _push_nonempty!(user_parts, s[user_lit_start:prevind(s, nxt[1])])
            popfirst!(st)  # consume $
            atom, s, _ = _parse_dollar_interp(st, s, P, filename)
            push!(user_parts, atom)
            i = firstindex(s)
            user_lit_start = i
        else
            popfirst!(st)
        end
    end
    user_end = something(peek(st), (lastindex(s) + 1) => '\0').first
    _push_nonempty!(user_parts, s[user_lit_start:prevind(s, user_end)])
    if isempty(user_parts)
        push!(arg, :(expanduser("~")))
    elseif length(user_parts) == 1 && isa(user_parts[1], AbstractString)
        push!(arg, :(expanduser($("~" * user_parts[1]))))
    else
        push!(arg, :(let _u = string($(user_parts...)); isempty(_u) ? "~" : expanduser(string('~', _u)) end))
    end
    return s, user_end
end

# Parse comma-separated brace alternatives: {alt1,alt2,...}
# `i` should point to the position just after the opening `{`.
# Returns (alts, s, i, last_arg, update_last_arg).
function _parse_brace_alts!(st, s, i, ::Type{P}, str, filename, last_arg, update_last_arg) where P
    alts = Any[]
    alt_parts = Any[]
    alt_i = i   # start of current literal segment
    bsq = false # single quotes within brace content
    bdq = false # double quotes within brace content
    bdepth = 0  # nested brace depth
    while !isempty(st)
        (bp, bc) = peek(st)::P
        if bsq
            # In single quotes: everything is literal until closing '
            popfirst!(st)
            if bc == '\''
                _push_nonempty!(alt_parts, s[alt_i:prevind(s, bp)::Int])
                bsq = false
                alt_i = something(peek(st), (lastindex(s)::Int+1) => '\0').first::Int
            end
        elseif !bdq && bc == '\''
            _push_nonempty!(alt_parts, s[alt_i:prevind(s, bp)::Int])
            popfirst!(st)
            bsq = true
            alt_i = something(peek(st), (lastindex(s)::Int+1) => '\0').first::Int
        elseif bc == '"'
            _push_nonempty!(alt_parts, s[alt_i:prevind(s, bp)::Int])
            popfirst!(st)
            bdq = !bdq
            alt_i = something(peek(st), (lastindex(s)::Int+1) => '\0').first::Int
        elseif !bsq && bc == '$'
            _push_nonempty!(alt_parts, s[alt_i:prevind(s, bp)::Int])
            popfirst!(st)
            atom, s, interp_pos = _parse_dollar_interp(st, s, P, filename)
            push!(alt_parts, atom)
            last_arg = interp_pos
            update_last_arg = true
            alt_i = firstindex(s)
        elseif !bsq && bc == '\\'
            _push_nonempty!(alt_parts, s[alt_i:prevind(s, bp)::Int])
            popfirst!(st)  # consume \
            if bdq
                if !isempty(st) && (peek(st)::P).second in ('"', '$', '\\')
                    alt_i = (peek(st)::P).first::Int
                    popfirst!(st)
                else
                    push!(alt_parts, "\\")
                    alt_i = something(peek(st), (lastindex(s)::Int+1) => '\0').first::Int
                end
            else
                isempty(st) && error("parsing command `$str`: dangling backslash in brace expansion")
                alt_i = (peek(st)::P).first::Int
                popfirst!(st)
            end
        elseif !bsq && !bdq && bc == '{'
            bdepth += 1
            popfirst!(st)
        elseif !bsq && !bdq && bc == '}' && bdepth > 0
            bdepth -= 1
            popfirst!(st)
        elseif !bsq && !bdq && bc == '}' && bdepth == 0
            # End of brace expansion
            _push_nonempty!(alt_parts, s[alt_i:prevind(s, bp)::Int])
            push!(alts, _brace_alt_expr(alt_parts))
            popfirst!(st)
            break
        elseif !bsq && !bdq && bc == ',' && bdepth == 0
            # Alternative separator
            _push_nonempty!(alt_parts, s[alt_i:prevind(s, bp)::Int])
            push!(alts, _brace_alt_expr(alt_parts))
            empty!(alt_parts)
            popfirst!(st)
            alt_i = something(peek(st), (lastindex(s)::Int+1) => '\0').first::Int
        else
            popfirst!(st)
        end
    end
    i = something(peek(st), (lastindex(s)::Int+1) => '\0').first::Int
    return alts, s, i, last_arg, update_last_arg
end

# Parse brace expansion at current position.
# `i` should point to the position just after the opening `{`.
# Returns (s, i, word_has_special, last_arg, update_last_arg).
function _parse_brace!(arg, st, s, i, ::Type{P}, str, filename, last_arg, update_last_arg) where P
    found_close, has_comma, has_dotdot, has_space = _scan_brace(s, i)
    if found_close && has_dotdot && !has_comma
        # Range expansion: {1..10}, {a..z}, {1..10..2}, {01..10}
        brace_content = ""
        while !isempty(st)
            (bp, bc) = peek(st)::P
            if bc == '}'
                brace_content = String(s[i:prevind(s, bp)::Int])
                popfirst!(st)
                break
            end
            popfirst!(st)
        end
        i = something(peek(st), (lastindex(s)::Int+1) => '\0').first::Int
        expanded = _brace_range_expand(brace_content)
        if expanded === nothing
            error("parsing command `$str`: invalid brace range expansion {$brace_content}")
        end
        push!(arg, Expr(:tuple, expanded...))
        return s, i, true, last_arg, update_last_arg
    elseif found_close && has_comma && has_space
        error("parsing command `$str`: unquoted space inside braces")
    elseif !(found_close && has_comma)
        # Not a valid brace expansion (no matching } or no comma):
        # treat the { as a literal character.
        push!(arg, "{")
        return s, i, false, last_arg, update_last_arg
    else
        alts, s, i, last_arg, update_last_arg =
            _parse_brace_alts!(st, s, i, P, str, filename, last_arg, update_last_arg)
        push!(arg, Expr(:tuple, alts...))
        return s, i, true, last_arg, update_last_arg
    end
end

# Build expression for a single pipeline segment with optional redirections.
function _build_seg_expr(seg_tuple, s_in, s_out, s_out_app, s_err, s_err_app)
    cmd_ex = Expr(:call, GlobalRef(Base, :cmd_gen), seg_tuple)
    if s_in !== nothing || s_out !== nothing
        kwargs = Any[]
        s_in !== nothing && push!(kwargs, Expr(:kw, :stdin, s_in))
        s_out !== nothing && push!(kwargs, Expr(:kw, :stdout, s_out))
        s_out_app && push!(kwargs, Expr(:kw, :append, true))
        cmd_ex = Expr(:call, GlobalRef(Base, :pipeline), Expr(:parameters, kwargs...), cmd_ex)
    end
    if s_err !== nothing
        kwargs = Any[Expr(:kw, :stderr, s_err)]
        s_err_app && push!(kwargs, Expr(:kw, :append, true))
        cmd_ex = Expr(:call, GlobalRef(Base, :pipeline), Expr(:parameters, kwargs...), cmd_ex)
    end
    return cmd_ex
end

# Build the final expression for a pipeline with redirections.
function _build_pipeline_expr(pipeline_parts, args, redir::_RedirectState)
    final_seg_tuple = Expr(:tuple)
    for a in args; push!(final_seg_tuple.args, Expr(:tuple, a...)); end

    if isempty(pipeline_parts)
        # Only redirects, no pipes.
        return _build_seg_expr(final_seg_tuple, redir.stdin, redir.stdout, redir.stdout_append,
                               redir.stderr, redir.stderr_append)
    end

    # Chain pipeline segments left-to-right: pipeline(seg1, pipeline(seg2, seg3, ...)).
    (s0_args, s0_in, s0_out, s0_out_app, s0_err, s0_err_app) = pipeline_parts[1]
    result = _build_seg_expr(s0_args, s0_in, s0_out, s0_out_app, s0_err, s0_err_app)
    for k in 2:length(pipeline_parts)
        (sk_args, sk_in, sk_out, sk_out_app, sk_err, sk_err_app) = pipeline_parts[k]
        result = Expr(:call, GlobalRef(Base, :pipeline), result,
                      _build_seg_expr(sk_args, sk_in, sk_out, sk_out_app, sk_err, sk_err_app))
    end
    result = Expr(:call, GlobalRef(Base, :pipeline), result,
                  _build_seg_expr(final_seg_tuple, redir.stdin, redir.stdout, redir.stdout_append,
                                   redir.stderr, redir.stderr_append))
    return result
end

## Main shell_parse entry point ##

shell_parse(str::AbstractString, interpolate::Bool=true;
            special::AbstractString="", filename="none") =
    __repl_entry_shell_parse(str, interpolate, special, filename)

# N.B.: Any functions starting with __repl_entry cut off backtraces when printing in the REPL.
function __repl_entry_shell_parse(str::AbstractString, interpolate::Bool, special::AbstractString, filename)
    last_arg = firstindex(str) # N.B.: This is used by REPLCompletions
    s = SubString(str, last_arg)
    s = rstrip_shell(lstrip(s))

    isempty(s) && return interpolate ? (Expr(:tuple,:()), last_arg) : ([], last_arg)

    in_single_quotes = false
    in_double_quotes = false

    args = []
    arg = []
    i = firstindex(s)
    st = Iterators.Stateful(pairs(s))
    update_last_arg = false # true after spaces or interpolate
    word_has_special = false # true if current word contains any quoted/escaped content

    pipeline_parts = []
    redir = _RedirectState()

    C = eltype(str)
    P = Pair{Int,C}

    for (j, c) in st
        j, c = j::Int, c::C
        if !in_single_quotes && !in_double_quotes && isspace(c)
            update_last_arg = true
            i = _consume_upto!(arg, st, s, i, j)
            _finalize_word!(args, arg, redir, word_has_special)
            word_has_special = false
            while !isempty(st)
                (i, c) = peek(st)::P
                isspace(c) || break
                popfirst!(st)
            end
        elseif interpolate && !in_single_quotes && c == '$'
            i = _consume_upto!(arg, st, s, i, j)
            atom, s, interp_pos = _parse_dollar_interp(st, s, P, filename)
            last_arg = interp_pos
            update_last_arg = true
            push!(arg, atom)
            i = firstindex(s)
        elseif interpolate && !in_single_quotes && !in_double_quotes && c == '|'
            # Pipeline operator: finalize current word/redirect, then save segment.
            i = _consume_upto!(arg, st, s, i, j)
            _finalize_word!(args, arg, redir, word_has_special)
            _save_pipeline_segment!(pipeline_parts, args, redir)
            word_has_special = false
            update_last_arg = true
        elseif interpolate && !in_single_quotes && !in_double_quotes && (c == '<' || c == '>')
            # Redirection operator: finalize any pending word, then set redirect mode.
            # A pure-integer word immediately before the operator is an fd number (e.g. 2>).
            i = _consume_upto!(arg, st, s, i, j)
            fd = nothing
            if !word_has_special && length(arg) == 1 && isa(arg[1], AbstractString) &&
                    !isempty(arg[1]::AbstractString) && all(isdigit, arg[1]::AbstractString)
                fd = parse(Int, arg[1]::AbstractString)
                empty!(arg)
                word_has_special = false
            elseif !isempty(arg)
                _append_word!(args, arg)
            end
            if c == '>'
                append = false
                if !isempty(st) && (peek(st)::P).second == '>'
                    popfirst!(st)  # consume second >
                    append = true
                    i = something(peek(st), lastindex(s)::Int+1 => '\0').first::Int
                end
                if fd === nothing || fd == 1
                    redir.mode = :stdout; redir.stdout_append = append
                elseif fd == 2
                    redir.mode = :stderr; redir.stderr_append = append
                else
                    error("parsing command `$str`: unsupported fd $fd in redirection")
                end
            else  # '<'
                (fd === nothing || fd == 0) ||
                    error("parsing command `$str`: unsupported fd $fd in redirection")
                redir.mode = :stdin
            end
            update_last_arg = true
        else
            if update_last_arg
                last_arg = i + s.offset
                update_last_arg = false
            end
            if !in_double_quotes && c == '\''
                in_single_quotes = !in_single_quotes
                word_has_special = true
                i = _consume_upto!(arg, st, s, i, j)
            elseif !in_single_quotes && c == '"'
                in_double_quotes = !in_double_quotes
                word_has_special = true
                i = _consume_upto!(arg, st, s, i, j)
            elseif !in_single_quotes && c == '\\'
                word_has_special = true
                if !isempty(st) && (peek(st)::P)[2] in ('\n', '\r')
                    i = _consume_upto!(arg, st, s, i, j) + 1
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
                        i = _consume_upto!(arg, st, s, i, j)
                        _ = popfirst!(st)
                    end
                else
                    isempty(st) && error("dangling backslash")
                    i = _consume_upto!(arg, st, s, i, j)
                    _ = popfirst!(st)
                end
            elseif interpolate && !in_single_quotes && !in_double_quotes &&
                    c == '~' && i == j && isempty(arg)
                i = _consume_upto!(arg, st, s, i, j)
                s, i = _parse_tilde!(arg, st, s, i, P, filename)
            elseif interpolate && !in_single_quotes && !in_double_quotes && c == '{'
                i = _consume_upto!(arg, st, s, i, j)
                s, i, whs, last_arg, update_last_arg =
                    _parse_brace!(arg, st, s, i, P, str, filename, last_arg, update_last_arg)
                if whs; word_has_special = true; end
            elseif !in_single_quotes && !in_double_quotes && c in special
                error("parsing command `$str`: special characters \"$special\" must be quoted in commands")
            end
        end
    end

    if in_single_quotes; error("unterminated single quote"); end
    if in_double_quotes; error("unterminated double quote"); end

    _push_nonempty!(arg, s[i:end])
    if interpolate && redir.mode != :none
        isempty(arg) && error("parsing command `$str`: redirect operator without filename")
        re = _redirect_word_expr(arg)
        if redir.mode === :stdin; redir.stdin = re
        elseif redir.mode === :stdout; redir.stdout = re
        else; redir.stderr = re; end
        empty!(arg)
    elseif !isempty(arg) || word_has_special
        _append_word!(args, arg)
    end

    interpolate || return args, last_arg

    # If no pipeline operators and no redirects: return existing tuple format.
    if isempty(pipeline_parts) && redir.stdin === nothing && redir.stdout === nothing &&
            redir.stderr === nothing
        ex = Expr(:tuple)
        for arg in args
            push!(ex.args, Expr(:tuple, arg...))
        end
        return ex, last_arg
    end

    return _build_pipeline_expr(pipeline_parts, args, redir), last_arg
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
            if isword
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
           sizehint = sum(sizeof, args) + length(args) * 3)

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
           sizehint = (sum(sizeof, args) + 3*length(args)))
