# This file is a part of Julia. License is MIT: http://julialang.org/license

# ––––––––
# Emphasis
# ––––––––

type Italic
    text
end

@trigger '*' ->
function asterisk_italic(stream::IO, md::MD)
    result = parse_inline_wrapper(stream, "*")
    return result === nothing ? nothing : Italic(parseinline(result, md))
end

type Bold
    text
end

@trigger '*' ->
function asterisk_bold(stream::IO, md::MD)
    result = parse_inline_wrapper(stream, "**")
    return result === nothing ? nothing : Bold(parseinline(result, md))
end

# ––––
# Code
# ––––

@trigger '`' ->
function inline_code(stream::IO, md::MD)
    result = parse_inline_wrapper(stream, "`"; rep=true)
    return result === nothing ? nothing : Code(result)
end

@trigger '`' ->
function inline_tex(stream::IO, md::MD)
    result = parse_inline_wrapper(stream, "``"; rep=true)
    return result === nothing ? nothing : LaTeX(result)
end

# ––––––––––––––
# Images & Links
# ––––––––––––––

type Image
    url::String
    alt::String
end

@trigger '!' ->
function image(stream::IO, md::MD)
    withstream(stream) do
        startswith(stream, "![") || return
        alt = readuntil(stream, ']', match = '[')
        alt ≡ nothing && return
        skipwhitespace(stream)
        startswith(stream, '(') || return
        url = readuntil(stream, ')', match = '(')
        url ≡ nothing && return
        return Image(url, alt)
    end
end

type Link
    text
    url::String
end

@trigger '[' ->
function link(stream::IO, md::MD)
    withstream(stream) do
        startswith(stream, '[') || return
        text = readuntil(stream, ']', match = '[')
        text ≡ nothing && return
        skipwhitespace(stream)
        startswith(stream, '(') || return
        url = readuntil(stream, ')', match = '(')
        url ≡ nothing && return
        return Link(parseinline(text, md), url)
    end
end

type Footnote
    id::String
    text
end

@trigger '[' ->
function footnote(stream::IO, md::MD)
    withstream(stream) do
        startswith(stream, "[^") || return
        id = readuntil(stream, ']', match = '[')
        id ≡ nothing && return
        Footnote(id, startswith(stream, ':') ? parseinline(stream, md) : nothing)
    end
end

# –––––––––––
# Punctuation
# –––––––––––

type LineBreak end

@trigger '\\' ->
function linebreak(stream::IO, md::MD)
    if startswith(stream, "\\\n")
        return LineBreak()
    end
end

@trigger '-' ->
function en_dash(stream::IO, md::MD)
    if startswith(stream, "--")
        return "–"
    end
end

const escape_chars = "\\`*_#+-.!{}[]()\$"

@trigger '\\' ->
function escapes(stream::IO, md::MD)
    withstream(stream) do
        if startswith(stream, "\\") && !eof(stream) && (c = read(stream, Char)) in escape_chars
            return string(c)
        end
    end
end
