# ––––––––
# Emphasis
# ––––––––

type Italic
  text
end

@trigger '*' ->
function asterisk_italic(stream::IO)
  result = parse_inline_wrapper(stream, "*")
  return result == nothing ? nothing : Italic(parseinline(result))
end

type Bold
  text
end

@trigger '*' ->
function asterisk_bold(stream::IO)
  result = parse_inline_wrapper(stream, "**")
  return result == nothing ? nothing : Bold(parseinline(result))
end

# ––––
# Code
# ––––

@trigger '`' ->
function inline_code(stream::IO)
  result = parse_inline_wrapper(stream, "`")
  return result == nothing ? nothing : Code(result)
end

# ––––––––––––––
# Images & Links
# ––––––––––––––

type Image
  url::UTF8String
  alt::UTF8String
end

@trigger '!' ->
function image(stream::IO)
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
  url::UTF8String
end

@trigger '[' ->
function link(stream::IO)
  withstream(stream) do
    startswith(stream, '[') || return
    text = readuntil(stream, ']', match = '[')
    text ≡ nothing && return
    skipwhitespace(stream)
    startswith(stream, '(') || return
    url = readuntil(stream, ')', match = '(')
    url ≡ nothing && return
    return Link(parseinline(text), url)
  end
end

# –––––––––––
# Punctuation
# –––––––––––

@trigger '-' ->
function en_dash(stream::IO)
  if startswith(stream, "--")
    return "–"
  end
end

const escape_chars = "\\`*_#+-.!{[("

@trigger '\\' ->
function escapes(stream::IO)
  withstream(stream) do
    if startswith(stream, "\\") && !eof(stream) && (c = read(stream, Char)) in escape_chars
      return string(c)
    end
  end
end
