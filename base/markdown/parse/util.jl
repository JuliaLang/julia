import Base: peek

macro dotimes(n, body)
  quote
    for i = 1:$(esc(n))
      $(esc(body))
    end
  end
end

const whitespace = " \t\r"

"""
Skip any leading whitespace. Returns io.
"""
function skipwhitespace(io::IO; newlines = true)
  while !eof(io) && (peek(io) in whitespace || (newlines && peek(io) == '\n'))
    read(io, Char)
  end
  return io
end

"""
Skip any leading blank lines. Returns the number skipped.
"""
function skipblank(io::IO)
  start = position(io)
  i = 0
  while !eof(io)
    c = read(io, Char)
    c == '\n' && (start = position(io); i+=1; continue)
    c in whitespace || break
  end
  seek(io, start)
  return i
end

"""
Returns true if the line contains only (and
at least one of) the characters given.
"""
function linecontains(io::IO, chars; allow_whitespace = true,
                                     eat = true,
                                     allowempty = false)
  start = position(io)
  l = readline(io) |> chomp
  length(l) == 0 && return allowempty

  result = false
  for c in l
    c in whitespace && (allow_whitespace ? continue : (result = false; break))
    c in chars && (result = true; continue)
    result = false; break
  end
  !(result && eat) && seek(io, start)
  return result
end

blankline(io::IO; eat = true) =
  linecontains(io, "",
               allow_whitespace = true,
               allowempty = true,
               eat = eat)

"""
Test if the stream starts with the given string.
`eat` specifies whether to advance on success (true by default).
`padding` specifies whether leading whitespace should be ignored.
"""
function startswith(stream::IO, s::String; eat = true, padding = false, newlines = true)
  start = position(stream)
  padding && skipwhitespace(stream, newlines = newlines)
  result = true
  for char in s
    !eof(stream) && read(stream, Char) == char ||
      (result = false; break)
  end
  !(result && eat) && seek(stream, start)
  return result
end

function startswith(stream::IO, c::Char; eat = true)
  if peek(stream) == c
    eat && read(stream, Char)
    return true
  else
    return false
  end
end

function startswith{T<:String}(stream::IO, ss::Vector{T}; kws...)
  any(s->startswith(stream, s; kws...), ss)
end

function startswith(stream::IO, r::Regex; eat = true, padding = false)
  @assert beginswith(r.pattern, "^")
  start = position(stream)
  padding && skipwhitespace(stream)
  line = chomp(readline(stream))
  seek(stream, start)
  m = match(r, line)
  m == nothing && return ""
  eat && @dotimes length(m.match) read(stream, Char)
  return m.match
end

"""
Executes the block of code, and if the return value is `nothing`,
returns the stream to its initial position.
"""
function withstream(f, stream)
  pos = position(stream)
  result = f()
  (result ≡ nothing || result ≡ false) && seek(stream, pos)
  return result
end

"""
Read the stream until startswith(stream, delim)
The delimiter is consumed but not included.
Returns nothing and resets the stream if delim is
not found.
"""
function readuntil(stream::IO, delimiter; newlines = false, match = nothing)
  withstream(stream) do
    buffer = IOBuffer()
    count = 0
    while !eof(stream)
      if startswith(stream, delimiter)
        if count == 0
          return takebuf_string(buffer)
        else
          count -= 1
          write(buffer, delimiter)
        end
      end
      char = read(stream, Char)
      char == match && (count += 1)
      !newlines && char == '\n' && break
      write(buffer, char)
    end
  end
end

"""
Parse a symmetrical delimiter which wraps words.
i.e. `*word word*` but not `*word * word`.
`repeat` specifies whether the delimiter can be repeated.
Escaped delimiters are not yet supported.
"""
function parse_inline_wrapper(stream::IO, delimiter::String; rep = false)
  withstream(stream) do
    startswith(stream, delimiter) || return nothing
    n = 1
    while rep && startswith(stream, delimiter); (n += 1) end

    buffer = IOBuffer()
    while !eof(stream)
      char = read(stream, Char)
      if !(char in whitespace || char == '\n') && startswith(stream, delimiter^n)
        write(buffer, char)
        return takebuf_string(buffer)
      end
      write(buffer, char)
    end
  end
end

function showrest(io::IO)
  start = position(io)
  show(readall(io))
  println()
  seek(io, start)
end
