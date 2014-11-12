# ––––––––––
# Paragraphs
# ––––––––––

type Paragraph
  content
end

Paragraph() = Paragraph([])

function paragraph(stream::IO, md::MD, config::Config)
  buffer = IOBuffer()
  p = Paragraph()
  push!(md, p)
  skipwhitespace(stream)
  while !eof(stream)
    char = read(stream, Char)
    if char == '\n' || char == '\r'
      if blankline(stream) || parse(stream, md, config, breaking = true)
        break
      else
        write(buffer, ' ')
      end
    else
      write(buffer, char)
    end
  end
  p.content = parseinline(seek(buffer, 0), config)
  return true
end

# –––––––
# Headers
# –––––––

type Header{level}
  text
end

Header(s, level::Int) = Header{level}(s)
Header(s) = Header(s, 1)

@breaking true ->
function hashheader(stream::IO, md::MD, config::Config)
  startswith(stream, "#") || return false
  level = 1
  while startswith(stream, "#")
    level += 1
  end
  h = readline(stream) |> chomp
  h = match(r"\s*(.*)(?<![#\s])", h).captures[1]
  buffer = IOBuffer()
  print(buffer, h)
  if !isempty(h)
    push!(md.content, Header(parseinline(seek(buffer, 0), config), level))
    return true
  else
    return false
  end
end

# ––––
# Code
# ––––

type Code
  language::UTF8String
  code::UTF8String
end

Code(code) = Code("", code)

function indentcode(stream::IO, block::MD, config::Config)
  withstream(stream) do
    buffer = IOBuffer()
    while startswith(stream, "    ") || startswith(stream, "\t")
      write(buffer, readline(stream))
    end
    code = takebuf_string(buffer)
    !isempty(code) && (push!(block, Code(chomp(code))); return true)
    return false
  end
end

# ––––––
# Quotes
# ––––––

type BlockQuote
  content
end

BlockQuote() = BlockQuote([])

# TODO: Laziness
@breaking true ->
function blockquote(stream::IO, block::MD, config::Config)
  withstream(stream) do
    buffer = IOBuffer()
    while startswith(stream, ">")
      startswith(stream, " ")
      write(buffer, readline(stream))
    end
    md = takebuf_string(buffer)
    if !isempty(md)
      push!(block, BlockQuote(parse(md, flavor = config).content))
      return true
    else
      return false
    end
  end
end

# –––––
# Lists
# –––––

type List
  items::Vector{Any}
  ordered::Bool

  List(x::AbstractVector) = new(x)
end

List(xs...) = List([xs...])

const bullets = ["* ", "• ", "+ ", "- "]

# Todo: ordered lists, inline formatting
function list(stream::IO, block::MD, config::Config)
  withstream(stream) do
    skipwhitespace(stream)
    startswith(stream, bullets) || return false
    the_list = List()
    buffer = IOBuffer()
    fresh_line = false
    while !eof(stream)
      if fresh_line
        skipwhitespace(stream)
        if startswith(stream, bullets)
          push!(the_list.items, parseinline(takebuf_string(buffer), config))
          buffer = IOBuffer()
        else
          write(buffer, ' ')
        end
        fresh_line = false
      else
        c = read(stream, Char)
        if c == '\n'
          eof(stream) && break
          next = peek(stream)
          if next == '\n'
            break
          else
            fresh_line = true
          end
        else
          write(buffer, c)
        end
      end
    end
    push!(the_list.items, parseinline(takebuf_string(buffer), config))
    push!(block, the_list)
    return true
  end
end
