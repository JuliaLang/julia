@breaking true ->
function fencedcode(stream::IO, block::MD, config::Config)
  startswith(stream, "```", padding = true) || return false
  readline(stream)
  buffer = IOBuffer()
  while !eof(stream)
    startswith(stream, "```") && break
    write(buffer, readline(stream))
  end
  push!(block, Code(takebuf_string(buffer) |> chomp))
  return true
end

function github_paragraph(stream::IO, md::MD, config::Config)
  skipwhitespace(stream)
  buffer = IOBuffer()
  p = Paragraph()
  push!(md, p)
  while !eof(stream)
    char = read(stream, Char)
    if char == '\n'
      eof(stream) && break
      if blankline(stream) || parse(stream, md, config, breaking = true)
        break
      else
        write(buffer, '\n')
      end
    else
      write(buffer, char)
    end
  end
  p.content = parseinline(seek(buffer, 0), config)
  return true
end

# TODO: tables

@flavor github [list, indentcode, blockquote, fencedcode, hashheader, github_paragraph,
                 en_dash, inline_code, asterisk_bold, asterisk_italic, image, link]
