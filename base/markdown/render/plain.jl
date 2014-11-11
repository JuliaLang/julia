plain(x) = sprint(plain, x)

function plain(io::IO, content::Vector)
  for md in content[1:end-1]
    plain(io, md)
    println(io)
  end
  plain(io, content[end])
end

plain(io::IO, md::MD) = plain(io, md.content)

function plain{l}(io::IO, header::Header{l})
  print(io, "#"^l*" ")
  plaininline(io, header.text)
  println(io)
end

function plain(io::IO, code::Code)
  println(io, "```", code.language)
  println(io, code.code)
  println(io, "```")
end

function plain(io::IO, p::Paragraph)
  for md in p.content
    plaininline(io, md)
  end
  println(io)
end

function plain(io::IO, list::List)
  for item in list.items
    print(io, "  * ")
    plaininline(io, item)
    println(io)
  end
end

plain(io::IO, x) = tohtml(io, x)

# Inline elements

function plaininline(io::IO, md...)
  for el in md
    plaininline(io, el)
  end
end

plaininline(io::IO, md::Vector) = plaininline(io, md...)

plaininline(io::IO, md::Image) = print(io, "![$(md.alt)]($(md.url))")

plaininline(io::IO, s::String) = print(io, s)

plaininline(io::IO, md::Bold) = plaininline(io, "**", md.text, "**")

plaininline(io::IO, md::Italic) = plaininline(io, "*", md.text, "*")

plaininline(io::IO, md::Code) = print(io, "`", md.code, "`")

plaininline(io::IO, x) = writemime(io, MIME"text/plain"(), x)

# writemime

Base.writemime(io::IO, ::MIME"text/plain", md::MD) = plain(io, md)
