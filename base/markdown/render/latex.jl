export latex

function wrapblock(f, io, env)
  println(io, "\\begin{", env, "}")
  f()
  println(io, "\\end{", env, "}")
end

function wrapinline(f, io, cmd)
    print(io, "\\", cmd, "{")
    f()
    print(io, "}")
end

write(io::IO, ::MIME"text/latex", md::Content) =
  write(io, TEXTPLAIN, md)

function write(io::IO, mime::MIME"text/latex", block::Block)
  for md in block.content[1:end-1]
    write(io::IO, mime, md)
    println(io)
  end
  write(io::IO, mime, block.content[end])
end

function write{l}(io::IO, mime::MIME"text/latex", header::Header{l})
  tag = l < 4 ? "sub"^(l-1) * "section" : "sub"^(l-4) * "paragraph"
  wrapinline(io, tag) do
    print(io, header.text)
  end
  println(io)
end

function write(io::IO, ::MIME"text/latex", code::BlockCode)
  wrapblock(io, "verbatim") do
    println(io, code.code)
  end
end

function write(io::IO, ::MIME"text/latex", code::InlineCode)
  wrapinline(io, "texttt") do
    print(io, code.code)
  end
end

function write(io::IO, ::MIME"text/latex", md::Paragraph)
  for md in md.content
    latex_inline(io, md)
  end
  println(io)
end

const TEXTLATEX = MIME("text/latex")

function write(io::IO, ::MIME"text/latex", md::BlockQuote)
  wrapblock(io, "quote") do
    write(io, TEXTLATEX, Block(md.content))
  end
end

function write(io::IO, ::MIME"text/latex", md::List)
  wrapblock(io, "itemize") do
    for item in md.content
      print(io, "\\item ")
      latex_inline(io, item)
      println(io)
    end
  end
end

# Inline elements

function write(io::IO, ::MIME"text/latex", md::Plain)
  print(io, md.text)
end

function write(io::IO, ::MIME"text/latex", md::Bold)
  wrapinline(io, "textbf") do
    print(io, md.text)
  end
end

function write(io::IO, ::MIME"text/latex", md::Italic)
  wrapinline(io, "emph") do
    print(io, md.text)
  end
end

function write(io::IO, ::MIME"text/latex", md::Image)
  wrapblock(io, "figure") do
    println(io, "\\centering")
    wrapinline(io, "includegraphics") do
      print(io, md.url)
    end
    println(io)
    wrapinline(io, "caption") do
      print(io, md.alt)
    end
    println(io)
  end
end

function write(io::IO, ::MIME"text/latex", md::Link)
  wrapinline(io, "href") do
    print(io, md.url)
  end
  print(io, "{", md.text, "}")
end

latex_inline(io::IO, el::Content) = write(io, TEXTLATEX, el)

latex(md::Content) = stringmime(TEXTLATEX, md)
