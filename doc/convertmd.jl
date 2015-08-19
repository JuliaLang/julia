using Lazy

function pandoc(rst)
  open("doc.rst", "w") do io
    print(io, rst)
  end
  run(`pandoc doc.rst -o doc.md -t markdown_github`)
  rm("doc.rst")
  result = open(readall, "doc.md")
  rm("doc.md")
  return result
end

function rmquote(md)
  ls = split(md, "\n")
  ls = map(ls) do l
    l in (">", ">\r") ? "" :
    startswith(l, "> ") ? l[3:end] : l
  end
  join(ls, "\n")
end

escape(md) = @> md replace("\$", "\\\$") replace("&gt;", ">") replace("&lt;", "<")

isvalid(rst) = !ismatch(r":\w+:|doctest|(^|\n)\s+\*|====|\[[\w0-9]+\]_", rst)

function convert(doc)
  if isvalid(doc)
    pandoc(doc) |> rmquote |> escape
  else
    """
    ```rst
    $(chomp(doc))
    ```
    """
  end |> chomp
end

function translate(file)
  ls = split(open(readall, file), "\n")
  doccing = false
  iscode = false
  open(file, "w") do io
    doc = IOBuffer()
    for l in ls
      if iscode
        l != "" && println(doc)
        iscode = false
      end
      doccing && l == "::" && (iscode = true)
      if l == "```rst"
        doccing = true
      elseif doccing && l == "```"
        doccing = false
        rst = takebuf_string(doc)
        println(io, convert(rst))
      elseif doccing
        println(doc, l)
      else
        println(io, l)
      end
    end
  end
end

translate("base/docs/helpdb.jl")
