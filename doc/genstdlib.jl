using .Markdown

cd(dirname(@__FILE__))

isrst(md) =
  length(md.content) == 1 &&
  isa(md.content[1], Markdown.Code) &&
  md.content[1].language == "rst"

rst(md) = isrst(md) ? join(split(md.content[1].code, "\n")[3:end], "\n") : Markdown.rst(md)

isop(func) = ismatch(r"[^\w@!.]|^!$", func)

ident(mod, x) = "$mod.$(isop(x) ? "(:($x))" : x)"

getdoc(mod, x) = try eval(parse("@doc $(ident(mod, x))")) catch e end

function translate(file)
  @assert isfile(file)
  ls = split(readall(file), "\n")[1:end-1]
  doccing = false
  func = nothing
  mod = "Base"

  open(file, "w+") do io
    for l in ls
      if ismatch(r".. (current)?module::", l)
        mod = match(r".. (current)?module:: ([\w\.]+)", l).captures[2]
        println(io, l)
      elseif startswith(l, ".. function::")
        func = match(r".. function:: (@?[^\(\s\{]+)", l)
        func == nothing && (warn("bad function $l"); continue)
        func = func.captures[1]

        if getdoc(mod, func) == nothing
          info("no docs for $(ident(mod, func))")
          println(io, l)
          doccing = false
          continue
        end

        doccing = true
        println(io, l)
        println(io)
        println(io, rst(getdoc(mod, func)))
        println(io)
      elseif doccing && (startswith(l, "   ") || ismatch(r"^\s*$", l))
      else
        doccing = false
        println(io, l)
      end
    end
  end
end

println("\nConveting stdlib/\n")

for file in readdir("stdlib")
  translate("stdlib/$file")
end

println("\nConverting devdocs/\n")

for file in readdir("devdocs")
  translate("devdocs/$file")
end
