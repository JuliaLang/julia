open("base/docs/helpdb2.jl", "w") do io
  rst = false
  sig = false
  for l in split(readall("base/docs/helpdb.jl"), "\n")
    if l == "```rst"
      rst = true
      sig = true
    elseif l == "```"
      rst = false
    elseif sig == true
      println(io, "    ", l)
      sig = false
    elseif rst
      if startswith(l, "      ")
        println(io, l[3:end])
      elseif startswith(l, "   ")
        println(io, l[4:end])
      else
        println(io, l)
      end
    else
      println(io, l)
    end
  end
end
