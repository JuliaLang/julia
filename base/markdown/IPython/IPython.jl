type LaTeX
  formula::UTF8String
end

@trigger '$' ->
function latex(stream::IO)
  result = parse_inline_wrapper(stream, "\$", rep = true)
  return result == nothing ? nothing : LaTeX(result)
end

function blocktex(stream::IO, md::MD, config::Config)
  withstream(stream) do
    ex = latex(stream)
    if ex ≡ nothing
      return false
    else
      push!(md, ex)
      return true
    end
  end
end

writemime(io::IO, ::MIME"text/plain", tex::LaTeX) =
  print(io, '$', tex.formula, '$')

term(io::IO, tex::LaTeX, cols) = println_with_format(:magenta, io, tex.formula)
terminline(io::IO, tex::LaTeX) = print_with_format(:magenta, io, tex.formula)
